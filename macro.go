package main

import (
    "fmt"
    "math/rand"
    "reflect"
)

const ellipsis = "..."
const underscore = "_"

// For now macros are always globally defined, and even shared between runtimes(!)
var macromap = map[string]transformer{
    "cond": transformer_cond,
    "let" : transformer_let,
    "and" : syntaxRules(parse(`(syntax-rules ()
                                 ((_) #t)
                                 ((_ e) e)
                                 ((_ e1 e2 e3 ...) (if e1 (and e2 e3 ...) #f))
                                 )`).AsPair()),
    "list": transformer_list,
    // kanren macros, to be defined instead
    "conde" : transformer_conde,
    "fresh" : transformer_fresh,
}

type transformer = func(Pair) SExpression

func expandMacro(p Pair) (SExpression, bool) {
	if !p.car().IsSymbol() {
		return p, false
	}
	s := p.car().AsSymbol()
    tf, ok := macromap[s]
    if !ok {
        return p, false
    }
    return tf(p), true
}

type clause struct {
    pattern pattern
    template pattern
}

type pattern struct {
    isVariable   bool
    isUnderscore bool
    isLiteral    bool
    isConstant   bool
    isList       bool
    hasEllipsis  bool
    content      SExpression
    listContent  []pattern
}

func gensym() Symbol {
    return Symbol("gensym" + fmt.Sprint(rand.Intn(9999999999)))
}

// build=true analyses pattern and builds up a gensym lookup table
// build=false analyses template and substitutes pattern vars with their gensymmed counterparts
func analyse(literals []string, p SExpression, gensyms map[Symbol]Symbol, build bool) pattern {
    if p.IsSymbol() {
        sym := p.AsSymbol()
        if sym == underscore {
            return pattern{isUnderscore: true}
        }
        if build {
            newsym := gensym()
            gensyms[sym] = newsym
            return pattern{isVariable: true, content: NewSymbol(newsym)}
        }
        newsym, ok := gensyms[sym]
        if !ok {
            return pattern{isVariable: true, content: p}
        }
        return pattern{isVariable: true, content: NewSymbol(newsym)}
    }
    if p.IsAtom() {
        return pattern{isConstant: true, content: p}
    }
    listContent := []pattern{}
    list := cons2list(p.AsPair())
    for i:=0; i<len(list); i++ {
        pi := analyse(literals, list[i], gensyms, build)
        if i != len(list)-1 {
            sexprj := list[i+1]
            if sexprj.IsSymbol() && sexprj.AsSymbol() == ellipsis {
                pi.hasEllipsis = true
                i += 1
            }
        }
        listContent = append(listContent, pi)
    }
    return pattern{isList: true, listContent: listContent}
}

func analysePattern(literals []string, p SExpression, gensyms map[Symbol]Symbol) pattern {
    return analyse(literals, p, gensyms, true)
}

// TODO: perhaps this should take as part of gensyms which have ellipsis,
// so we can validate pattern and template ellipsis use matches
func analyseTemplate(t SExpression, gensyms map[Symbol]Symbol) pattern {
    return analyse(nil, t, gensyms, false)
}

// assuming for now every define-syntax is followed by syntax-rules
func syntaxRules(sr Pair) transformer {
    if sr.car().AsSymbol() != "syntax-rules" {
        panic("expected syntax-rules")
    }
    // TODO: ignoring literals for now
    literals := []string{} //sr.cadr()
    clauses := []clause{}
    for _, c := range cons2list(sr.cddr().AsPair()) {
        cp := c.AsPair()
        s := map[Symbol]Symbol{}
        p := analysePattern(literals, cp.car(), s)
        t := analyseTemplate(cp.cadr(), s)
        clauses = append(clauses, clause{pattern:p, template:t})
    }
    return func(p Pair) SExpression {
        for _, c := range clauses {
            substitutions := map[Symbol]SExpression{}
            if !unify(c.pattern, p, substitutions) {
                continue
            }
            return substituteTemplate(c.template, substitutions)
        }
		panic(fmt.Sprintf("invalid syntax %s", p))
    }
}

// matching pattern to input, returning substitutions needed for valid unification if any
// TODO: for now, all symbols are pattern variables
func unify(p pattern, q SExpression, s map[Symbol]SExpression) bool {
    return unifyWithEllipsis(p, q, s, []int{})
}

func unifyWithEllipsis(p pattern, q SExpression, s map[Symbol]SExpression, depth []int) bool {
    if p.isUnderscore {
        return true
    }
    if p.isConstant {
        return reflect.DeepEqual(p.content, q)
    }
    if p.isVariable {
        ps := p.content.AsSymbol()
        for i := 0; i < len(depth); i++ {
            ps += fmt.Sprintf("#%d", depth[i])
        }
        // TODO: not even sure if repeated pattern vars are allowed
        prev, ok := s[ps]
        if !ok {
            s[ps] = q
            return true
        }
        return reflect.DeepEqual(prev, q)
    }
    if !p.isList {
        panic("pattern assumed to be list")
    }
    if !q.IsPair() {
        return false
    }
    qp := q.AsPair()
Loop:
    for _, pp := range p.listContent {
        if !pp.hasEllipsis {
            if qp == empty {
                return false
            }
            if !unifyWithEllipsis(pp, qp.car(), s, depth) {
                return false
            }
            qp = qp.cdr().AsPair()
            continue Loop
        }
        newdepth := make([]int, len(depth))
        copy(newdepth, depth)
        newdepth = append(newdepth, 0)
        for {
            if qp == empty {
               continue Loop
            }   
            ok := unifyWithEllipsis(pp, qp.car(), s, newdepth)
            // TODO: scary bug waiting to happen where s contains partial substitution
            // but the full substitution has failed.. needs map copying to fix?
            if !ok {
               continue Loop
            }
            newdepth[len(newdepth)-1] = newdepth[len(newdepth)-1] + 1
            qp = qp.cdr().AsPair()
        }
    }
    return qp == empty
}

func substituteTemplate(template pattern, substitutions map[Symbol]SExpression) SExpression {
    sexpr, _ := substituteTemplateWithEllipsis(template, substitutions, []int{})
    return sexpr
}

func substituteTemplateWithEllipsis(template pattern, substitutions map[Symbol]SExpression, depth []int) (SExpression, bool) {
    if template.isConstant {
        return template.content, true
    }
    if template.isVariable {
        ss := template.content.AsSymbol()
        for i := 0; i < len(depth); i++ {
            ss += fmt.Sprintf("#%d", depth[i])
        }
        s, ok := substitutions[ss]
        if ok {
            return s, true
        }
        return template.content, false
    }
    if !template.isList {
        panic("template assumed to be list")
    }
    out := []SExpression{}
    found := true
Loop:
    for _, v := range template.listContent {
        if !v.hasEllipsis {
            sexpr, ok := substituteTemplateWithEllipsis(v, substitutions, depth)
            found = found && ok
            out = append(out, sexpr)
            continue
        }
        // attempt to substitute using depth until failure
        // TODO: all pattern vars must be matched in ellipsis right now!
        newdepth := make([]int, len(depth))
        copy(newdepth, depth)
        newdepth = append(newdepth, 0)
        for {
            sexpr, ok := substituteTemplateWithEllipsis(v, substitutions, newdepth)
            if !ok {
                continue Loop
            }
            out = append(out, sexpr)
            newdepth[len(newdepth)-1] = newdepth[len(newdepth)-1] + 1
        }
    }
    return list2cons(out...), found
}

func transformer_cond(p Pair) SExpression {
	var expanded SExpression
	expanded = NewPrimitive(false)
	clauses := cons2list(p.cdr().AsPair())
	for i := len(clauses) - 1; i >= 0; i-- {
		clause := clauses[i].AsPair()
		cond := clause.car()
		if cond.IsAtom() {
			if cond.AsSymbol() != "else" {
				panic("expected else")
			}
			if i != len(clauses)-1 {
				panic("else is not last in cond")
			}
			expanded = clause.cadr()
			continue
		}
		begin := []SExpression{NewSymbol("begin")}
		clause = clause.cdr().AsPair()
		for clause != empty {
			begin = append(begin, clause.car())
			clause = clause.cdr().AsPair()
		}
		expanded = list2cons(
			NewSymbol("if"),
			cond,
			list2cons(begin...),
			expanded,
		)
	}
	return expanded.AsPair()
}

func transformer_let(p Pair) SExpression {
	bindings := cons2list(p.cadr().AsPair())
	body := p.caddr()
	vars := make([]SExpression, len(bindings))
	exps := make([]SExpression, len(bindings))
	for i, b := range bindings {
		bl := b.AsPair() // list of len 2
		vars[i] = bl.car()
		exps[i] = bl.cadr()
	}
	lambda := list2cons(
		NewSymbol("lambda"),
		list2cons(vars...),
		body,
	)
	return NewPair(lambda, list2cons(exps...))
}

func transformer_list(p Pair) SExpression {
	clauses := p.cdr().AsPair()
	if clauses == empty {
		return list2cons(NewSymbol("quote"), empty)
	}
	return list2cons(
		NewSymbol("cons"),
		clauses.car(),
		NewPair(NewSymbol("list"), clauses.cdr()),
	)
}

func transformer_conde(p Pair) SExpression {
	clauses := p.cdr().AsPair()
	list := []SExpression{NewSymbol("disj+")}
	for clauses != empty {
		clauses.car().AsPair() //require
		list = append(list, NewPair(
			NewSymbol("conj+"),
			clauses.car(),
		))
		clauses = clauses.cdr().AsPair()
	}
	return list2cons(list...)
}

func transformer_fresh(p Pair) SExpression {
	vars := p.cadr().AsPair()
	goals := p.cddr()
	if vars == empty {
		return NewPair(
			NewSymbol("conj+"),
			goals,
		)
	}
	x0, xlist := vars.car(), vars.cdr()
	freshrec := NewPair(
		NewSymbol("fresh"),
		NewPair(xlist, goals),
	)
	lambda := list2cons(
		NewSymbol("lambda"),
		list2cons(x0),
		freshrec,
	)
	return list2cons(
		NewSymbol("call/fresh"),
		lambda,
	)
}
