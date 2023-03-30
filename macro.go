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
    // TODO: needs implementation of literals in macro rules
    "cond": transformer_cond,
    // TODO: needs a 'begin' in lambda because my lambda implementation only takes 1 body
    "let" : syntaxRules(parse(`(syntax-rules ()
                                 ((_ ((var exp) ...) body1 body2 ...)
                                   ((lambda (var ...) (begin body1 body2 ...)) exp ...)))`).AsPair()),
    "and" : syntaxRules(parse(`(syntax-rules ()
                                 ((_) #t)
                                 ((_ e) e)
                                 ((_ e1 e2 e3 ...) (if e1 (and e2 e3 ...) #f)))`).AsPair()),
    "list": syntaxRules(parse(`(syntax-rules ()
                                 ((_) (quote ()))
                                 ((_ a b ...) (cons a (list b ...))))`).AsPair()),
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
        e := map[Symbol]int{}
        p := analysePattern(literals, cp.car(), s, e)
        t := analyseTemplate(cp.cadr(), s, e)
        clauses = append(clauses, clause{pattern:p, template:t, ellipsis:e})
    }
    return func(p Pair) SExpression {
        for _, c := range clauses {
            substitutions := map[Symbol]SExpression{}
            if !unify(c.pattern, p, substitutions) {
                continue
            }
            return substituteTemplate(c.template, substitutions, c.ellipsis)
        }
		panic(fmt.Sprintf("invalid syntax %s", p))
    }
}

type clause struct {
    pattern pattern
    template pattern
    ellipsis map[Symbol]int
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

func analysePattern(literals []string, p SExpression, gensyms map[Symbol]Symbol, ellipsis map[Symbol]int) pattern {
    pattern := analyse(literals, p, gensyms, true)
    analyseEllipsis(pattern, ellipsis, 0)
    return pattern
}

func analyseTemplate(t SExpression, gensyms map[Symbol]Symbol, ellipsis map[Symbol]int) pattern {
    pattern := analyse(nil, t, gensyms, false)
    if !verifyEllipsis(pattern, ellipsis, 0) {
        panic("nonmatching ellipsis")
    }
    return pattern
}

// which symbols are found at which depth in ellipsis
func analyseEllipsis(p pattern, e map[Symbol]int, depth int) {
    if p.isVariable {
        if depth == 0 && !p.hasEllipsis {
            return
        }
        ps := p.content.AsSymbol()
        if p.hasEllipsis {
            depth++
        }
        e[ps] = depth
        return
    }
    if !p.isList {
        return
    }
    newdepth := depth
    if p.hasEllipsis {
        newdepth++
    }
    for _, pp := range p.listContent {
        analyseEllipsis(pp, e, newdepth)
    }
}

// verifying ellipsis vars
func verifyEllipsis(p pattern, e map[Symbol]int, depth int) bool {
    if p.isVariable {
        ps := p.content.AsSymbol()
        d, ok := e[ps]
        if !ok {
            return true
        }
        if p.hasEllipsis {
            depth++
        }
        return d == depth
    }
    if !p.isList {
        return true
    }
    newdepth := depth
    if p.hasEllipsis {
        newdepth++
    }
    for _, pp := range p.listContent {
        if ok := verifyEllipsis(pp, e, newdepth); !ok {
            return false
        }
    }
    return true
}

// matching pattern to input, returning substitutions needed for valid unification if any
// TODO: for now, all symbols are pattern variables
// NOTE: this has become less unification since duplicate pattern vars are not allowed, rename?
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
        if _, ok := s[ps]; ok {
            panic("duplicate pattern var")
        }
        s[ps] = q
        return true
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

func substituteTemplate(template pattern, substitutions map[Symbol]SExpression, ellipsis map[Symbol]int) SExpression {
    sexpr, _ := substituteTemplateWithEllipsis(template, substitutions, ellipsis, []int{})
    return sexpr
}

func substituteTemplateWithEllipsis(template pattern, substitutions map[Symbol]SExpression, e map[Symbol]int, depth []int) (SExpression, bool) {
    if template.isConstant {
        return template.content, false
    }
    if template.isVariable {
        ss := template.content.AsSymbol()
        _, isEllipsis := e[ss]
        for i := 0; i < len(depth); i++ {
            ss += fmt.Sprintf("#%d", depth[i])
        }
        s, ok := substitutions[ss]
        if ok {
            if isEllipsis && len(depth) == 0 {
                panic("impossible")
            }
            // the found variable is an ellipsis var and we have found a substitution for it at this repeat
            // OR the found variable is a toplevel pattern var without ellipsis
            return s, isEllipsis
        }
        // the found variable is an ellipsis var, but we fail to find a repeat match
        // OR the found variable is a pattern var without subsitutions and no ellipsis
        return template.content, false
    }
    if !template.isList {
        panic("template assumed to be list")
    }
    out := []SExpression{}
    found := false
    for _, v := range template.listContent {
        if !v.hasEllipsis {
            sexpr, ok := substituteTemplateWithEllipsis(v, substitutions, e, depth)
            found = found || ok
            out = append(out, sexpr)
            continue
        }
        // attempt to substitute using depth until failure
        // we stop when recursion does not match a single ellipsis variable
        newdepth := make([]int, len(depth))
        copy(newdepth, depth)
        newdepth = append(newdepth, 0)
        i := 0
        for {
            sexpr, ok := substituteTemplateWithEllipsis(v, substitutions, e, newdepth)
            if !ok {
                break
            }
            out = append(out, sexpr)
            newdepth[len(newdepth)-1] = newdepth[len(newdepth)-1] + 1
            i++
        }
        found = found || (i != 0)
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
