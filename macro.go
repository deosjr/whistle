package main

import (
    "fmt"
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
                                 ((_ e1 e2 ...) (if e1 (and e2 ...) #f))
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

// TODO:
// - replace variables with gensyms in both pattern and template
// x unify pattern with input pair ('unify')
// x unify resulting substitution with template ('substitute')
// x take into account ellipsis when unifying
// - take into account literals when unifying

// assuming for now every define-syntax is followed by syntax-rules
func syntaxRules(sr Pair) transformer {
    if sr.car().AsSymbol() != "syntax-rules" {
        panic("expected syntax-rules")
    }
    // TODO: ignoring literals for now
    // literals := sr.cadr()
    clauses := cons2list(sr.cddr().AsPair())
    return func(p Pair) SExpression {
        for _, c := range clauses {
            clause := c.AsPair()
            pattern, template := clause.car().AsPair(), clause.cadr()
            match, ok := matchClause(p, pattern, template)
            if !ok {
                continue
            }
            return match
        }
		panic(fmt.Sprintf("invalid syntax %s", p))
    }
}

type syntaxSub struct {
    sexpr SExpression
    hasEllipsis bool
    ellipsis Pair
}

// matching pattern to input, returning substitutions needed for valid unification if any
// TODO: for now, all symbols are pattern variables
func unify(p, q SExpression, s map[string]syntaxSub) bool {
    switch {
    case p.IsSymbol():
        ps := p.AsSymbol()
        if ps == underscore {
            return true
        }
        prev, ok := s[ps]
        if !ok {
            s[ps] = syntaxSub{sexpr:q}
            return true
        }
        return reflect.DeepEqual(prev.sexpr, q)
    case p.IsPair():
        if !q.IsPair() {
            return false
        }
        pp := p.AsPair()
        qq := q.AsPair()
        if pp == empty || qq == empty {
            return pp == empty && qq == empty
        }
        if !unify(pp.car(), qq.car(), s) {
            return false
        }
        // test for ellipsis
        if pp.cdr() != empty {
            if pp.cadr().IsSymbol() && pp.cadr().AsSymbol() == ellipsis && pp.cddr() == empty {
                if !pp.car().IsSymbol() {
                    return false
                }
                ps := pp.car().AsSymbol()
                prevsub, ok := s[ps]
                if !ok {
                    return false
                }
                prevsub.hasEllipsis = true
                prevsub.ellipsis = qq.cdr().AsPair()
                s[ps] = prevsub
                return true
            }
        }
        if !unify(pp.cdr(), qq.cdr(), s) {
            return false
        }
        return true
    }
    return false
}

func matchClause(pair, pattern Pair, template SExpression) (SExpression, bool) {
    pairList := cons2list(pair)
    patternList := cons2list(pattern)
    lastInPattern := patternList[len(patternList)-1]
    if lastInPattern.IsSymbol() && lastInPattern.AsSymbol() == ellipsis {
        n := len(patternList)-1
        pairList = append(pairList[:n], list2cons(pairList[n:]...))
    }
    if len(pairList) != len(patternList) {
        return nil, false
    }
    substitutions := map[string]syntaxSub{}
    if !unify(pattern, pair, substitutions) {
        return nil, false
    }
    return substituteTemplate(substitutions, template), true
}

func substituteTemplate(substitutions map[string]syntaxSub, template SExpression) SExpression {
    if template.IsAtom() {
        if template.IsSymbol() {
            s, ok := substitutions[template.AsSymbol()]
            if ok {
                return s.sexpr
            }
        }
        return template
    }
    tList := cons2list(template.AsPair())
    out := []SExpression{}
    for _, t := range tList {
        if !t.IsSymbol() {
            out = append(out, substituteTemplate(substitutions, t))
            continue
        }
        if t.AsSymbol() == ellipsis {
            continue
        }
        s, ok := substitutions[t.AsSymbol()]
        if ok {
            out = append(out, s.sexpr)
            if s.hasEllipsis {
                out = append(out, cons2list(s.ellipsis)...)
            }
            continue
        }
        out = append(out, t)
    }
    return list2cons(out...)
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
