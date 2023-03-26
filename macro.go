package main

import "fmt"

const ellipsis = "..."

// For now macros are always globally defined, and even shared between runtimes(!)
var macromap = map[string]transformer{
    "cond": transformer_cond,
    "let" : transformer_let,
    "and" : transformer_and,
    "list": transformer_list,
    // kanren macros, to be defined instead
    "zzz" : transformer_zzz,
    "conj+" : transformer_conj,
    "disj+" : transformer_disj,
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

func matchClause(pair, pattern Pair, template SExpression) (SExpression, bool) {
    pairList := cons2list(pair)
    patternList := cons2list(pattern)
    lastInPattern := patternList[len(patternList)-1]
    if lastInPattern.AsSymbol() == ellipsis {
        n := len(patternList)-1
        pairList = append(pairList[:n], list2cons(pairList[n:]...))
    }
    if len(pairList) != len(patternList) {
        return nil, false
    }
    substitutions := map[string]SExpression{}
    for i:=1; i < len(patternList); i++ {
        substitutions[patternList[i].AsSymbol()] = pairList[i]
    }
    return substituteTemplate(substitutions, template), true
}

func substituteTemplate(substitutions map[string]SExpression, template SExpression) SExpression {
    if template.IsAtom() {
        if template.IsSymbol() {
            s, ok := substitutions[template.AsSymbol()]
            if ok {
                return s
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
        s, ok := substitutions[t.AsSymbol()]
        if ok {
            if t.AsSymbol() == ellipsis {
                out = append(out, cons2list(s.AsPair())...)
            } else {
                out = append(out, s)
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

func transformer_and(p Pair) SExpression {
	clauses := p.cdr().AsPair()
	if clauses == empty {
		return NewSymbol("#t")
	}
	if clauses.cdr().AsPair() == empty {
		return clauses.car()
	}
	return list2cons(
		NewSymbol("if"),
		clauses.car(),
		NewPair(NewSymbol("and"), clauses.cdr()),
		NewSymbol("#f"),
	)
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

func transformer_zzz(p Pair) SExpression {
	goal := p.cadr()
	sc := NewSymbol("s/c")
	lambda := list2cons(
		NewSymbol("lambda"),
		empty,
		list2cons(goal, sc),
	)
	return list2cons(
		NewSymbol("lambda"),
		list2cons(sc),
		lambda,
	)
}

func transformer_conj(p Pair) SExpression {
	g0 := list2cons(NewSymbol("zzz"), p.cadr())
	g := p.cddr()
	if g.AsPair() == empty {
		return g0
	}
	return list2cons(
		NewSymbol("conj"),
		g0,
		NewPair(NewSymbol("conj+"), g),
	)
}

func transformer_disj(p Pair) SExpression {
	g0 := list2cons(NewSymbol("zzz"), p.cadr())
	g := p.cddr()
	if g.AsPair() == empty {
		return g0
	}
	return list2cons(
		NewSymbol("disj"),
		g0,
		NewPair(NewSymbol("disj+"), g),
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
