package main

// TODO: obviously its not macros rn, its just hardcoded language features
// They cannot be defined from repl yet!
func expandMacro(p Pair) SExpression {
	if !p.car().IsSymbol() {
		return p
	}
	s := p.car().AsSymbol()
	switch s {
	case "cond":
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
		return expandMacro(expanded.AsPair())
	case "let":
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
		return expandMacro(NewPair(lambda, list2cons(exps...)))
	case "and":
		clauses := p.cdr().AsPair()
		if clauses == empty {
			return NewSymbol("#t")
		}
		if clauses.cdr().AsPair() == empty {
			return clauses.car()
		}
		return expandMacro(list2cons(
			NewSymbol("if"),
			clauses.car(),
			NewPair(NewSymbol("and"), clauses.cdr()),
			NewSymbol("#f"),
		))
	case "list":
		clauses := p.cdr().AsPair()
		if clauses == empty {
			return list2cons(NewSymbol("quote"), empty)
		}
		return list2cons(
			NewSymbol("cons"),
			clauses.car(),
			NewPair(NewSymbol("list"), clauses.cdr()),
		)
	// kanren macros
	case "zzz":
		goal := p.cadr()
		sc := NewSymbol("s/c")
		lambda := list2cons(
			NewSymbol("lambda"),
			empty,
			list2cons(goal, sc),
		)
		return expandMacro(list2cons(
			NewSymbol("lambda"),
			list2cons(sc),
			lambda,
		))
	case "conj+":
		g0 := list2cons(NewSymbol("zzz"), p.cadr())
		g := p.cddr()
		if g.AsPair() == empty {
			return expandMacro(g0)
		}
		return expandMacro(list2cons(
			NewSymbol("conj"),
			g0,
			NewPair(NewSymbol("conj+"), g),
		))
	case "disj+":
		g0 := list2cons(NewSymbol("zzz"), p.cadr())
		g := p.cddr()
		if g.AsPair() == empty {
			return expandMacro(g0)
		}
		return expandMacro(list2cons(
			NewSymbol("disj"),
			g0,
			NewPair(NewSymbol("disj+"), g),
		))
	case "conde":
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
		return expandMacro(list2cons(list...))
	case "fresh":
		vars := p.cadr().AsPair()
		goals := p.cddr()
		if vars == empty {
			return expandMacro(NewPair(
				NewSymbol("conj+"),
				goals,
			))
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
		return expandMacro(list2cons(
			NewSymbol("call/fresh"),
			lambda,
		))
	default:
		return p
	}
}
