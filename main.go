package main

// http://norvig.com/lispy.html

import (
	"fmt"
	"math"
	"reflect"
	"strconv"
	"strings"
)

func main() {
	env := GlobalEnv()
	for _, s := range []string{
		"(list 1 2 3)",
		"(list (list (quote point) 1 2))",
	} {
		t := parse(s)
		fmt.Println("> ", t)
		e := evalEnv(t, env)
		s := e.String()
		if s != "" {
			fmt.Println(s)
		}
	}
}

func parse(program string) ExpOrProc {
	s := tokenize(program)
	p, _ := readFromTokens(s)
	return p
}

func tokenize(s string) []string {
	s = strings.ReplaceAll(s, "[", "(")
	s = strings.ReplaceAll(s, "]", ")")
	s = strings.ReplaceAll(s, "(", " ( ")
	s = strings.ReplaceAll(s, ")", " ) ")
	return strings.Fields(s)
}

func readFromTokens(tokens []string) (ExpOrProc, []string) {
	if len(tokens) == 0 {
		panic("syntax error")
	}
	token := tokens[0]
	tokens = tokens[1:]
	switch token {
	case "(":
		list := []ExpOrProc{}
		for tokens[0] != ")" {
			parsed, t := readFromTokens(tokens)
			tokens = t
			list = append(list, parsed)
		}
		conslist := list2cons(list...)
		return ExpOrProc{isExp: true, value: Exp{isPair: true, value: conslist}}, tokens[1:]
	case ")":
		panic("unexpected ')'")
	default:
		return ExpOrProc{isExp: true, value: Exp{value: atom(token)}}, tokens
	}
}

func atom(token string) Atom {
	if n, err := strconv.ParseFloat(token, 64); err == nil {
		return Atom{value: n}
	}
	return Atom{isSymbol: true, value: token}
}

func GlobalEnv() Env {
	return Env{dict: map[Symbol]ExpOrProc{
		"*": builtinFunc(func(args []ExpOrProc) ExpOrProc {
			return atomWithValue(number(args[0]) * number(args[1]))
		}),
		"+": builtinFunc(func(args []ExpOrProc) ExpOrProc {
			return atomWithValue(number(args[0]) + number(args[1]))
		}),
		"-": builtinFunc(func(args []ExpOrProc) ExpOrProc {
			return atomWithValue(number(args[0]) - number(args[1]))
		}),
		"=": builtinFunc(func(args []ExpOrProc) ExpOrProc {
			return atomWithValue(number(args[0]) == number(args[1]))
		}),
		">": builtinFunc(func(args []ExpOrProc) ExpOrProc {
			return atomWithValue(number(args[0]) > number(args[1]))
		}),
		"<=": builtinFunc(func(args []ExpOrProc) ExpOrProc {
			return atomWithValue(number(args[0]) <= number(args[1]))
		}),
		"#t":    atomWithValue(true),
		"#f":    atomWithValue(false),
		"pi":    atomWithValue(math.Pi),
		"number?": builtinFunc(func(args []ExpOrProc) ExpOrProc {
			x := args[0]
			if !x.isExp {
				return atomWithValue(false)
			}
			e := x.exp()
			if e.isPair {
				return atomWithValue(false)
			}
			a := e.atom()
			return atomWithValue(!a.isSymbol)
        }),
		"pair?": builtinFunc(func(args []ExpOrProc) ExpOrProc {
			x := args[0]
			if !x.isExp {
				return atomWithValue(false)
			}
			e := x.exp()
			return atomWithValue(e.isPair && e.pair() != empty)
        }),
		"car": builtinFunc(func(args []ExpOrProc) ExpOrProc {
			return args[0].exp().pair().car()
        }),
		"cdr": builtinFunc(func(args []ExpOrProc) ExpOrProc {
			return args[0].exp().pair().cdr()
        }),
		"cons": builtinFunc(func(args []ExpOrProc) ExpOrProc {
			return pairExpression(newPair(args[0], args[1]))
        }),
		"null?": builtinFunc(func(args []ExpOrProc) ExpOrProc {
			x := args[0]
			if !x.isExp {
				return atomWithValue(false)
			}
			e := x.exp()
			if !e.isPair {
				return atomWithValue(false)
			}
			return atomWithValue(e.pair() == empty)
        }),
		"procedure?": builtinFunc(func(args []ExpOrProc) ExpOrProc {
			return atomWithValue(!args[0].isExp)
        }),
		"eqv?": builtinFunc(func(args []ExpOrProc) ExpOrProc {
			return atomWithValue(reflect.DeepEqual(args[0], args[1]))
        }),
		"display": builtinFunc(func(args []ExpOrProc) ExpOrProc {
			fmt.Println(args[0])
			return atomWithValue(true)
        }),
	}, outer: nil}
}

func newEnv(params Pair, args []ExpOrProc, outer Env) Env {
	m := map[Symbol]ExpOrProc{}
	i := 0
	for params != empty {
		m[params.car().exp().atom().symbol()] = args[i]
		params = params.cdr().exp().pair()
		i++
	}
	return Env{dict: m, outer: &outer}
}

// TODO: obviously its not macros rn, its just hardcoded language features
// They cannot be defined from repl yet!
func expandMacro(p Pair) ExpOrProc {
	if !isAtom(p.car()) {
		return pairExpression(p)
	}
	s := p.car().exp().atom().symbol()
	switch s {
	case "cond":
		expanded := atomWithValue(false)
		clauses := cons2list(p.cdr().exp().pair())
		for i := len(clauses) - 1; i >= 0; i-- {
			clause := clauses[i].exp().pair()
			cond := clause.car()
			if isAtom(cond) {
				if cond.exp().atom().symbol() != "else" {
					panic("expected else")
				}
				if i != len(clauses)-1 {
					panic("else is not last in cond")
				}
				expanded = clause.cadr()
				continue
			}
			begin := []ExpOrProc{newSymbol("begin")}
			clause = clause.cdr().exp().pair()
			for clause != empty {
				begin = append(begin, clause.car())
				clause = clause.cdr().exp().pair()
			}
			expanded = pairExpression(list2cons(
				newSymbol("if"),
				cond,
				pairExpression(list2cons(begin...)),
				expanded,
			))
		}
		return expandMacro(expanded.exp().pair())
	case "let":
		bindings := cons2list(p.cadr().exp().pair())
		body := p.caddr()
		vars := make([]ExpOrProc, len(bindings))
		exps := make([]ExpOrProc, len(bindings))
		for i, b := range bindings {
			bl := b.exp().pair() // list of len 2
			vars[i] = bl.car()
			exps[i] = bl.cadr()
		}
		lambda := pairExpression(list2cons(
			newSymbol("lambda"),
			pairExpression(list2cons(vars...)),
			body,
		))
		return expandMacro(newPair(lambda, pairExpression(list2cons(exps...))))
	case "and":
		clauses := p.cdr().exp().pair()
		if clauses == empty {
			return newSymbol("#t")
		}
		if clauses.cdr().exp().pair() == empty {
			return clauses.car()
		}
		return expandMacro(list2cons(
			newSymbol("if"),
			clauses.car(),
			pairExpression(newPair(newSymbol("and"), clauses.cdr())),
			newSymbol("#f"),
		))
	case "list":
		clauses := p.cdr().exp().pair()
		if clauses == empty {
			return pairExpression(list2cons(newSymbol("quote"), pairExpression(empty)))
		}
		return pairExpression(list2cons(
			newSymbol("cons"),
			clauses.car(),
			pairExpression(newPair(newSymbol("list"), clauses.cdr())),
		))
	// kanren macros
	case "zzz":
		goal := p.cadr()
		sc := newSymbol("s/c")
		lambda := pairExpression(list2cons(
			newSymbol("lambda"),
			pairExpression(empty),
			pairExpression(list2cons(goal, sc)),
		))
		return expandMacro(list2cons(
			newSymbol("lambda"),
			pairExpression(list2cons(sc)),
			lambda,
		))
	case "conj+":
		g0 := list2cons(newSymbol("zzz"), p.cadr())
		g := p.cdr().exp().pair().cdr()
		if g.exp().pair() == empty {
			return expandMacro(g0)
		}
		return expandMacro(list2cons(
			newSymbol("conj"),
			pairExpression(g0),
			pairExpression(newPair(newSymbol("conj+"), g)),
		))
	case "disj+":
		g0 := list2cons(newSymbol("zzz"), p.cadr())
		g := p.cdr().exp().pair().cdr()
		if g.exp().pair() == empty {
			return expandMacro(g0)
		}
		return expandMacro(list2cons(
			newSymbol("disj"),
			pairExpression(g0),
			pairExpression(newPair(newSymbol("disj+"), g)),
		))
	case "conde":
		clauses := p.cdr().exp().pair()
		list := []ExpOrProc{newSymbol("disj+")}
		for clauses != empty {
			clauses.car().exp().pair() //require
			list = append(list, pairExpression(newPair(
				newSymbol("conj+"),
				clauses.car(),
			)))
			clauses = clauses.cdr().exp().pair()
		}
		return expandMacro(list2cons(list...))
	case "fresh":
		vars := p.cadr().exp().pair()
		goals := p.cdr().exp().pair().cdr()
		if vars == empty {
			return expandMacro(newPair(
				newSymbol("conj+"),
				goals,
			))
		}
		x0, xlist := vars.car(), vars.cdr()
		freshrec := pairExpression(newPair(
			newSymbol("fresh"),
			pairExpression(newPair(xlist, goals)),
		))
		lambda := pairExpression(list2cons(
			newSymbol("lambda"),
			pairExpression(list2cons(x0)),
			freshrec,
		))
		return expandMacro(list2cons(
			newSymbol("call/fresh"),
			lambda,
		))
	default:
		return pairExpression(p)
	}
}

func eval(e Exp) Exp {
	eop := ExpOrProc{isExp: true, value: e}
	return evalEnv(eop, GlobalEnv()).exp()
}

func evalEnv(x ExpOrProc, env Env) ExpOrProc {
	if !x.isExp {
		// x is a proc, evaluate returns itself
		return x
	}
Loop:
	for {
		e := x.exp()
		if e.isPair {
			e = expandMacro(e.pair()).exp()
		}
		if !e.isPair {
			a := e.atom()
			if a.isSymbol {
				return env.find(a.symbol()).dict[a.symbol()]
			}
			// number
			return x
		}
		// list
		p := e.pair()
		car := p.car()
		if !car.exp().isPair {
			s := car.exp().atom().symbol()
			switch s {
			case "if":
				test := p.cadr()
				conseq := p.caddr()
				tested := evalEnv(test, env)
				if isTruthy(tested) {
                    x = conseq
                    continue Loop
				}
				if p.cdddr().exp().pair() == empty {
                    x = atomWithValue(false)
                    continue Loop
				}
				alt := p.cadddr()
                x = alt
                continue Loop
            case "begin":
		        args := p.cdr().exp().pair()
		        for args.cdr().exp().pair() != empty {
			        evalEnv(args.car(), env)
			        args = args.cdr().exp().pair()
		        }
                x = args.car()
                continue Loop
			case "define":
				sym := p.cadr().exp().atom().symbol()
				exp := p.caddr()
				env.dict[sym] = evalEnv(exp, env)
				return atomWithValue(false)
			case "quote":
				return p.cadr()
			case "lambda":
				params := p.cadr().exp().pair()
				body := p.caddr()
				return ExpOrProc{isExp: false, value: Proc{isBuiltin: false,
					value: DefinedProc{
						params: params,
						body:   body,
						env:    env,
					},
				}}
				// default: falls through to procedure call
			}
		}
		// procedure call
		proc := evalEnv(car, env).proc()
		args := []ExpOrProc{}
		cdr := p.cdr().exp().pair()
		for cdr != empty {
			args = append(args, evalEnv(cdr.car(), env))
			cdr = cdr.cdr().exp().pair()
		}
		if proc.isBuiltin {
			return proc.builtin()(args)
		}
		defproc := proc.defined()
		x = defproc.body
		env = newEnv(defproc.params, args, defproc.env)
	}
}
