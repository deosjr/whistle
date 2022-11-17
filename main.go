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

func parse(program string) SExpression {
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

func readFromTokens(tokens []string) (SExpression, []string) {
	if len(tokens) == 0 {
		panic("syntax error")
	}
	token := tokens[0]
	tokens = tokens[1:]
	switch token {
	case "(":
		list := []SExpression{}
		for tokens[0] != ")" {
			parsed, t := readFromTokens(tokens)
			tokens = t
			list = append(list, parsed)
		}
        return list2cons(list...), tokens[1:]
	case ")":
		panic("unexpected ')'")
	default:
        return atom(token), tokens
	}
}

func atom(token string) SExpression {
	if n, err := strconv.ParseFloat(token, 64); err == nil {
        return NewNumber(n)
	}
    return NewSymbol(token)
}

func GlobalEnv() Env {
	return Env{dict: map[Symbol]SExpression{
		"*": builtinFunc(func(args []SExpression) SExpression {
			return atomWithValue(args[0].AsNumber() * args[1].AsNumber())
		}),
		"+": builtinFunc(func(args []SExpression) SExpression {
			return atomWithValue(args[0].AsNumber() + args[1].AsNumber())
		}),
		"-": builtinFunc(func(args []SExpression) SExpression {
			return atomWithValue(args[0].AsNumber() - args[1].AsNumber())
		}),
		"=": builtinFunc(func(args []SExpression) SExpression {
			return atomWithValue(args[0].AsNumber() == args[1].AsNumber())
		}),
		">": builtinFunc(func(args []SExpression) SExpression {
			return atomWithValue(args[0].AsNumber() > args[1].AsNumber())
		}),
		"<=": builtinFunc(func(args []SExpression) SExpression {
			return atomWithValue(args[0].AsNumber() <= args[1].AsNumber())
		}),
		"#t":    atomWithValue(true),
		"#f":    atomWithValue(false),
		"pi":    atomWithValue(math.Pi),
		"number?": builtinFunc(func(args []SExpression) SExpression {
            return atomWithValue(args[0].IsNumber())
        }),
		"pair?": builtinFunc(func(args []SExpression) SExpression {
            x := args[0]
            if !x.IsPair() {
                return atomWithValue(false)
            }
            return atomWithValue(x.AsPair() != empty)
        }),
		"car": builtinFunc(func(args []SExpression) SExpression {
			return args[0].AsPair().car()
        }),
		"cdr": builtinFunc(func(args []SExpression) SExpression {
			return args[0].AsPair().cdr()
        }),
		"cons": builtinFunc(func(args []SExpression) SExpression {
			return NewPair(args[0], args[1])
        }),
		"null?": builtinFunc(func(args []SExpression) SExpression {
			x := args[0]
			if x.IsProcedure() {
				return atomWithValue(false)
			}
			if x.IsAtom() {
				return atomWithValue(false)
			}
			return atomWithValue(x.AsPair() == empty)
        }),
		"procedure?": builtinFunc(func(args []SExpression) SExpression {
			return atomWithValue(args[0].IsProcedure())
        }),
		"eqv?": builtinFunc(func(args []SExpression) SExpression {
			return atomWithValue(reflect.DeepEqual(args[0], args[1]))
        }),
		"display": builtinFunc(func(args []SExpression) SExpression {
			fmt.Println(args[0])
			return atomWithValue(true)
        }),
	}, outer: nil}
}

func newEnv(params Pair, args []SExpression, outer Env) Env {
	m := map[Symbol]SExpression{}
	i := 0
	for params != empty {
		m[params.car().AsSymbol()] = args[i]
		params = params.cdr().AsPair()
		i++
	}
	return Env{dict: m, outer: &outer}
}

// TODO: obviously its not macros rn, its just hardcoded language features
// They cannot be defined from repl yet!
func expandMacro(p Pair) SExpression {
	if !p.car().IsAtom() {
		return p
	}
	s := p.car().AsSymbol()
	switch s {
	case "cond":
        var expanded SExpression
		expanded = atomWithValue(false)
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

func eval(e SExpression) SExpression {
	return evalEnv(e, GlobalEnv())
}

func evalEnv(e SExpression, env Env) SExpression {
	if e.IsProcedure() {
		// e is a proc, evaluate returns itself
		return e
	}
Loop:
	for {
		if e.IsPair() {
			e = expandMacro(e.AsPair())
		}
		if e.IsAtom() {
			if e.IsSymbol() {
				return env.find(e.AsSymbol()).dict[e.AsSymbol()]
			}
			// number
			return e
		}
		// list
		p := e.AsPair()
		car := p.car()
		if car.IsAtom() {
			s := car.AsSymbol()
			switch s {
			case "if":
				test := p.cadr()
				conseq := p.caddr()
				tested := evalEnv(test, env)
				if isTruthy(tested) {
                    e = conseq
                    continue Loop
				}
				if p.cdddr().AsPair() == empty {
                    e = atomWithValue(false)
                    continue Loop
				}
				alt := p.cadddr()
                e = alt
                continue Loop
            case "begin":
		        args := p.cdr().AsPair()
		        for args.cdr().AsPair() != empty {
			        evalEnv(args.car(), env)
			        args = args.cdr().AsPair()
		        }
                e = args.car()
                continue Loop
			case "define":
				sym := p.cadr().AsSymbol()
				exp := p.caddr()
				env.dict[sym] = evalEnv(exp, env)
				return atomWithValue(false)
			case "quote":
				return p.cadr()
			case "lambda":
				params := p.cadr().AsPair()
				body := p.caddr()
                return Proc{sexpression:sexpression{
                    value: DefinedProc{
                        params: params,
                        body: body,
                        env: env,
                    },
                }}
				// default: falls through to procedure call
			}
		}
		// procedure call
		proc := evalEnv(car, env).AsProcedure()
		args := []SExpression{}
		cdr := p.cdr().AsPair()
		for cdr != empty {
			args = append(args, evalEnv(cdr.car(), env))
			cdr = cdr.cdr().AsPair()
		}
		if proc.isBuiltin {
			return proc.builtin()(args)
		}
		defproc := proc.defined()
		e = defproc.body
		env = newEnv(defproc.params, args, defproc.env)
	}
}
