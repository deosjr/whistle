package main

// http://norvig.com/lispy.html

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

// REPL
func main() {
	env := GlobalEnv()
	scanner := bufio.NewScanner(os.Stdin)
	for {
		fmt.Print("> ")
		scanner.Scan()
		t := parse(scanner.Text())
		e := evalEnv(env, t)
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
        syntaxCheck(list)
		return list2cons(list...), tokens[1:]
	case ")":
		panic("unexpected ')'")
	default:
		return atom(token), tokens
	}
}

func atom(token string) SExpression {
	if n, err := strconv.ParseFloat(token, 64); err == nil {
		return NewPrimitive(n)
	}
	// TODO: strings cant have spaces in them atm!
	if token[0] == token[len(token)-1] && token[0] == '"' {
		return NewPrimitive(token[1 : len(token)-1])
	}
	return NewSymbol(token)
}

// check syntactic form of some builtins
// so we don't encounter weirdness at runtime
// TODO: how does this work with macros?
func syntaxCheck(list []SExpression) {
    if len(list) == 0 {
        return
    }
    if !list[0].IsSymbol() {
        return
    }
    switch list[0].AsSymbol() {
    case "if":
        if len(list) != 3 && len(list) != 4 {
            panic(fmt.Sprintf("invalid syntax %s", list2cons(list...)))
        }
    case "begin":
        if len(list) == 1 {
            panic(fmt.Sprintf("invalid syntax %s", list2cons(list...)))
        }
    case "quote":
        if len(list) != 2 {
            panic(fmt.Sprintf("invalid syntax %s", list2cons(list...)))
        }
    case "define":
        if len(list) != 3 {
            panic(fmt.Sprintf("invalid syntax %s", list2cons(list...)))
        }
        if !list[1].IsSymbol() {
            panic(fmt.Sprintf("invalid syntax %s", list2cons(list...)))
        }
    case "lambda":
        if len(list) != 3 {
            panic(fmt.Sprintf("invalid syntax %s", list2cons(list...)))
        }
        if !list[1].IsPair() {
            panic(fmt.Sprintf("invalid syntax %s", list2cons(list...)))
        }
    }
}

type Env struct {
	dict  map[Symbol]SExpression
	outer *Env
}

func (e Env) find(s Symbol) Env {
	if _, ok := e.dict[s]; ok {
		return e
	}
	if e.outer == nil {
        panic(fmt.Sprintf("Exception: variable %s is not bound", s))
	}
	return e.outer.find(s)
}


func newEnv(params Pair, args []SExpression, outer *Env) *Env {
	m := map[Symbol]SExpression{}
	i := 0
	for params != empty {
		m[params.car().AsSymbol()] = args[i]
		params = params.cdr().AsPair()
		i++
	}
	return &Env{dict: m, outer: outer}
}

func evalEnv(env *Env, e SExpression) SExpression {
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
			// primitive
			return e
		}
		// list, at which point car should be one of two things:
        // smth evaluating to procedure, or one of a few builtin symbols (which mark builtin procedures)
		p := e.AsPair()
		car := p.car()
		if car.IsSymbol() {
			s := car.AsSymbol()
            // builtin funcs that arent like other builtins:
            // they rely on their args not being evaluated first
            // their syntactic forms are checked at read-time
			switch s {
			case "if":
	            test := p.cadr()
	            conseq := p.caddr()
	            tested := evalEnv(env, test)
	            if isTruthy(tested) {
                    e = conseq
                    continue Loop
	            }
	            if p.cdddr().AsPair() == empty {
		            e = NewPrimitive(false)
                    continue Loop
	            }
	            alt := p.cadddr()
                e = alt
				continue Loop
			case "begin":
				args := p.cdr().AsPair()
				for args.cdr().AsPair() != empty {
					evalEnv(env, args.car())
					args = args.cdr().AsPair()
				}
				e = args.car()
				continue Loop
			case "quote":
				return p.cadr()
			case "define":
				sym := p.cadr().AsSymbol()
				exp := p.caddr()
				env.dict[sym] = evalEnv(env, exp)
				return NewPrimitive(false)
			case "lambda":
				params := p.cadr().AsPair()
				body := p.caddr()
				return Proc{sexpression: sexpression{
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
        e = evalEnv(env, car)
        if !e.IsProcedure() {
            panic(fmt.Sprintf("Exception: attempt to apply non-procedure %s", e))
        }
		proc := e.AsProcedure()
		args := []SExpression{}
		cdr := p.cdr().AsPair()
		for cdr != empty {
			args = append(args, evalEnv(env, cdr.car()))
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
