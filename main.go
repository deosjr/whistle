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
// TODO: this should just start a process which executes the function
// (loop (print (eval (read))))
func main() {
	env := GlobalEnv()
    loadErlang(env)
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
			if len(t) == 0 {
				panic("syntax error")
			}
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
    // TODO quote syntax only works on symbols, not lists atm!
    if token[0] == '\'' {
        quote, _ := readFromTokens([]string{"(", "quote", token[1:], ")"})
        return quote
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
	case "define-syntax":
		if len(list) != 3 {
			panic(fmt.Sprintf("invalid syntax %s", list2cons(list...)))
		}
		if !list[1].IsSymbol() {
			panic(fmt.Sprintf("invalid syntax %s", list2cons(list...)))
		}
	case "syntax-rules":
		if !list[1].IsPair() {
			panic(fmt.Sprintf("invalid syntax %s", list2cons(list...)))
		}
        for _, e := range list[2:] {
            if !e.IsPair() {
			    panic(fmt.Sprintf("invalid syntax %s", list2cons(list...)))
            }
            p := cons2list(e.AsPair())
            if len(p) != 2 {
			    panic(fmt.Sprintf("invalid syntax %s", list2cons(list...)))
            }
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

func (e *Env) find(s Symbol) (*Env, bool) {
	if _, ok := e.dict[s]; ok {
		return e, true
	}
	if e.outer == nil {
        return nil, false
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
            if e.AsPair() == empty {
                panic("invalid syntax ()")
            }
			ex, ok := expandMacro(e.AsPair())
            if ok {
                e = ex
                continue Loop
            }
		}
		if e.IsAtom() {
			if e.IsSymbol() {
				ed, ok := env.find(e.AsSymbol())
                if !ok {
		            panic(fmt.Sprintf("Exception: variable %s is not bound", e.AsSymbol()))
                }
                return ed.dict[e.AsSymbol()]
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
            case "define-syntax":
                keyword := p.cadr().AsSymbol()
                transformer := p.caddr().AsPair()
                macromap[keyword] = syntaxRules(keyword, transformer)
				return NewPrimitive(false)
            case "macroexpand":
                expanded, _ := expandMacro(p.cadr().AsPair())
                return expanded
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
        env, e = evalProcedureFromPair(env, proc, p.cdr().AsPair())
        if proc.isBuiltin {
            return e
        }
	}
}

func evalProcedureFromPair(env *Env, proc Proc, pargs Pair) (*Env, SExpression) {
    args := []SExpression{}
    for pargs != empty {
        args = append(args, pargs.car())
        pargs = pargs.cdr().AsPair()
    }
    return evalProcedure(env, proc, args...)
}

func evalProcedure(env *Env, proc Proc, args ...SExpression) (*Env, SExpression) {
    for i, arg := range args {
        args[i] = evalEnv(env, arg)
    }
	if proc.isBuiltin {
		return env, proc.builtin()(env, args)
	}
	defproc := proc.defined()
    // TODO: erlang hack. lambda closures overwrite pid in env
    // because they save env in which function was declared
    // solution: find a better hack than sending pid over env
    d, ok := defproc.env.find("$PID")
    if ok {
        ed, _ := env.find("$PID")
        d.dict["$PID"] = ed.dict["$PID"]
    }
    // end hack
	return newEnv(defproc.params, args, defproc.env), defproc.body
}
