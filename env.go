package main

import (
    "fmt"
)

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

func (e *Env) replace(s Symbol, sexp SExpression) bool {
    outer, ok := e.find(s)
    if !ok {
        return false
    }
    outer.dict[s] = sexp
    return true
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

func (p process) evalEnv(env *Env, e SExpression) SExpression {
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
		ep := e.AsPair()
		car := ep.car()
		if car.IsSymbol() {
			s := car.AsSymbol()
			// builtin funcs that arent like other builtins:
			// they rely on their args not being evaluated first
			// their syntactic forms are checked at read-time
			switch s {
			case "if":
				test := ep.cadr()
				conseq := ep.caddr()
				tested := p.evalEnv(env, test)
				if isTruthy(tested) {
					e = conseq
					continue Loop
				}
				if ep.cdddr().AsPair() == empty {
					e = NewPrimitive(false)
					continue Loop
				}
				alt := ep.cadddr()
				e = alt
				continue Loop
			case "begin":
				args := ep.cdr().AsPair()
				for args.cdr().AsPair() != empty {
					p.evalEnv(env, args.car())
					args = args.cdr().AsPair()
				}
				e = args.car()
				continue Loop
			case "quote":
				return ep.cadr()
			case "define":
				sym := ep.cadr().AsSymbol()
				exp := ep.caddr()
				env.dict[sym] = p.evalEnv(env, exp)
				return NewPrimitive(false)
            case "define-syntax":
                keyword := ep.cadr().AsSymbol()
                transformer := ep.caddr().AsPair()
                macromap[keyword] = syntaxRules(keyword, transformer)
				return NewPrimitive(false)
            case "macroexpand":
                expanded, _ := expandMacro(ep.cadr().AsPair())
                return expanded
			case "lambda":
				params := ep.cadr().AsPair()
				body := ep.caddr()
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
		e = p.evalEnv(env, car)
		if !e.IsProcedure() {
			panic(fmt.Sprintf("Exception: attempt to apply non-procedure %s", e))
		}
		proc := e.AsProcedure()
        env, e = p.evalProcedureFromPair(env, proc, ep.cdr().AsPair())
        if proc.isBuiltin {
            return e
        }
	}
}

func (p process) evalProcedureFromPair(env *Env, proc Proc, pargs Pair) (*Env, SExpression) {
    args := []SExpression{}
    for pargs != empty {
        args = append(args, pargs.car())
        pargs = pargs.cdr().AsPair()
    }
    return p.evalProcedure(env, proc, args...)
}

func (p process) evalProcedure(env *Env, proc Proc, args ...SExpression) (*Env, SExpression) {
    for i, arg := range args {
        args[i] = p.evalEnv(env, arg)
    }
	if proc.isBuiltin {
		return env, proc.builtin()(p, env, args)
	}
	defproc := proc.defined()
	return newEnv(defproc.params, args, defproc.env), defproc.body
}
