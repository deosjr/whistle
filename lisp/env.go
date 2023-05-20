package lisp

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

func (e *Env) Add(s Symbol, sexp SExpression) {
	e.dict[s] = sexp
}

func (e *Env) addBuiltin(s Symbol, f BuiltinProc) {
	e.dict[s] = builtinFunc(f)
}

func (e *Env) AddBuiltin(s Symbol, f ExternalProc) {
	e.dict[s] = builtinFunc(func(p *process, env *Env, args []SExpression) (SExpression, error) {
		return f(args)
	})
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

func (p *process) evalEnv(env *Env, e SExpression) (SExpression, error) {
	if p.err != nil {
		return nil, p.err
	}
	if e.IsProcedure() {
		// e is a proc, evaluate returns itself
		return e, nil
	}
Loop:
	for {
		if e.IsPair() {
			if e.AsPair() == empty {
				return nil, fmt.Errorf("invalid syntax ()")
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
					return nil, fmt.Errorf("variable %s is not bound", e.AsSymbol())
				}
				return ed.dict[e.AsSymbol()], nil
			}
			// primitive
			return e, nil
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
				tested, err := p.evalEnv(env, test)
				if err != nil {
					return nil, err
				}
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
					if _, err := p.evalEnv(env, args.car()); err != nil {
						return nil, err
					}
					args = args.cdr().AsPair()
				}
				e = args.car()
				continue Loop
			case "quote":
				return ep.cadr(), nil
			case "define":
				sym := ep.cadr().AsSymbol()
				exp := ep.caddr()
				evalled, err := p.evalEnv(env, exp)
				if err != nil {
					return nil, err
				}
				env.dict[sym] = evalled
				return NewPrimitive(false), nil
			case "set!":
				sym := ep.cadr().AsSymbol()
				exp := ep.caddr()
				evalled, err := p.evalEnv(env, exp)
				if err != nil {
					return nil, err
				}
				// TODO: will silently fail if not found
				// check output bool if you want a proper error
				env.replace(sym, evalled)
				return NewPrimitive(false), nil
			case "define-syntax":
				keyword := ep.cadr().AsSymbol()
				transformer := ep.caddr().AsPair()
				macromap[keyword] = syntaxRules(keyword, transformer)
				return NewPrimitive(false), nil
			case "macroexpand":
				expanded, _ := expandMacro(ep.cadr().AsPair())
				return expanded, nil
			case "lambda":
				params := ep.cadr().AsPair()
				body := ep.caddr()
				return Proc{sexpression: sexpression{
					value: DefinedProc{
						params: params,
						body:   body,
						env:    env,
					},
				}}, nil
				// default: falls through to procedure call
			}
		}
		// procedure call
		peval, err := p.evalEnv(env, car)
		if err != nil {
			return nil, err
		}
		e = peval
		if !e.IsProcedure() {
			return nil, fmt.Errorf("attempt to apply non-procedure %s", e)
		}
		proc := e.AsProcedure()
		pargs := ep.cdr().AsPair()
		args := []SExpression{}
		for pargs != empty {
			args = append(args, pargs.car())
			pargs = pargs.cdr().AsPair()
		}
		for i, arg := range args {
			evarg, err := p.evalEnv(env, arg)
			if err != nil {
				return nil, err
			}
			args[i] = evarg
		}
		if proc.isBuiltin {
			return proc.builtin()(p, env, args)
		}
		defproc := proc.defined()
		env, e = newEnv(defproc.params, args, defproc.env), defproc.body
	}
}

type continuation func(SExpression) SExpression

// TODO: representing an error as an SExpression that includes error msg,
// but also continuation and value!
// NOTE: instead of evalEnv above, here we'll reify error+continuation as an SExpression
// TODO: add back tail recursion!?
func (p *process) evalEnvK(env *Env, e SExpression, k continuation) SExpression {
	if e.IsProcedure() {
		return k(e)
	}
	if e.IsPair() {
		if e.AsPair() == empty {
			return p.Errorf(k, e, "invalid syntax ()")
		}
		ex, ok := expandMacro(e.AsPair())
		if ok {
			return p.evalEnvK(env, ex, k)
		}
	}
	if e.IsAtom() {
		if e.IsSymbol() {
			ed, ok := env.find(e.AsSymbol())
			if !ok {
				return p.Errorf(k, e, "variable %s is not bound", e.AsSymbol())
			}
			return k(ed.dict[e.AsSymbol()])
		}
		// primitive
		return k(e)
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
			return p.evalEnvK(env, test, func(tested SExpression) SExpression {
				if isTruthy(tested) {
					return p.evalEnvK(env, conseq, k)
				}
				if ep.cdddr().AsPair() == empty {
					return k(NewPrimitive(false))
				}
				alt := ep.cadddr()
				return p.evalEnvK(env, alt, k)
			})
		case "begin":
			// guaranteed in parsing to not be empty
			var begin func(Pair) SExpression
			begin = func(args Pair) SExpression {
				if args.cdr().AsPair() == empty {
					return p.evalEnvK(env, args.car(), k)
				}
				return p.evalEnvK(env, args.car(), func(SExpression) SExpression {
					return begin(args.cdr().AsPair())
				})
			}
			epargs := ep.cdr().AsPair()
			return begin(epargs)
		case "quote":
			return k(ep.cadr())
		case "define":
			sym := ep.cadr().AsSymbol()
			exp := ep.caddr()
			return p.evalEnvK(env, exp, func(evalled SExpression) SExpression {
				env.dict[sym] = evalled
				return k(nil)
			})
		case "set!":
			sym := ep.cadr().AsSymbol()
			exp := ep.caddr()
			return p.evalEnvK(env, exp, func(evalled SExpression) SExpression {
				// TODO: will silently fail if not found
				// check output bool if you want a proper error
				env.replace(sym, evalled)
				return k(nil)
			})
		case "define-syntax":
			keyword := ep.cadr().AsSymbol()
			transformer := ep.caddr().AsPair()
			macromap[keyword] = syntaxRules(keyword, transformer)
			return k(nil)
		case "macroexpand":
			expanded, _ := expandMacro(ep.cadr().AsPair())
			return k(expanded)
		case "call/cc":
			// takes as argument a function f that in turn only takes one arg,
			// which is assumed to be a continuation object c
			// if this continuation is evaluated, it overwrites k arg on process
			// call/cc itself returns f applied to c or (f c)
			f := ep.cadr()
			c := NewPrimitive(k)
			fc := list2cons(f, c)
			return p.evalEnvK(env, fc, k)
		case "lambda":
			params := ep.cadr().AsPair()
			body := ep.caddr()
			return k(Proc{sexpression: sexpression{
				value: DefinedProc{
					params: params,
					body:   body,
					env:    env,
				},
			}})
			// default: falls through to procedure call
		}
	}
	// procedure call
	args := []SExpression{}
	var evalArgs func(Proc, Pair) SExpression
	evalArgs = func(proc Proc, pargs Pair) SExpression {
		if pargs == empty {
			// were done, apply the function!
			if proc.isBuiltin {
				ex, err := proc.builtin()(p, env, args)
				if err != nil {
					return p.Errorf(k, e, err.Error())
				}
				return k(ex)
			}
			defproc := proc.defined()
			newenv := newEnv(defproc.params, args, defproc.env)
			return p.evalEnvK(newenv, defproc.body, k)
		}
		// continue evalling arguments
		return p.evalEnvK(env, pargs.car(), func(evalled SExpression) SExpression {
			args = append(args, evalled)
			return evalArgs(proc, pargs.cdr().AsPair())
		})
	}
	return p.evalEnvK(env, car, func(f SExpression) SExpression {
		// applying continuation completely ignores previous k
		if f.IsPrimitive() {
			if c, ok := f.AsPrimitive().(continuation); ok {
				arg := ep.cadr()
				return p.evalEnvK(env, arg, c)
			}
		}
		if !f.IsProcedure() {
			return p.Errorf(k, e, "attempt to apply non-procedure %s", f)
		}
		proc := f.AsProcedure()
		pargs := ep.cdr().AsPair()
		return evalArgs(proc, pargs)
	})
}

func copyEnv(env *Env) *Env {
	if env == nil {
		return nil
	}
	cenv := &Env{outer: copyEnv(env.outer)}
	m := map[Symbol]SExpression{}
	for k, v := range env.dict {
		if v.IsProcedure() && !v.AsProcedure().isBuiltin {
			d := v.AsProcedure().defined()
			// TODO: does this always hold? I think so but not sure
			d.env = cenv
			m[k] = Proc{sexpression: sexpression{value: d}}
			continue
		}
		m[k] = v
	}
	cenv.dict = m
	return cenv
}
