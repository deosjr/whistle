package lisp

import "fmt"

type Lisp struct {
	process *process
	Env     *Env
}

func New() Lisp {
    if macromap == nil {
        initMacromap()
    }
	p := newProcess()
	env := GlobalEnv()
	return Lisp{p, env}
}

func (l Lisp) Eval(input string) (SExpression, error) {
	sexp, err := parse(input)
	if err != nil {
		return nil, err
	}
	if l.process.evalWithContinuation {
		id := func(x SExpression) SExpression { return x }
		return l.process.evalEnvK(l.Env, sexp, id), nil
	}
	return l.process.evalEnv(l.Env, sexp)
}

func (l Lisp) EvalExpr(e SExpression) (SExpression, error) {
	if l.process.evalWithContinuation {
		id := func(x SExpression) SExpression { return x }
		return l.process.evalEnvK(l.Env, e, id), nil
	}
	return l.process.evalEnv(l.Env, e)
}

func (l Lisp) Continue(e SExpression) (SExpression, error) {
	if !l.process.evalWithContinuation {
		return nil, fmt.Errorf("not running with continuation")
	}
	perr := e.AsPrimitive().(processError)
	return l.process.evalEnvK(l.Env, perr.v, perr.k), nil
}
