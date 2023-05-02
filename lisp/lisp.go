package lisp

type Lisp struct {
	process *process
	Env     *Env
}

func New() Lisp {
	p := newProcess()
	env := GlobalEnv()
	loadKanren(p, env)
	loadErlang(p, env)
	return Lisp{p, env}
}

// TODO: switch between these based on process_flag?

func (l Lisp) Eval(input string) (SExpression, error) {
	sexp, err := parse(input)
	if err != nil {
		return nil, err
	}
	return l.process.evalEnv(l.Env, sexp)
}

func (l Lisp) EvalK(input string) (SExpression, error) {
	sexp, err := parse(input)
	if err != nil {
		return nil, err
	}
    id := func(x SExpression) SExpression { return x }
	return l.process.evalEnvK(l.Env, sexp, id), nil
}
