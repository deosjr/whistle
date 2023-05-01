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

func (l Lisp) Eval(input string) (SExpression, error) {
	sexp, err := parse(input)
	if err != nil {
		return nil, err
	}
	return l.process.evalEnv(l.Env, sexp)
}
