package main

// http://norvig.com/lispy.html

import (
	"github.com/deosjr/lispadventures/lisp"
)

func main() {
	p, env := lisp.New()
	// example of adding function from outside lisp package
	env.AddBuiltin("/", func(args []lisp.SExpression) (lisp.SExpression, error) {
		return lisp.NewPrimitive(args[0].AsNumber() / args[1].AsNumber()), nil
	})
	// TODO: reintroduces extra newline after define
	p.Eval(env, `(define REPL (lambda (env)
        (begin (display "> ")
               (display (eval (read) env))
               (display newline)
               (REPL env))))`)
	// NOTE: injecting the env through closure/func argument
	// shows that spawn is still full of env sharing bugs!
	// DANGEROUS sharing of state, but will work :)
	// REPL is now restarted on error with env intact
	p.Eval(env, `(define restarter (lambda (env)
        (begin (process_flag 'trap_exit #t)
               (let ((pid (spawn_link (lambda () (REPL env)) (quote ()))))
                    (receive
                        ((reason) (quasiquote (EXIT ,pid ,reason)) ->
                            (if (eqv? reason "normal") #t
                            (begin (display "** exception error: ") (display reason) (display newline) (restarter env)))))))))`)
	p.Eval(env, "(restarter (environment))")
}
