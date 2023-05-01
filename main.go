package main

// http://norvig.com/lispy.html

import (
	"github.com/deosjr/lispadventures/lisp"
)

func main() {
	l := lisp.New()
	// TODO: reintroduces extra newline after define
	l.Eval(`(define REPL (lambda (env)
        (begin (display "> ")
               (display (eval (read) env))
               (display newline)
               (REPL env))))`)
	// NOTE: injecting the env through closure/func argument
	// shows that spawn is still full of env sharing bugs!
	// DANGEROUS sharing of state, but will work :)
	// REPL is now restarted on error with env intact
	l.Eval(`(define restarter (lambda (env)
        (begin (process_flag 'trap_exit #t)
               (let ((pid (spawn_link (lambda () (REPL env)) (quote ()))))
                    (receive
                        ((reason) (quasiquote (EXIT ,pid ,reason)) ->
                            (if (eqv? reason "normal") #t
                            (begin (display "** exception error: ") (display reason) (display newline) (restarter env)))))))))`)
	l.Eval("(restarter (environment))")
}
