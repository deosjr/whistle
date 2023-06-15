package main

import (
	"github.com/deosjr/whistle/erlang"
	"github.com/deosjr/whistle/kanren"
	"github.com/deosjr/whistle/lisp"
)

func main() {
	l := lisp.New()
	kanren.Load(l)
	erlang.Load(l)
	l.Eval(`(define REPL (lambda (env)
        (begin (display "> ")
               (display (eval (read) env))
               (display newline)
               (REPL env))))`)
	l.Eval(`(define restarter (lambda (env)
        (begin (process_flag 'trap_exit #t)
               (let ((pid (spawn_link (lambda () (begin (process_flag 'eval_with_continuation #t) (REPL env))) '())))
                    (receive
                        ((reason) (quasiquote (EXIT ,pid ,reason)) ->
                            (if (eqv? reason "normal") #t
                            (begin (display "** exception error: ") (display reason) (display newline) (restarter env)))))))))`)
	l.Eval("(restarter (environment))") // starts an interactive REPL
}
