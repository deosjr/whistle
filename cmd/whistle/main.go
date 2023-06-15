package main

// http://norvig.com/lispy.html

import (
	"fmt"
	"os"

	"github.com/deosjr/whistle/datalog"
	"github.com/deosjr/whistle/erlang"
	"github.com/deosjr/whistle/kanren"
	"github.com/deosjr/whistle/lisp"
)

func main() {
	if len(os.Args) < 2 {
		startREPL()
		return
	}
	filename := os.Args[1]
	sexpressions, err := lisp.ParseFile(filename)
	if err != nil {
		fmt.Println(err)
		return
	}
	l := lisp.New()
	kanren.Load(l)
	erlang.Load(l)
	datalog.Load(l)
	for _, e := range sexpressions {
		_, err := l.EvalExpr(e)
		if err != nil {
			fmt.Println(err)
			return
		}
	}
}

func startREPL() {
	l := lisp.New()
	kanren.Load(l)
	erlang.Load(l)
	datalog.Load(l)
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
               (let ((pid (spawn_link (lambda () (begin (process_flag 'eval_with_continuation #t) (REPL env))) '())))
                    (receive
                        ((reason) (quasiquote (EXIT ,pid ,reason)) ->
                            (if (eqv? reason "normal") #t
                            (begin (display "** exception error: ") (display reason) (display newline) (restarter env)))))))))`)
	l.Eval("(restarter (environment))")
}
