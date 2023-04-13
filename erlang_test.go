package main

import (
    "fmt"
    "testing"
)

// https://learnyousomeerlang.com/more-on-multiprocessing
func TestErlangReceiveMacro(t *testing.T) {
    pidi := 0
    pidFunc = func() string {
        pidi++
        return fmt.Sprintf("<%02d>", pidi)
    }
    main := newProcess()
	env := GlobalEnv()
	loadErlang(env)
    loadKanren(main, env) // for pattern matching in receive
	for i, tt := range []struct {
		input string
		want  string
	} {
        {
            input: "(self)",
            want:  "<01>",
        },
        {
            input: `(define rec (lambda (sender)
                        (receive
                          ((priority) (quasiquote (,priority 'request)) (when (equalo priority 1)) ->
                            (send sender (quote ('high 'response)))
                            (rec sender))
                          ((priority) (quasiquote (,priority 'request)) (when (equalo priority 2)) ->
                            (send sender (quote ('normal 'response)))
                            (rec sender))
                          ((priority) (quasiquote (,priority 'request)) (when (equalo priority 3)) ->
                            (send sender (quote ('low 'response)))
                            (rec sender))
                        )))`,
        },
        {
            input: `(define pid (let ((this (self)))
                      (spawn rec (quasiquote (,this)))))`,
        },
        {
            // doesnt match anything, ignored in mailbox!
            input: "(send pid (quote (4 'request)))",
            want:  "(4 (quote request))",
        },
        {
            input: "(send pid (quote (2 'request)))",
            want:  "(2 (quote request))",
        },
        {
            input: "(receive ((x) (quasiquote (,x 'response)) -> x))",
            want:  "(quote normal)",
        },
        {
            input: `(define important (lambda ()
                      (receive 
                        ((priority message) (quasiquote (,priority ,message)) (when (equalo priority 1)) ->
                          (cons message (important)))
                        (after 0 -> (normal)))))`,
        },
        {
            input: `(define normal (lambda ()
                      (receive 
                        ((priority message) (quasiquote (,priority ,message)) (when (equalo priority 3)) ->
                          (cons message (normal)))
                        (after 0 -> (quote ()) )))))`,
        },
        {
            input: "(begin (send (self) (quote (1 high))) (send (self) (quote (3 low))) (send (self) (quote (3 low))) (send (self) (quote (1 high))))",
            want:  "(1 high)",
        },
        {
            input: "(important)",
            want:  "(high high low low)",
        },
    } {
        p := parse(tt.input)
        e := main.evalEnv(env, p)
		got := e.String()
		if got != tt.want {
			t.Errorf("%d) got %s want %s", i, got, tt.want)
		}
    }
}
