package main

import (
    "fmt"
    "testing"
)

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
            input: `(define rec_with_prio (lambda (sender)
                        (receive
                          ((priority) (quasiquote (,priority 'request)) (when (equalo priority 1)) ->
                            (send sender (quote ('high 'response)))
                            (rec_with_prio sender))
                          ((priority) (quasiquote (,priority 'request)) (when (equalo priority 2)) ->
                            (send sender (quote ('normal 'response)))
                            (rec_with_prio sender))
                          ((priority) (quasiquote (,priority 'request)) (when (equalo priority 3)) ->
                            (send sender (quote ('low 'response)))
                            (rec_with_prio sender))
                        )))`,
        },
        {
            input: `(define pid (let ((this (self)))
                      (spawn rec_with_prio (quasiquote (,this)))))`,
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
    } {
        p := parse(tt.input)
        fmt.Println(p)
        e := main.evalEnv(env, p)
		got := e.String()
		if got != tt.want {
			t.Errorf("%d) got %s want %s", i, got, tt.want)
		}
    }
}
