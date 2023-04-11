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
        // (receive (pattern expression) ...)
        // FIRST ATTEMPT: only constants matched in pattern, no unification with vars yet
        {
            input: `(define-syntax receive (syntax-rules (let receive_msg receive_)
                      ((_ (pattern expression) ...)
                       (let ((msg (receive_msg))) (receive_ msg (pattern expression) ...)))))`,
        },
        {
            input: `(define-syntax receive_ (syntax-rules (eqv?)
                      ((_ _) #f)
                      ((_ msg (pattern expression) b ...)
                       (if (eqv? pattern msg) expression (receive_ msg b ...)))))`,
        },
        {
            input: `(define pid (let ((this (self)))
                      (spawn (lambda ()
                        (receive
                          ('request (send this 'response))
                        ))
                      (quote ()))))`,
        },
        {
            input: "(send pid 'request)",
            want:  "request",
        },
        {
            input: "(receive ('response 'received))",
            want:  "received",
        },
        // (receive (pattern expression) ...)
        // SECOND ATTEMPT: only matching a single declared var in each pattern
        {
            input: "(define msg (quote ('sender 'atom)))",
        },
        {
            input: "(run 1 (fresh (from q) (equalo q (quasiquote (,from 'atom))) (equalo q msg)))",
            want:  "((quote sender))",
        },
        {
            input: `(define-syntax receive (syntax-rules (let receive_msg receive_)
                      ((_ (var pattern expression ...) ...)
                       (let ((msg (receive_msg))) (receive_ msg (var pattern expression ...) ...)))))`,
        },
        {
            input: `(define-syntax receive_ (syntax-rules (eqv? let run fresh equalo car null?)
                      ((_ _) #f)
                      ((_ msg (var pattern expression ...) b ...)
                       (let ((match (run 1 (fresh (var q) (equalo q pattern) guard ... (equalo q msg)))))
                         (if (null? match)
                           (receive_ msg b ...)
                           (let ((var (car match))) expression ...))))))`,
        },
        {
            input: `(define rec (lambda (sender)
                        (receive
                          (from (quasiquote (,from 'request))
                            (send sender (quasiquote (,from 'response)))
                            (rec sender))
                          (x x
                            (send sender (quasiquote (('unknown ,x) 'response)))
                            (rec sender))
                        )))`,
        },
        {
            input: `(define pid (let ((this (self)))
                      (spawn rec (quasiquote (,this)))))`,
        },
        {
            input: "(send pid (quote ('sender 'request)))",
            want:  "((quote sender) (quote request))",
        },
        {
            input: "(receive (x (quasiquote (,x 'response)) x))",
            want:  "(quote sender)",
        },
        {
            input: "(send pid 'wrongmessage)",
            want:  "wrongmessage",
        },
        {
            input: "(receive (x (quasiquote (,x 'response)) x))",
            want:  "((quote unknown) wrongmessage)",
        },
        // TODO: 'when' guards are simply minikanren statements to inject in run!
    } {
        p := parse(tt.input)
        e := main.evalEnv(env, p)
		got := e.String()
		if got != tt.want {
			t.Errorf("%d) got %s want %s", i, got, tt.want)
		}
    }
}
