package main

import (
    "fmt"
    "testing"
)

func TestErlang(t *testing.T) {
    pidi := 0
    pidFunc = func() string {
        pidi++
        return fmt.Sprintf("<%02d>", pidi)
    }
	env := GlobalEnv()
	loadErlang(env)
	for i, tt := range []struct {
		input string
		want  string
	} {
        {
            input: "(self)",
            want:  "<01>",
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
    } {
        p := parse(tt.input)
        e := evalEnv(env, p)
		got := e.String()
		if got != tt.want {
			t.Errorf("%d) got %s want %s", i, got, tt.want)
		}
    }
}
