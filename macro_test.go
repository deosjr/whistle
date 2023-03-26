package main

import (
    "testing"
)

func TestDefSyntax(t *testing.T) {
	env := GlobalEnv()
	for i, tt := range []struct {
		input string
		want  string
	} {
        {
            input: "(define-syntax test-macro (syntax-rules () ((_) 1) ((_ v) (cons v 2))))",
        },
        {
            input: "(test-macro)",
            want:  "1",
        },
        {
            input: "(test-macro 1)",
            want:  "(1 . 2)",
        },
        {
            input: `(define-syntax or
                      (syntax-rules ()
                        ((_) #f)
                        ((_ e) e)
                        ((_ e ...) (if e e (or ...)))))`,
        },
        {
            input: "(or #f #f 'yes)",
            want:  "yes",
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
