package main

import (
    "reflect"
    "testing"
)

func TestUnification(t *testing.T) {
	for i, tt := range []struct {
		pattern string
        input   string
		want    map[string]syntaxSub
	} {
        {
            pattern: "(_ x)",
            input:   "(macro 42)",
            want:    map[string]syntaxSub{
                "x": syntaxSub{sexpr: parse("42")},
            },
        },
    } {
        s := map[string]syntaxSub{}
        ok := unify(parse(tt.pattern), parse(tt.input), s)
        if !ok || !reflect.DeepEqual(s, tt.want) {
            t.Errorf("%d) got %v want %v", i, s, tt.want)
        }
    }
}

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
            input: "(define-syntax test-macro (syntax-rules () ((_ (a b)) (cons b a))))",
        },
        {
            input: "(test-macro (1 2))",
            want:  "(2 . 1)",
        },
        {
            input: `(define-syntax or
                      (syntax-rules ()
                        ((_) #f)
                        ((_ e) e)
                        ((_ e1 e2 ...) (if e1 e1 (or e2 ...)))))`,
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
