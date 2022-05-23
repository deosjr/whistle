package main

import (
	"testing"
)

func TestKanren(t *testing.T) {
	env := GlobalEnv()
	loadKanren(env)
	for i, tt := range []struct {
		input string
		want  string
	}{
		{
			input: "((call/fresh (lambda (q) (equalo q 5))) empty-state)",
			want:  "(((((var . 0) . 5)) . 1))",
		},
		{
			input: `(define a-and-b
                      (conj
                        (call/fresh (lambda (a) (equalo a 7)))
                        (call/fresh (lambda (b) (disj (equalo b 5) (equalo b 6))))))`,
		},
		{
			input: "(a-and-b empty-state)",
			want:  "(((((var . 1) . 5) ((var . 0) . 7)) . 2) ((((var . 1) . 6) ((var . 0) . 7)) . 2))",
		},
		{
			input: "(define fives (lambda (x) (disj (equalo x 5) (lambda (s/c) (lambda () ((fives x) s/c))))))",
		},
		{
			input: "(define sixes (lambda (x) (disj (equalo x 6) (lambda (s/c) (lambda () ((sixes x) s/c))))))",
		},
		{
			input: "(define fives-and-sixes (call/fresh (lambda (x) (disj (fives x) (sixes x)))))",
		},
		{
			input: "(take 4 (call/empty-state fives-and-sixes))",
			want:  "(((((var . 0) . 5)) . 1) ((((var . 0) . 6)) . 1) ((((var . 0) . 5)) . 1) ((((var . 0) . 6)) . 1))",
		},
		{
			input: "(run 4 fives-and-sixes)",
			want:  "(5 6 5 6)",
		},
	} {
		p := parse(tt.input)
		e := evalEnv(p, env)
		got := e.String()
		if got != tt.want {
			t.Errorf("%d) got %s want %s", i, got, tt.want)
		}
	}
}
