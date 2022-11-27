package main

import (
	"testing"
)

// examples taken from the paper
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
		e := evalEnv(env, p)
		got := e.String()
		if got != tt.want {
			t.Errorf("%d) got %s want %s", i, got, tt.want)
		}
	}
}

// mostly documenting simpler behaviour so I can refer back to it while studying
func TestLearnKanren(t *testing.T) {
	env := GlobalEnv()
	loadKanren(env)
	for i, tt := range []struct {
		input string
		want  string
	}{
		{
			// a state is a pair of a substitution (assoclist) and a variable counter
			input: "empty-state",
			want:  "(() . 0)",
		},
		{
			// unify returns a state with possible new substitutions if unification succeeds
			input: "(unify 1 1 empty-state)",
			want:  "(() . 0)",
		},
		{
			// if unification fails, it returns #f
			input: "(unify 1 2 empty-state)",
			want:  "",
		},
		{
			// equalo returns a goal, essentially wrapping unify.
			// goals take a state and return a stream, which is a list of states
			input: "((equalo 1 1) empty-state)",
			want:  "((() . 0))",
		},
		{
			input: "((equalo 1 2) empty-state)",
			want:  "()",
		},
		{
			// call/fresh introduces a new variable using the variable counter in state
			// when this variable is ignored, all it does is increase the counter
			input: "((call/fresh (lambda (q) (equalo 1 1))) empty-state)",
			want:  "((() . 1))",
		},
		{
			// when it is used, equalo binds it in a substitution.
			// the result here is identical to the previous except the state for success is one
			// that includes the substitution var0 == 5
			input: "((call/fresh (lambda (q) (equalo q 5))) empty-state)",
			want:  "(((((var . 0) . 5)) . 1))",
		},
		{
			// this will result in the unification check (unify var0 var0)
			// which succeeds without even checking var0's bindings.
			// the output therefore doesnt include any substitutions (as expected)
			input: "((call/fresh (lambda (q) (equalo q q))) empty-state)",
			want:  "((() . 1))",
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
