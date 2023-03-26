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
		{
			// disj and conj both merge streams using mplus
			input: "((disj (equalo 1 1) (equalo 1 2)) empty-state)",
			want:  "((() . 0))",
		},
		{
			// conj uses bind to combine substitutions before applying mplus
			input: "((conj (equalo 1 1) (equalo 1 2)) empty-state)",
			want:  "()",
		},
		{
			// this result stream has 2 states, one for each matching substitution in the disjunction
			input: "((call/fresh (lambda (q) (disj (equalo q 5) (equalo q 6)))) empty-state)",
			want:  "(((((var . 0) . 5)) . 1) ((((var . 0) . 6)) . 1))",
		},
		{
			// using conj here in comparison still equates to failure, as indicated by a stream of 0 valid states
			input: "((call/fresh (lambda (q) (conj (equalo q 1) (equalo q 2)))) empty-state)",
			want:  "()",
		},
		{
			// now the second goal equates to true, and the second state result changes
			input: "((call/fresh (lambda (q) (disj (equalo q 5) (equalo q q)))) empty-state)",
			want:  "(((((var . 0) . 5)) . 1) (() . 1))",
		},
		{
			// in conj, there is now one valid state matching both equalo goals: q==5
			input: "((call/fresh (lambda (q) (conj (equalo q 5) (equalo q q)))) empty-state)",
			want:  "(((((var . 0) . 5)) . 1))",
		},
		{
			// here it is again but using the run* macro for brevity in the output (only reifying var0)
			input: "(run* (call/fresh (lambda (q) (conj (equalo q 5) (equalo q q)))))",
			want:  "(5)",
		},
		{
			// lets look a bit more into mplus. merging two empty streams is just the empty stream again
			input: "(mplus mzero mzero)",
			want:  "()",
		},
		{
			// empty-state is the simplest state, merging with mzero is the identity function
			input: "(mplus (unit empty-state) mzero)",
			want:  "((() . 0))",
		},
		{
			// which is commutative
			input: "(mplus mzero (unit empty-state))",
			want:  "((() . 0))",
		},
		{
			// lets use these two streams moving forward, each consisting of a single state: one-state has one bound var
			input: "(define one-state ((call/fresh (lambda (x) (equalo x x))) empty-state))",
		},
		{
			// and two-state has two bound vars, just to tell two streams apart in the following
			input: "(define two-state ((call/fresh (lambda (x) (call/fresh (lambda (y) (conj (equalo x x) (equalo y y)))))) empty-state))",
		},
		{
			// mplus of these two streams works like append
			input: "(mplus one-state two-state)",
			want:  "((() . 1) (() . 2))",
		},
		{
			// meaning in general, mplus is not commutative
			input: "(mplus two-state one-state)",
			want:  "((() . 2) (() . 1))",
		},
		{
			// bind takes a stream and a goal. it returns a stream of states where goal is applied to all states in the input stream
			// binding anything to mzero results in mzero
			input: "(bind mzero (equalo 1 1))",
			want:  "()",
		},
		{
			// no bindings needed to succeed, but we have bound (and ignored) one variable in one-state
			input: "(bind one-state (equalo 1 1))",
			want:  "((() . 1))",
		},
		{
			input: "(define five-six-seven (lambda (x) (disj (disj (equalo x 5) (equalo x 6)) (equalo x 7))))",
		},
		{
			input: "(define seven-eight-nine (lambda (x) (disj (disj (equalo x 7) (equalo x 8)) (equalo x 9))))",
		},
		{
			// notice no interleaving happening here: the result is one stream of states where x is bound
			// first according to the first full disjunction and then to the second
			// interleaving only happens when flagged by the user defining relations using thunks, or immature streams
			input: "(mK-reify ((lambda (x) (mplus ((five-six-seven x) empty-state) ((seven-eight-nine x) empty-state))) (var 0)))",
			want:  "(5 6 7 7 8 9)",
		},
		{
			// bind will find the intersection in valid states still
			input: "(mK-reify ((lambda (x) (bind ((five-six-seven x) empty-state) (seven-eight-nine x))) (var 0)))",
			want:  "(7)",
		},
		{
			// redefining these using immature streams to hand over control (trampolining) in order to see interleaving
			input: `(define five-six-seven (lambda (x) (disj
                      (lambda (s/c) (lambda () ((equalo x 5) s/c)))
                      (disj (lambda (s/c) (lambda () ((equalo x 6) s/c))) (lambda (s/c) (lambda () ((equalo x 7) s/c)))))))`,
		},
		{
			input: `(define seven-eight-nine (lambda (x) (disj
                      (lambda (s/c) (lambda () ((equalo x 7) s/c)))
                      (disj (lambda (s/c) (lambda () ((equalo x 8) s/c))) (lambda (s/c) (lambda () ((equalo x 9) s/c)))))))`,
		},
		{
			// which can also be written using the disj+ macro as follows:
			input: "(define five-six-seven (lambda (x) (disj+ (equalo x 5) (equalo x 6) (equalo x 7))))",
		},
		{
			input: "(define seven-eight-nine (lambda (x) (disj+ (equalo x 7) (equalo x 8) (equalo x 9))))",
		},
		{
			// here we need to use run because it knows how to pull from immature streams (mK-reify as above would break)
			// notice now interleaving happens! but there is no interleaving at the end of the stream (no (5 7 6 8 7 9) !)
			input: "(run* ((lambda (x) (mplus ((five-six-seven x) empty-state) ((seven-eight-nine x) empty-state))) (var 0)))",
			want:  "(5 7 6 7 8 9)",
		},
		{
			// and conj in this way is not affected at all
			input: "(run* ((lambda (x) (bind ((five-six-seven x) empty-state) (seven-eight-nine x))) (var 0)))",
			want:  "(7)",
		},
        {
            // can we define an ifthenelse now that we know how mplus/bind work?
            // would it be pure? why/why not? how does this work with infinite streams?
            input: `(define ifthenelse (lambda (g1 g2 g3)
                      (lambda (s/c)
                        (let ((s (pull (g1 s/c))))
                          (if (null? s)
                            (g3 s/c)
                            (bind s g2))))))`,
        },
        {
            // commits to the goal it can satisfy (x==1), binding y==2 and never returns y==3
            input: "(call/empty-state (call/fresh (lambda (x) (call/fresh (lambda (y) (ifthenelse (equalo x 1) (equalo y 2) (equalo y 3)))))))",
            want:  "(((((var . 1) . 2) ((var . 0) . 1)) . 2))",
        },
        {
            // now the first goal fails, so we commit to the else clause of y==3.
            // since the first goal failed, we know that x=\=1
            input: "(call/empty-state (call/fresh (lambda (x) (call/fresh (lambda (y) (conj (equalo x 2) (ifthenelse (equalo x 1) (equalo y 2) (equalo y 3))))))))",
            want:  "(((((var . 1) . 3) ((var . 0) . 2)) . 2))",
        },
        {
            // (AND x==2 ( IF (OR X==1 X==2) Y==2 Y==3 ))
            // this returns x==2, y==2 and nothing else
            input: "(call/empty-state (call/fresh (lambda (x) (call/fresh (lambda (y) (conj (equalo x 2) (ifthenelse (disj (equalo x 1) (equalo x 2)) (equalo y 2) (equalo y 3))))))))",
            want:  "(((((var . 1) . 2) ((var . 0) . 2)) . 2))",
        },
        {
            // works with disj+ macro, which wraps inverse-eta-delay around all its goals (making them immature to start)
            input: "(call/empty-state (call/fresh (lambda (x) (call/fresh (lambda (y) (conj (equalo x 2) (ifthenelse (disj+ (equalo x 1) (equalo x 2)) (equalo y 2) (equalo y 3))))))))",
            want:  "(((((var . 1) . 2) ((var . 0) . 2)) . 2))",
        },
        {
            // turns out that was almost (but not quite) correct. Heres ifte from
            // microKanren: A Lucid Little Logic Language with a Simple Complete Search
            // ifte + once together constitute Prolog's cut operator, definitely impure (example?)
            // note how loop is needed to properly work with infinite streams
            // lots of extra overhead for not having some syntactic sugar but essence is the same
            input: `(define ifte (lambda (g0 g1 g2)
                      (lambda (s/c)
                        (begin
                        (define loop (lambda (s)
                          (cond
                            [(null? s) (g2 s/c)]
                            [(procedure? s) (lambda () (loop (s)))]
                            [else (bind g1 s)])))
                        (loop (g0 s/c))))))`,
        },
        {
            // using ifte
            input: "(call/empty-state (call/fresh (lambda (x) (call/fresh (lambda (y) (conj (equalo x 2) (ifte (equalo x 1) (equalo y 2) (equalo y 3))))))))",
            want:  "(((((var . 1) . 3) ((var . 0) . 2)) . 2))",
        },
        {
            // and here is once/1. ifte(once(g0), g1, g2) is equivalent to cut
            input: `(define once (lambda (g)
                      (lambda (s/c)
                        (begin
                        (define loop (lambda (s)
                          (cond
                            [(null? s) mzero]
                            [(procedure? s) (lambda () (loop (s)))]
                            [else (list (car s))])))
                        (loop (g s/c))))))`,
        },
        {
            input: "(call/empty-state (call/fresh (lambda (x) (once (disj (equalo x 1) (equalo x 2))))))",
            want:  "(((((var . 0) . 1)) . 1))",
        },
        // TODO: showcase ifte being impure
        // I want to get to pure ifte but would need disequality constraints first
        // Neumerkel/Kral (2016) - Indexing dif/2
	} {
		p := parse(tt.input)
		e := evalEnv(env, p)
		got := e.String()
		if got != tt.want {
			t.Errorf("%d) got %s want %s", i, got, tt.want)
		}
	}
}

func TestKanrenDCG(t *testing.T) {
	env := GlobalEnv()
	loadKanren(env)
	for i, tt := range []struct {
		input string
		want  string
	}{
        {
            input: "(define conso (lambda (a b l) (equalo (cons a b) l)))",
        },
        {
            input: `(define diflist (lambda (l x dl)
                      (conde
                        [(equalo (quote ()) l) (equalo x dl)]
                        [(fresh (a d ddl)
                           (conso a d l)
                           (diflist d x ddl)
                           (conso a ddl dl))])))`,
        },
        {
            input: `(define appendo (lambda (a b c d e f)
                      (conj+ (equalo a e) (equalo b c) (equalo d f))))`,
        },
        {
            input: `(run* (fresh (q a x b y c z)
                            (diflist (list 1 2) x a)
                            (diflist (list 3 4) y b)
                            (equalo z (quote ()))
                            (equalo c q)
                            (appendo a x b y c z)))`,
            want: "((1 2 3 4))",
        },
        {
            // diverges at (run 6) or (run*)
            input: `(run 5 (fresh (q p a x b y c z)
                            (diflist q x a)
                            (diflist p y b)
                            (diflist (list 1 2 3 4) z c)
                            (equalo z (quote ()))
                            (appendo a x b y c z)))`,
            want: "(() (1) (1 2) (1 2 3) (1 2 3 4))",
        },
        // TODO: build a --> macro or equivalent to play around with macros?
    } {
		p := parse(tt.input)
		e := evalEnv(env, p)
		got := e.String()
		if got != tt.want {
			t.Errorf("%d) got %s want %s", i, got, tt.want)
		}
	}
}
