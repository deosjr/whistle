package lisp

import (
	"testing"
)

func TestCallCC(t *testing.T) {
	main := New()
	main.process.evalWithContinuation = true
	for i, tt := range []struct {
		input string
		want  string
	}{
		{
			input: "(define f (lambda (return) (begin (return 2) 3)))",
		},
		{
			input: "(f (lambda (x) x))",
			want:  "3",
		},
		{
			input: "(call/cc f)",
			want:  "2",
		},
		{
			input: "(+ 1 (call/cc (lambda (cc) (+ 20 300))))",
			want:  "321",
		},
		{
			input: "(+ 1 (call/cc (lambda (cc) (+ 20 (cc 300)))))",
			want:  "301",
		},
	} {
		t.Log(tt.input)
		e, err := main.Eval(tt.input)
		if err != nil {
			t.Errorf("%d) parse error %v", i, err)
		}
		if e == nil && tt.want == "" {
			continue
		}
		got := e.String()
		if got != tt.want {
			t.Errorf("%d) got %s want %s", i, got, tt.want)
		}
	}
}

func TestCPS(t *testing.T) {
	main := New()
	main.process.evalWithContinuation = true
	for i, tt := range []struct {
		input string
		want  string
	}{
		{
			input: `(define *& (lambda (x y k) 
                (k (* x y)))) `,
		},
		{
			input: "(*& 6 7 (lambda (x) x))",
			want:  "42",
		},
		// copied from lisp_test.go
		{
			input: "(begin (define r 10) (* pi (* r r)))",
			want:  "314.1592653589793",
		},
		{
			input: "(if (> (* 11 11) 120) (* 7 6) oops)",
			want:  "42",
		},
		{
			input: "(define circle-area (lambda (r) (* pi (* r r))))",
		},
		{
			input: "(circle-area 3)",
			want:  "28.274333882308138",
		},
		{
			input: "(quote quoted)",
			want:  "quoted",
		},
		{
			input: "'quoted",
			want:  "quoted",
		},
		{
			input: "(if (number? (quote ())) 4 5)",
			want:  "5",
		},
		{
			input: "(car (quote (1 2 3)))",
			want:  "1",
		},
		{
			input: "(cdr (quote (1 2 3)))",
			want:  "(2 3)",
		},
		{
			input: `(define fact 
            (lambda (n) 
                (if (<= n 1) 1 (* n (fact (- n 1))))))`,
		},
		{
			input: "(fact 10)",
			want:  "3628800",
		},
		{
			input: "(define twice (lambda (x) (* 2 x)))",
		},
		{
			input: "(twice 5)",
			want:  "10",
		},
		{
			input: "(define repeat (lambda (f) (lambda (x) (f (f x)))))",
		},
		{
			input: "((repeat twice) 10)",
			want:  "40",
		},
		{
			input: "((repeat (repeat twice)) 10)",
			want:  "160",
		},
		{
			input: "((repeat (repeat (repeat twice))) 10)",
			want:  "2560",
		},
		{
			input: "((repeat (repeat (repeat (repeat twice)))) 10)",
			want:  "655360",
		},
		{
			input: `((lambda (a b) (cond ((= a 4) 6)
                          ((= b 4) (+ 6 7))
                          (else 25))) 1 4)`,
			want: "13",
		},
		{
			input: "(define x 5)",
		},
		{
			input: "(+ (let ((x 3)) (+ x (* x 10))) x)",
			want:  "38",
		},
		{
			input: `(define find (lambda (proc list) 
            (let ((x (car list)))
            (cond
                ((proc x) x)
                (else (find proc (cdr list)))
            ))))`,
		},
		{
			input: "(find (lambda (x) (= x 2)) (quote (1 2)))",
			want:  "2",
		},
		{
			input: "(cons 1 (quote (2 3)))",
			want:  "(1 2 3)",
		},
		{
			input: "(cons 1 2)",
			want:  "(1 . 2)",
		},
		{
			input: "(cons 1 (cons 1 2))",
			want:  "(1 1 . 2)",
		},
	} {
		e, err := main.Eval(tt.input)
		if err != nil {
			t.Errorf("%d) parse error %v", i, err)
		}
		if e == nil && tt.want == "" {
			continue
		}
		got := e.String()
		if got != tt.want {
			t.Errorf("%d) got %s want %s", i, got, tt.want)
		}
	}
}
