package main

import (
    "testing"
)

func TestKanren(t *testing.T) {
    env := GlobalEnv()
    loadKanren(env)
    for i, tt := range []struct{
        input string
        want string
    }{
        {
            // TODO: quote cannot parse pairs that are not lists!
            //input: "(define empty-state (quote (() 0)))",
            input: "(define empty-state (cons (quote ()) 0))",
        },
        {
            input: "((call/fresh (lambda (q) (equalo q 5))) empty-state)",
            want:  "((((0 . 5)) . 1))",
        },
        {
            input: `(define a-and-b
                      (conj
                        (call/fresh (lambda (a) (equalo a 7)))
                        (call/fresh (lambda (b) (disj (equalo b 5) (equalo b 6))))))`,
        },
        {
            input: "(a-and-b empty-state)",
            want:  "((((1 . 5) (0 . 7)) . 2) (((1 . 6) (0 . 7)) . 2))",
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
            input: "(take 4 (fives-and-sixes empty-state))",
            want:  "((((0 . 5)) . 1) (((0 . 6)) . 1) (((0 . 5)) . 1) (((0 . 6)) . 1))",
        },
    }{
        p := parse(tt.input)
        e := evalEnv(p, env)
        got := e.String()
        if got != tt.want {
            t.Errorf("%d) got %s want %s", i, got, tt.want)
        }
    }
}
