package main

import (
    "testing"
)

func TestLisp(t *testing.T) {
    // NOTE: one shared global env for test, meaning order matters here!
    env := GlobalEnv()
    for i, tt := range []struct{
        input string
        want string
    }{
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
            want:  "0",
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
            input: "(if (number? (quote ())) 4 5)",
            want:  "5",
        },
        {
            input: "(car (quote (1 2 3)))",
            want:  "1",
        },
        {
            input: "(cdr (quote (1 2 3)))",
            want:  "[2 3]",
        },
        {
            input: "(define fact (lambda (n) (if (<= n 1) 1 (* n (fact (- n 1))))))",
            want: "0",
        },
        {
            input: "(fact 10)",
            want:  "3628800",
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
