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
            input: "(define empty-state (quote (() 0)))",
        },
        {
            input: "((call/fresh (lambda (q) (equalo q 5))) empty-state)",
            want:  "[[[[0 5]] 1]]",
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
