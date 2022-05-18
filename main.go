package main

// http://norvig.com/lispy.html

import (
    "fmt"
    "math"
    "strconv"
    "strings"
)

func main() {
    env := GlobalEnv()
    for _, s := range []string{
        "(begin (define r 10) (* pi (* r r)))",
        "(if (> (* 11 11) 120) (* 7 6) oops)",
        "(define circle-area (lambda (r) (* pi (* r r))))",
        "(circle-area 3)",
        "(quote quoted)",
        "(if (number? (quote ())) 4 5)",
        "(car (quote (1 2 3)))",
        "(cdr (quote (1 2 3)))",
    }{
        t := parse(s)
        fmt.Println(t)
        e := evalEnv(t, env)
        fmt.Println(e)
    }
}

func parse(program string) ExpOrProc {
    s := tokenize(program)
    p, _ := readFromTokens(s)
    return p
}

func tokenize(s string) []string {
    s = strings.ReplaceAll(s, "(", " ( ")
    s = strings.ReplaceAll(s, ")", " ) ")
    return strings.Fields(s)
}

func readFromTokens(tokens []string) (ExpOrProc, []string) {
    if len(tokens) == 0 {
        panic("syntax error")
    }
    token := tokens[0]
    tokens = tokens[1:]
    switch token {
    case "(":
        list := []ExpOrProc{}
        for tokens[0] != ")" {
            parsed, t := readFromTokens(tokens)
            tokens = t
            list = append(list, parsed)
        }
        return ExpOrProc{isExp: true, value: Exp{isList: true, value: list}}, tokens[1:]
    case ")":
        panic("unexpected ')'")
    default:
        return ExpOrProc{isExp: true, value: Exp{value: atom(token)}}, tokens
    }
}

func atom(token string) Atom {
    if n, err := strconv.ParseFloat(token, 64); err == nil {
        return Atom{value: n}
    }
    return Atom{isSymbol: true, value: token}
}

func GlobalEnv() Env {
    return Env{ dict: map[Symbol]ExpOrProc{
    "*": ExpOrProc{value: func(args []ExpOrProc) ExpOrProc {
        return atomWithValue( number(args[0]) * number(args[1]) )
    }},
    "+": ExpOrProc{value: func(args []ExpOrProc) ExpOrProc {
        return atomWithValue( number(args[0]) + number(args[1]) )
    }},
    "-": ExpOrProc{value: func(args []ExpOrProc) ExpOrProc {
        return atomWithValue( number(args[0]) - number(args[1]) )
    }},
    ">": ExpOrProc{value: func(args []ExpOrProc) ExpOrProc {
        return atomWithValue( number(args[0]) > number(args[1]) )
    }},
    "<=": ExpOrProc{value: func(args []ExpOrProc) ExpOrProc {
        return atomWithValue( number(args[0]) <= number(args[1]) )
    }},
    "pi": atomWithValue( math.Pi ),
    "begin": ExpOrProc{value: func(args []ExpOrProc) ExpOrProc { return args[len(args)-1] }},
    "number?": ExpOrProc{value: func(args []ExpOrProc) ExpOrProc {
        x := args[0]
        if !x.isExp {
            return atomWithValue(false)
        }
        e := x.exp()
        if e.isList {
            return atomWithValue(false)
        }
        a := e.atom()
        return atomWithValue( !a.isSymbol )
    }},
    "car": ExpOrProc{value: func(args []ExpOrProc) ExpOrProc {
        return args[0].exp().list()[0]
    }},
    "cdr": ExpOrProc{value: func(args []ExpOrProc) ExpOrProc {
        return ExpOrProc{isExp: true, value: Exp{isList:true, value: args[0].exp().list()[1:]}}
    }},
    }, outer: nil}
}

func newEnv(params ExpOrProc, args []ExpOrProc, outer Env) Env {
    m := map[Symbol]ExpOrProc{}
    for i, p := range params.exp().list() {
        m[p.exp().atom().symbol()] = args[i]
    }
    return Env{dict: m, outer: &outer}
}

func eval(e Exp) Exp {
    eop := ExpOrProc{isExp: true, value: e}
    return evalEnv(eop, GlobalEnv()).exp()
}

func evalEnv(x ExpOrProc, env Env) ExpOrProc {
    if x.isExp {
        e := x.exp()
        if !e.isList {
            a := e.atom()
            if a.isSymbol {
                return env.find(a.symbol()).dict[a.symbol()]
            }
            // number
            return x
        }
        // list
        l := e.list()
        switch l[0].exp().atom().symbol() {
        case "if":
            test := l[1]
            conseq := l[2]
            alt := l[3]
            if evalEnv(test, env).exp().atom().value.(bool) {
                return evalEnv(conseq, env)
            }
            return evalEnv(alt, env)
        case "define":
            sym := l[1].exp().atom().symbol()
            exp := l[2]
            env.dict[sym] = evalEnv(exp, env)
            return ExpOrProc{isExp:true, value: Exp{value:Atom{value: 0.0}}}
        case "quote":
            return l[1]
        case "lambda":
            params := l[1]
            body := l[2]
            return ExpOrProc{value: func(args []ExpOrProc) ExpOrProc {
                return evalEnv(body, newEnv(params, args, env))
            }}
        default: // procedure call
            proc := evalEnv(l[0], env).proc()
            args := make([]ExpOrProc, len(l)-1)
            for i:=0; i < len(args); i++ {
                args[i] = evalEnv(l[i+1], env)
            }
            return proc(args)
        }
    }
    // proc
    return x // TODO
}
