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
    loadKanren(env)
    for _, s := range []string{
        "(define empty-state (quote (() 0)))",
        "((call/fresh (lambda (q) (equalo q 5))) empty-state)",
    }{
        t := parse(s)
        fmt.Println(t)
        e := evalEnv(t, env)
        if e.String() != "" {
            fmt.Println(e)
        }
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
    "=": ExpOrProc{value: func(args []ExpOrProc) ExpOrProc {
        return atomWithValue( number(args[0]) == number(args[1]) )
    }},
    ">": ExpOrProc{value: func(args []ExpOrProc) ExpOrProc {
        return atomWithValue( number(args[0]) > number(args[1]) )
    }},
    "<=": ExpOrProc{value: func(args []ExpOrProc) ExpOrProc {
        return atomWithValue( number(args[0]) <= number(args[1]) )
    }},
    "#t": atomWithValue(true),
    "#f": atomWithValue(false),
    "and": ExpOrProc{value: func(args []ExpOrProc) ExpOrProc {
        and := true
        for _, a := range args {
            and = and && boolean(a)
            if !and {
                break
            }
        }
        return atomWithValue(and)
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
    "pair?": ExpOrProc{value: func(args []ExpOrProc) ExpOrProc {
        x := args[0]
        if !x.isExp {
            return atomWithValue(false)
        }
        e := x.exp()
        if !e.isList {
            return atomWithValue(false)
        }
        l := e.list()
        return atomWithValue( len(l) == 2 )
    }},
    "car": ExpOrProc{value: func(args []ExpOrProc) ExpOrProc {
        return args[0].exp().list()[0]
    }},
    "cdr": ExpOrProc{value: func(args []ExpOrProc) ExpOrProc {
        l := args[0].exp().list()
        // TODO: cdr on a pair returns value, not list
        return ExpOrProc{isExp: true, value: Exp{isList:true, value: l[1:]}}
    }},
    // TODO: remove
    "paircdr": ExpOrProc{value: func(args []ExpOrProc) ExpOrProc {
        l := args[0].exp().list()
        return l[1]
    }},
    "cons": ExpOrProc{value: func(args []ExpOrProc) ExpOrProc {
        var value []ExpOrProc
        if args[1].isExp && args[1].exp().isList {
            value = append([]ExpOrProc{args[0]}, args[1].exp().list()...)
        } else {
            value = []ExpOrProc{args[0], args[1]}
        }
        return ExpOrProc{isExp: true, value: Exp{isList:true, value: value}}
    }},
    "null?": ExpOrProc{value: func(args []ExpOrProc) ExpOrProc {
        x := args[0]
        if !x.isExp {
            return atomWithValue(false)
        }
        e := x.exp()
        if !e.isList {
            return atomWithValue(false)
        }
        l := e.list()
        return atomWithValue( len(l) == 0 )
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

func expandMacro(l []ExpOrProc) []ExpOrProc {
    if !isAtom(l[0]) {
        return l
    }
    s := l[0].exp().atom().symbol()
    switch s {
    case "cond":
        expanded := atomWithValue(false)
        clauses := l[1:]
        for i := len(clauses)-1; i>=0; i-- {
            clause := clauses[i].exp().list()
            cond := clause[0]
            if isAtom(cond) {
                if cond.exp().atom().symbol() != "else" {
                    panic("expected else")
                }
                if i != len(clauses)-1 {
                    panic("else is not last in cond")
                }
                expanded = clause[1]
                continue
            }
            begin := []ExpOrProc{ {isExp: true, value: Exp{value: Atom{isSymbol: true, value: "begin"}}} }
            for _, c := range clause[1:] {
                begin = append(begin, c)
            }
            expanded = ExpOrProc{isExp: true, value: Exp{isList: true, value: []ExpOrProc{
                ExpOrProc{isExp: true, value: Exp{value: Atom{isSymbol: true, value: "if"}}},
                cond,
                ExpOrProc{isExp: true, value: Exp{isList: true, value: begin }},
                expanded,
            }}}
        }
        return expanded.exp().list()
    case "let":
        bindings := l[1].exp().list()
        body := l[2]
        vars := make([]ExpOrProc, len(bindings))
        exps := make([]ExpOrProc, len(bindings))
        for i, b := range bindings {
            bl := b.exp().list() // of len 2
            vars[i] = bl[0]
            exps[i] = bl[1]
        }
        lambda := ExpOrProc{isExp: true, value: Exp{isList: true, value: []ExpOrProc{
            ExpOrProc{isExp: true, value: Exp{value: Atom{isSymbol: true, value: "lambda"}}},
            ExpOrProc{isExp: true, value: Exp{isList: true, value: vars}},
            body,
        }}}
        return append([]ExpOrProc{lambda}, exps...)
    default:
        return l
    }
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
        l = expandMacro(l)
        first := l[0].exp()
        if !first.isList {
            s := l[0].exp().atom().symbol()
            switch s {
            case "if":
                test := l[1]
                conseq := l[2]
                tested := evalEnv(test, env)
                if isTruthy(tested) {
                //if tested.exp().atom().value.(bool) {
                    return evalEnv(conseq, env)
                }
                if len(l) == 3 {
                    return ExpOrProc{isExp:true, value: Exp{value:Atom{value: false}}}
                }
                alt := l[3]
                return evalEnv(alt, env)
            case "define":
                sym := l[1].exp().atom().symbol()
                exp := l[2]
                env.dict[sym] = evalEnv(exp, env)
                return ExpOrProc{isExp:true, value: Exp{value:Atom{value: false}}}
            case "quote":
                return l[1]
            case "lambda":
                params := l[1]
                body := l[2]
                return ExpOrProc{value: func(args []ExpOrProc) ExpOrProc {
                    return evalEnv(body, newEnv(params, args, env))
                }}
            // default: falls through to procedure call
            }
        }
        // procedure call
        proc := evalEnv(l[0], env).proc()
        args := make([]ExpOrProc, len(l)-1)
        for i:=0; i < len(args); i++ {
            args[i] = evalEnv(l[i+1], env)
        }
        return proc(args)
    }
    // proc
    return x // TODO
}
