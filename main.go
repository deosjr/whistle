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
        "(define empty-state (cons (quote ()) 0))",
        "((call/fresh (lambda (q) (equalo q 5))) empty-state)",
        "(define fives (lambda (x) (disj (equalo x 5) (zzz (fives x)))))",
        "(define sixes (lambda (x) (disj (equalo x 6) (zzz (sixes x)))))",
        "(define fives-and-sixes (call/fresh (lambda (x) (disj (fives x) (sixes x)))))",
        "(take 4 (fives-and-sixes empty-state))",
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
        conslist := list2cons(list)
        return ExpOrProc{isExp: true, value: Exp{isPair: true, value: conslist}}, tokens[1:]
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
        if e.isPair {
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
        return atomWithValue( e.isPair && e.pair() != empty )
    }},
    "car": ExpOrProc{value: func(args []ExpOrProc) ExpOrProc {
        return args[0].exp().pair().car()
    }},
    "cdr": ExpOrProc{value: func(args []ExpOrProc) ExpOrProc {
        return args[0].exp().pair().cdr()
    }},
    "cons": ExpOrProc{value: func(args []ExpOrProc) ExpOrProc {
        return ExpOrProc{isExp: true, value: Exp{isPair:true, value: newPair(args[0], args[1])}}
    }},
    "null?": ExpOrProc{value: func(args []ExpOrProc) ExpOrProc {
        x := args[0]
        if !x.isExp {
            return atomWithValue(false)
        }
        e := x.exp()
        if !e.isPair {
            return atomWithValue(false)
        }
        return atomWithValue( e.pair() == empty )
    }},
    "procedure?": ExpOrProc{value: func(args []ExpOrProc) ExpOrProc {
        return atomWithValue( !args[0].isExp )
    }},
    }, outer: nil}
}

func newEnv(params Pair, args []ExpOrProc, outer Env) Env {
    m := map[Symbol]ExpOrProc{}
    i := 0
    for params != empty {
        m[params.car().exp().atom().symbol()] = args[i]
        params = params.cdr().exp().pair()
        i++
    }
    return Env{dict: m, outer: &outer}
}

func expandMacro(p Pair) Pair {
    if !isAtom(p.car()) {
        return p
    }
    s := p.car().exp().atom().symbol()
    switch s {
    case "cond":
        expanded := atomWithValue(false)
        clauses := cons2list(p.cdr().exp().pair())
        for i := len(clauses)-1; i>=0; i-- {
            clause := clauses[i].exp().pair()
            cond := clause.car()
            if isAtom(cond) {
                if cond.exp().atom().symbol() != "else" {
                    panic("expected else")
                }
                if i != len(clauses)-1 {
                    panic("else is not last in cond")
                }
                expanded = clause.cadr()
                continue
            }
            begin := []ExpOrProc{ {isExp: true, value: Exp{value: Atom{isSymbol: true, value: "begin"}}} }
            clause = clause.cdr().exp().pair()
            for clause != empty {
                begin = append(begin, clause.car())
                clause = clause.cdr().exp().pair()
            }
            expanded = ExpOrProc{isExp: true, value: Exp{isPair: true, value: list2cons([]ExpOrProc{
                ExpOrProc{isExp: true, value: Exp{value: Atom{isSymbol: true, value: "if"}}},
                cond,
                ExpOrProc{isExp: true, value: Exp{isPair: true, value: list2cons(begin) }},
                expanded,
            })}}
        }
        return expanded.exp().pair()
    case "let":
        bindings := cons2list(p.cadr().exp().pair())
        body := p.caddr()
        vars := make([]ExpOrProc, len(bindings))
        exps := make([]ExpOrProc, len(bindings))
        for i, b := range bindings {
            bl := b.exp().pair() // list of len 2
            vars[i] = bl.car()
            exps[i] = bl.cadr()
        }
        lambda := ExpOrProc{isExp: true, value: Exp{isPair: true, value: list2cons([]ExpOrProc{
            ExpOrProc{isExp: true, value: Exp{value: Atom{isSymbol: true, value: "lambda"}}},
            ExpOrProc{isExp: true, value: Exp{isPair: true, value: list2cons(vars)}},
            body,
        })}}
        return list2cons(append([]ExpOrProc{lambda}, exps...))
    case "zzz":
        goal := p.cadr()
        sc := ExpOrProc{isExp: true, value: Exp{value: Atom{isSymbol: true, value: "s/c"}}}
        lambda := ExpOrProc{isExp: true, value: Exp{isPair: true, value: list2cons([]ExpOrProc{
            ExpOrProc{isExp: true, value: Exp{value: Atom{isSymbol: true, value: "lambda"}}},
            ExpOrProc{isExp: true, value: Exp{isPair: true, value: empty}},
            ExpOrProc{isExp: true, value: Exp{isPair: true, value: list2cons([]ExpOrProc{goal, sc})}},
        })}}
        return list2cons([]ExpOrProc{
            ExpOrProc{isExp: true, value: Exp{value: Atom{isSymbol: true, value: "lambda"}}},
            ExpOrProc{isExp: true, value: Exp{isPair: true, value: list2cons([]ExpOrProc{sc})}},
            lambda,
        })
    default:
        return p
    }
}

func eval(e Exp) Exp {
    eop := ExpOrProc{isExp: true, value: e}
    return evalEnv(eop, GlobalEnv()).exp()
}

func evalEnv(x ExpOrProc, env Env) ExpOrProc {
    if x.isExp {
        e := x.exp()
        if !e.isPair {
            a := e.atom()
            if a.isSymbol {
                return env.find(a.symbol()).dict[a.symbol()]
            }
            // number
            return x
        }
        // list
        p := e.pair()
        p = expandMacro(p)
        car := p.car()
        if !car.exp().isPair {
            s := car.exp().atom().symbol()
            switch s {
            case "if":
                test := p.cadr()
                conseq := p.caddr()
                tested := evalEnv(test, env)
                if isTruthy(tested) {
                    return evalEnv(conseq, env)
                }
                if p.cdddr().exp().pair() == empty {
                    return ExpOrProc{isExp:true, value: Exp{value:Atom{value: false}}}
                }
                alt := p.cadddr()
                return evalEnv(alt, env)
            case "define":
                sym := p.cadr().exp().atom().symbol()
                exp := p.caddr()
                env.dict[sym] = evalEnv(exp, env)
                return ExpOrProc{isExp:true, value: Exp{value:Atom{value: false}}}
            case "quote":
                return p.cadr()
            case "lambda":
                params := p.cadr().exp().pair()
                body := p.caddr()
                return ExpOrProc{value: func(args []ExpOrProc) ExpOrProc {
                    return evalEnv(body, newEnv(params, args, env))
                }}
            // default: falls through to procedure call
            }
        }
        // procedure call
        proc := evalEnv(car, env).proc()
        args := []ExpOrProc{}
        cdr := p.cdr().exp().pair()
        for cdr != empty {
            args = append(args, evalEnv(cdr.car(), env))
            cdr = cdr.cdr().exp().pair()
        }
        return proc(args)
    }
    // proc
    return x // TODO
}
