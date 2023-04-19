package main

// http://norvig.com/lispy.html

import (
	"fmt"
	"strconv"
	"strings"
)

func main() {
    p := newProcess()
	env := GlobalEnv()
    loadKanren(p, env)
    loadErlang(p, env)
    // TODO: reintroduces extra newline after define
    repl := mustParse(`(define REPL (lambda (env)
        (begin (display "> ")
               (display (eval (read) env))
               (display newline)
               (REPL env))))`)
    p.evalEnv(env, repl)
    p.evalEnv(env, mustParse("(process_flag 'trap_exit #t)"))
    p.evalEnv(env, mustParse("(spawn_link (lambda () (REPL (environment))) (quote ()))"))
    // TODO: hack so that the REPL communicates env back to main
    // DANGEROUS sharing of state, but will work :)
    // TODO: restart the REPL with last-known env if REPL crashes
    e, _ := p.evalEnv(env, mustParse("(receive ((x) x -> x))"))
    reason := e.AsPair().caddr()
    if reason.String() != "normal" {
        fmt.Print("** exception error: ")
        display(p, env, []SExpression{reason})
        fmt.Println()
    }
}

func mustParse(program string) SExpression {
    p, err := parse(program)
    if err != nil {
        panic(err)
    }
    return p
}

func parse(program string) (SExpression, error) {
	s := tokenize(program)
	p, _ := readFromTokens(s)
	return p, nil // TODO!
}

func tokenize(s string) []string {
	s = strings.ReplaceAll(s, "[", "(")
	s = strings.ReplaceAll(s, "]", ")")
	s = strings.ReplaceAll(s, "(", " ( ")
	s = strings.ReplaceAll(s, ")", " ) ")
    tokenized := []string{}
    fields := strings.Fields(s)
    // pasting string escaped stuff back together..
    str := ""
    for i:=0; i<len(fields);i++ {
        ss := fields[i]
        if len(str) == 0 && strings.HasPrefix(ss, `"`) {
            if len(ss) > 1 && strings.HasSuffix(ss, `"`) {
                tokenized = append(tokenized, ss)
                continue
            }
            str = ss
            continue
        }
        if len(str) > 0 {
            str += " " + ss
            if strings.HasSuffix(ss, `"`) {
                tokenized = append(tokenized, str)
                str = ""
            }
            continue
        }
        tokenized = append(tokenized, ss)
    }
	return tokenized
}

func readFromTokens(tokens []string) (SExpression, []string) {
	if len(tokens) == 0 {
		panic("syntax error")
	}
	token := tokens[0]
	tokens = tokens[1:]
	switch token {
	case "(":
		list := []SExpression{}
		for tokens[0] != ")" {
			parsed, t := readFromTokens(tokens)
			if len(t) == 0 {
				panic("syntax error")
			}
			tokens = t
			list = append(list, parsed)
		}
		syntaxCheck(list)
		return list2cons(list...), tokens[1:]
	case ")":
		panic("unexpected ')'")
	default:
		return atom(token), tokens
	}
}

func atom(token string) SExpression {
	if n, err := strconv.ParseFloat(token, 64); err == nil {
		return NewPrimitive(n)
	}
	if token[0] == token[len(token)-1] && token[0] == '"' {
		return NewPrimitive(token[1 : len(token)-1])
	}
    // TODO unquote syntax only works on symbols, not lists atm!
    if token[0] == ',' {
        unquote, _ := readFromTokens([]string{"(", "unquote", token[1:], ")"})
        return unquote
    }
    // TODO quote syntax only works on symbols, not lists atm!
    if token[0] == '\'' {
        quote, _ := readFromTokens([]string{"(", "quote", token[1:], ")"})
        return quote
    }
	return NewSymbol(token)
}

// check syntactic form of some builtins
// so we don't encounter weirdness at runtime
// TODO: how does this work with macros? -> currently it doesn't
// would take applying macros actually at read time to make this work
func syntaxCheck(list []SExpression) {
	if len(list) == 0 {
		return
	}
	if !list[0].IsSymbol() {
		return
	}
	switch list[0].AsSymbol() {
	case "if":
		if len(list) != 3 && len(list) != 4 {
			panic(fmt.Sprintf("invalid syntax %s", list2cons(list...)))
		}
	case "begin":
		if len(list) == 1 {
			panic(fmt.Sprintf("invalid syntax %s", list2cons(list...)))
		}
	case "quote":
		if len(list) != 2 {
			panic(fmt.Sprintf("invalid syntax %s", list2cons(list...)))
		}
	case "define":
		if len(list) != 3 {
			panic(fmt.Sprintf("invalid syntax %s", list2cons(list...)))
		}
		if !list[1].IsSymbol() {
			panic(fmt.Sprintf("invalid syntax %s", list2cons(list...)))
		}
	case "define-syntax":
		if len(list) != 3 {
			panic(fmt.Sprintf("invalid syntax %s", list2cons(list...)))
		}
		if !list[1].IsSymbol() {
			panic(fmt.Sprintf("invalid syntax %s", list2cons(list...)))
		}
	case "syntax-rules":
		if !list[1].IsPair() {
			panic(fmt.Sprintf("invalid syntax %s", list2cons(list...)))
		}
        for _, e := range list[2:] {
            if !e.IsPair() {
			    panic(fmt.Sprintf("invalid syntax %s", list2cons(list...)))
            }
            p := cons2list(e.AsPair())
            if len(p) != 2 {
			    panic(fmt.Sprintf("invalid syntax %s", list2cons(list...)))
            }
        }
	case "lambda":
		if len(list) != 3 {
			panic(fmt.Sprintf("invalid syntax %s", list2cons(list...)))
		}
		if !list[1].IsPair() {
			panic(fmt.Sprintf("invalid syntax %s", list2cons(list...)))
		}
	}
}
