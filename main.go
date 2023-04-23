package main

// http://norvig.com/lispy.html

import (
	"fmt"
	"strconv"
	"strings"
)

func main() {
    p, env := New()
    // TODO: reintroduces extra newline after define
    p.Eval(env, `(define REPL (lambda (env)
        (begin (display "> ")
               (display (eval (read) env))
               (display newline)
               (REPL env))))`)
    // NOTE: injecting the env through closure/func argument
    // shows that spawn is still full of env sharing bugs!
    // DANGEROUS sharing of state, but will work :)
    // REPL is now restarted on error with env intact
    p.Eval(env, `(define restarter (lambda (env)
        (begin (process_flag 'trap_exit #t)
               (let ((pid (spawn_link (lambda () (REPL env)) (quote ()))))
                    (receive
                        ((reason) (quasiquote (EXIT ,pid ,reason)) ->
                            (if (eqv? reason "normal") #t
                            (begin (display "** exception error: ") (display reason) (display newline) (restarter env)))))))))`)
    p.Eval(env, "(restarter (environment))")
}

func New() (*process, *Env) {
    p := newProcess()
	env := GlobalEnv()
    loadKanren(p, env)
    loadErlang(p, env)
    return p, env
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
	p, _, err := readFromTokens(s)
	return p, err
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

func readFromTokens(tokens []string) (SExpression, []string, error) {
	if len(tokens) == 0 {
        return nil, nil, fmt.Errorf("syntax error")
	}
	token := tokens[0]
	tokens = tokens[1:]
	switch token {
	case "(":
		list := []SExpression{}
		for tokens[0] != ")" {
			parsed, t, err := readFromTokens(tokens)
            if err != nil {
                return nil, nil, err
            }
			if len(t) == 0 {
                return nil, nil, fmt.Errorf("syntax error")
			}
			tokens = t
			list = append(list, parsed)
		}
        if err := syntaxCheck(list); err != nil {
            return nil, nil, err
        }
		return list2cons(list...), tokens[1:], nil
	case ")":
        return nil, nil, fmt.Errorf("unexpected ')'")
	default:
		return atom(token), tokens, nil
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
        unquote, _, _ := readFromTokens([]string{"(", "unquote", token[1:], ")"})
        return unquote
    }
    // TODO quote syntax only works on symbols, not lists atm!
    if token[0] == '\'' {
        quote, _, _ := readFromTokens([]string{"(", "quote", token[1:], ")"})
        return quote
    }
	return NewSymbol(token)
}

// check syntactic form of some builtins
// so we don't encounter weirdness at runtime
// TODO: how does this work with macros? -> currently it doesn't
// would take applying macros actually at read time to make this work
func syntaxCheck(list []SExpression) error {
	if len(list) == 0 {
		return nil
	}
	if !list[0].IsSymbol() {
		return nil
	}
	switch list[0].AsSymbol() {
	case "if":
		if len(list) != 3 && len(list) != 4 {
			return fmt.Errorf("invalid syntax %s", list2cons(list...))
		}
	case "begin":
		if len(list) == 1 {
			return fmt.Errorf("invalid syntax %s", list2cons(list...))
		}
	case "quote":
		if len(list) != 2 {
			return fmt.Errorf("invalid syntax %s", list2cons(list...))
		}
	case "define":
		if len(list) != 3 {
			return fmt.Errorf("invalid syntax %s", list2cons(list...))
		}
		if !list[1].IsSymbol() {
			return fmt.Errorf("invalid syntax %s", list2cons(list...))
		}
	case "define-syntax":
		if len(list) != 3 {
			return fmt.Errorf("invalid syntax %s", list2cons(list...))
		}
		if !list[1].IsSymbol() {
			return fmt.Errorf("invalid syntax %s", list2cons(list...))
		}
	case "syntax-rules":
		if !list[1].IsPair() {
			return fmt.Errorf("invalid syntax %s", list2cons(list...))
		}
        for _, e := range list[2:] {
            if !e.IsPair() {
			    return fmt.Errorf("invalid syntax %s", list2cons(list...))
            }
            p := cons2list(e.AsPair())
            if len(p) != 2 {
			    return fmt.Errorf("invalid syntax %s", list2cons(list...))
            }
        }
	case "lambda":
		if len(list) != 3 {
			return fmt.Errorf("invalid syntax %s", list2cons(list...))
		}
		if !list[1].IsPair() {
			return fmt.Errorf("invalid syntax %s", list2cons(list...))
		}
	}
    return nil
}
