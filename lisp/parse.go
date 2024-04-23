package lisp

import (
	"fmt"
	"os"
	"strconv"
	"strings"
	"unicode"
	"unicode/utf8"
)

// TODO: read from stream of input, maybe eval as we parse valid sexpressions
// For now, we just slurp in the entire file and return a list of expressions
func ParseFile(filename string) ([]SExpression, error) {
	b, err := os.ReadFile(filename)
	if err != nil {
		return nil, err
	}
	return Multiparse(string(b))
}

func Multiparse(file string) ([]SExpression, error) {
	return multiparse(file)
}

func mustParse(program string) SExpression {
	p, err := parse(program)
	if err != nil {
		panic(err)
	}
	return p
}

type parsed struct {
	sexp    SExpression
	special parseConst
}

type parseConst uint8

const (
	none parseConst = iota
	bracket
	quote
	quasiquote
	unquote
)

func parse(program string) (SExpression, error) {
	list, err := multiparse(program)
	if err != nil {
		return nil, err
	}
	return list[0], nil
}

func multiparse(program string) ([]SExpression, error) {
	stack := []parsed{}
	for len(program) > 0 {
		token, p, err := nextToken(program)
		if err != nil {
			return nil, err
		}
		program = p
		// TODO: can we change this switch to a map and include reader macros that way?
		switch token {
		case "(", "[":
			stack = append(stack, parsed{special: bracket})
		case ")", "]":
			s, err := simplifyStack(stack)
			if err != nil {
				return nil, err
			}
			stack = s
		case "'":
			stack = append(stack, parsed{special: quote})
		case "`":
			stack = append(stack, parsed{special: quasiquote})
		case ",":
			stack = append(stack, parsed{special: unquote})
		case `"`:
			var s []byte
			for len(program) > 0 {
				r, size := utf8.DecodeRuneInString(program)
				if size == 0 {
					return nil, fmt.Errorf(`unclosed string quote '"'`)
				}
				program = program[size:]
				if r == '"' {
					break
				}
				if r == '\\' {
					next, n := utf8.DecodeRuneInString(program)
					if next == '"' {
						program = program[n:]
						s = utf8.AppendRune(s, next)
						continue
					}
				}
				s = utf8.AppendRune(s, r)
			}
			stack = append(stack, parsed{sexp: NewPrimitive(string(s))})
		default:
			e := atom(token)
			if len(stack) > 0 {
				p := stack[len(stack)-1]
				switch p.special {
				case quote:
					e = list2cons(NewSymbol("quote"), e)
					stack = stack[:len(stack)-1]
				case quasiquote:
					e = list2cons(NewSymbol("quasiquote"), e)
					stack = stack[:len(stack)-1]
				case unquote:
					e = list2cons(NewSymbol("unquote"), e)
					stack = stack[:len(stack)-1]
				}
			}
			stack = append(stack, parsed{sexp: e})
		}
	}

	list := []SExpression{}
	for _, p := range stack {
		if p.sexp == nil {
			return nil, fmt.Errorf("syntax error")
		}
		list = append(list, p.sexp)
	}

	return list, nil
}

func nextToken(program string) (string, string, error) {
	program = strings.TrimSpace(program)
	// multiline comment: read until |# and ignore
	for strings.HasPrefix(program, "#|") {
		_, rest, found := strings.Cut(program, "|#")
		if !found {
			return "", "", fmt.Errorf("missing matching comment end |#")
		}
		program = rest
		program = strings.TrimSpace(program)
	}

	var token []byte

	for len(program) > 0 {
		r, size := utf8.DecodeRuneInString(program)
		if strings.ContainsRune("()[]'\",`", r) {
			if len(token) == 0 {
				return string(r), program[size:], nil
			}
			return string(token), program, nil
		}
		if unicode.IsSpace(r) {
			break
		}
		program = program[size:]
		token = utf8.AppendRune(token, r)
	}
	return string(token), program, nil
}

func atom(token string) SExpression {
	if n, err := strconv.ParseFloat(token, 64); err == nil {
		return NewPrimitive(n)
	}
	return NewSymbol(token)
}

// we just consumed a closing bracket, find matching opening bracket from top of stack
// optionally modify with (quasi/un)quote (TODO: recursive)
// push the resulting Pair back on the stack
func simplifyStack(stack []parsed) ([]parsed, error) {
	list := []SExpression{}
	for len(stack) > 0 {
		p := stack[len(stack)-1]
		stack = stack[:len(stack)-1]
		switch p.special {
		case bracket:
			rev := make([]SExpression, len(list))
			for i, v := range list {
				rev[len(list)-1-i] = v
			}
			if err := syntaxCheck(rev); err != nil {
				return nil, err
			}
			e := list2cons(rev...)
			if len(stack) > 0 {
				pp := stack[len(stack)-1]
				switch pp.special {
				case quote:
					e = list2cons(NewSymbol("quote"), e)
					stack = stack[:len(stack)-1]
				case quasiquote:
					e = list2cons(NewSymbol("quasiquote"), e)
					stack = stack[:len(stack)-1]
				case unquote:
					e = list2cons(NewSymbol("unquote"), e)
					stack = stack[:len(stack)-1]
				}
			}
			stack = append(stack, parsed{sexp: e})
			return stack, nil
		}
		list = append(list, p.sexp)
	}
	return nil, fmt.Errorf("unexpected ')'")
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
			return syntaxError(list)
		}
	case "begin":
		if len(list) == 1 {
			return syntaxError(list)
		}
	case "quote":
		if len(list) != 2 {
			return syntaxError(list)
		}
	case "define":
		if len(list) != 3 {
			return syntaxError(list)
		}
		if !list[1].IsSymbol() {
			return syntaxError(list)
		}
	case "define-syntax":
		if len(list) != 3 {
			return syntaxError(list)
		}
		if !list[1].IsSymbol() {
			return syntaxError(list)
		}
	case "syntax-rules":
		if !list[1].IsPair() {
			return syntaxError(list)
		}
		for _, e := range list[2:] {
			if !e.IsPair() {
				return syntaxError(list)
			}
			p := cons2list(e.AsPair())
			if len(p) != 2 {
				return syntaxError(list)
			}
		}
	case "lambda":
		if len(list) != 3 {
			return syntaxError(list)
		}
		if !list[1].IsPair() {
			return syntaxError(list)
		}
	}
	return nil
}

func syntaxError(list []SExpression) error {
	return fmt.Errorf("invalid syntax %s", list2cons(list...))
}
