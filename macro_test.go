package main

import (
    "fmt"
    "reflect"
    "strings"
    "testing"
)

func reversePattern(revGensym map[Symbol]Symbol, p pattern) pattern {
    if p.isVariable {
        p.content = NewSymbol(revGensym[p.content.AsSymbol()])
        return p
    }
    if !p.isList {
        return p
    }
    for i, v := range p.listContent {
        p.listContent[i] = reversePattern(revGensym, v)
    }
    return p
}

func TestAnalysePattern(t *testing.T) {
	for i, tt := range []struct {
        literals []string
		pattern  string
		want     pattern
	} {
        {
            pattern: "a",
            want: pattern{isVariable: true, content: parse("a")},
        },
        {
            pattern: "(_ a b ...)",
            want: pattern{
                isList: true,
                listContent: []pattern{
                    {isUnderscore: true},
                    {isVariable: true, content: parse("a")},
                    {isVariable: true, hasEllipsis: true, content: parse("b")},
                },
            },
        },
        {
            pattern: "(_ (a b ...) ...)",
            want: pattern{
                isList: true,
                listContent: []pattern{
                    {isUnderscore: true},
                    {isList: true, listContent: []pattern{
                        {isVariable: true, content: parse("a")},
                        {isVariable: true, hasEllipsis: true, content: parse("b")}},
                    hasEllipsis: true},
                },
            },
        },
    } {
        sp := parse(tt.pattern)
        gensyms := map[Symbol]Symbol{}
        got := analysePattern(tt.literals, sp, gensyms)
        // gensyms contains a->hashA lookups. for this test, we can reverse those
        // and check against our wanted pattern which uses non-hashed var names
        revGensym := map[Symbol]Symbol{}
        for k, v := range gensyms {
            revGensym[v] = k
        }
        got = reversePattern(revGensym, got)
        if !reflect.DeepEqual(got, tt.want) {
            t.Errorf("%d) got %v want %v", i, got, tt.want)
        }
    }
}

func TestAnalyseTemplate(t *testing.T) {
	for i, tt := range []struct {
        template string 
        gensyms  map[Symbol]Symbol
		want     pattern
	} {
        {
            template: "a",
            gensyms: map[Symbol]Symbol{ "a": "hashA" },
            want: pattern{isVariable: true, content: parse("a")},
        },
	} {
        st := parse(tt.template)
        got := analyseTemplate(st, tt.gensyms)
        revGensym := map[Symbol]Symbol{}
        for k, v := range tt.gensyms {
            revGensym[v] = k
        }
        got = reversePattern(revGensym, got)
        if !reflect.DeepEqual(got, tt.want) {
            t.Errorf("%d) got %v want %v", i, got, tt.want)
        }
    }
}

func TestUnification(t *testing.T) {
	for i, tt := range []struct {
		pattern string
        input   string
		want    map[Symbol]SExpression
	} {
        {
            pattern: "x",
            input:   "42",
            want:    map[Symbol]SExpression{
                "x": parse("42"),
            },
        },
        {
            pattern: "(_ x)",
            input:   "(macro 42)",
            want:    map[Symbol]SExpression{
                "x": parse("42"),
            },
        },
        {
            pattern: "(_ a b ...)",
            input:   "(macro 42)",
            want:    map[Symbol]SExpression{
                "a": parse("42"),
            },
        },
        {
            pattern: "(_ a b ...)",
            input:   "(macro 1 2 3 4)",
            want:    map[Symbol]SExpression{
                "a":   parse("1"),
                "b#0": parse("2"),
                "b#1": parse("3"),
                "b#2": parse("4"),
            },
        },
        {
            pattern: "(_ (a b ...) ...)",
            input:   "(macro (1))",
            want:    map[Symbol]SExpression{
                "a#0": parse("1"),
            },
        },
        {
            pattern: "(_ (a b ...) ...)",
            input:   "(macro (1 2) (3 4))",
            want:    map[Symbol]SExpression{
                "a#0":   parse("1"),
                "b#0#0": parse("2"),
                "a#1":   parse("3"),
                "b#1#0": parse("4"),
            },
        },
        {
            pattern: "(_ (a b ...) ...)",
            input:   "(macro (1 2 3) (4 5 6))",
            want:    map[Symbol]SExpression{
                "a#0":   parse("1"),
                "b#0#0": parse("2"),
                "b#0#1": parse("3"),
                "a#1":   parse("4"),
                "b#1#0": parse("5"),
                "b#1#1": parse("6"),
            },
        },
    } {
        gensyms := map[Symbol]Symbol{}
        pp := analysePattern(nil, parse(tt.pattern), gensyms)
        revGensym := map[Symbol]Symbol{}
        for k, v := range gensyms {
            revGensym[v] = k
        }
        s := map[Symbol]SExpression{}
        ok := unify(pp, parse(tt.input), s)
        // here we have to reverse the substitution map wrt gensyms,
        // but taking into account the # annotations for ellipsis repetition
        for k, v := range s {
            delete(s, k)
            before, after, found := strings.Cut(k, "#")
            if !found {
                s[revGensym[k]] = v
                continue
            }
            s[fmt.Sprintf("%s#%s", revGensym[before], after)] = v
        }
        if !ok || !reflect.DeepEqual(s, tt.want) {
            t.Errorf("%d) got %v want %v", i, s, tt.want)
        }
    }
}

func TestSubstitution(t *testing.T) {
	for i, tt := range []struct {
		template      string
		substitutions map[Symbol]SExpression
        want          string
	} {
        {
            template: "x",
            substitutions: map[Symbol]SExpression{
                "x": parse("42"),
            },
            want:   "42",
        },
        {
            template: "(x)",
            substitutions: map[Symbol]SExpression{
                "x": parse("42"),
            },
            want:   "(42)",
        },
        {
            template: "(a b ...)",
            substitutions: map[Symbol]SExpression{
                "a": parse("42"),
            },
            want:   "(42)",
        },
        {
            template: "(a b ...)",
            substitutions: map[Symbol]SExpression{
                "a":   parse("1"),
                "b#0": parse("2"),
                "b#1": parse("3"),
                "b#2": parse("4"),
            },
            want:   "(1 2 3 4)",
        },
        {
            template: "((a b ...) ...)",
            substitutions: map[Symbol]SExpression{
                "a#0": parse("1"),
            },
            want:   "((1))",
        },
        {
            template: "((a b ...) ...)",
            substitutions: map[Symbol]SExpression{
                "a#0":   parse("1"),
                "b#0#0": parse("2"),
                "a#1":   parse("3"),
                "b#1#0": parse("4"),
            },
            want:   "((1 2) (3 4))",
        },
        {
            template: "((a b ...) ...)",
            substitutions: map[Symbol]SExpression{
                "a#0":   parse("1"),
                "b#0#0": parse("2"),
                "b#0#1": parse("3"),
                "a#1":   parse("4"),
                "b#1#0": parse("5"),
                "b#1#1": parse("6"),
            },
            want:   "((1 2 3) (4 5 6))",
        },
    } {
        // not using any gensyms here
        want := parse(tt.want)
        templ := analyseTemplate(parse(tt.template), map[Symbol]Symbol{})
        got := substituteTemplate(templ, tt.substitutions)
        if !reflect.DeepEqual(got, want) {
            t.Errorf("%d) got %v want %v", i, got, want)
        }
    }
}

func TestDefSyntax(t *testing.T) {
	env := GlobalEnv()
	for i, tt := range []struct {
		input string
		want  string
	} {
        {
            input: "(define-syntax test-macro (syntax-rules () ((_) 1) ((_ v) (cons v 2))))",
        },
        {
            input: "(test-macro)",
            want:  "1",
        },
        {
            input: "(test-macro 1)",
            want:  "(1 . 2)",
        },
        {
            input: "(define-syntax test-macro (syntax-rules () ((_ (a b)) (cons b a))))",
        },
        {
            input: "(test-macro (1 2))",
            want:  "(2 . 1)",
        },
        {
            input: `(define-syntax or
                      (syntax-rules ()
                        ((_) #f)
                        ((_ e) e)
                        ((_ e1 e2 e3 ...) (if e1 e1 (or e2 e3 ...)))))`,
        },
        {
            input: "(or #f #f 'yes)",
            want:  "yes",
        },
        {
            //TODO: previous test failed because 'list'  inside ellipsis failed to match..
            input: "(define-syntax test-macro (syntax-rules () ((_ (a b ...) ...) (list (b ... a) ...))))",
        },
        {
            input: "(test-macro (1 list 2 3 4) (5 list 6 7))",
            want:  "((2 3 4 1) (6 7 5))",
        },
	} {
		p := parse(tt.input)
		e := evalEnv(env, p)
		got := e.String()
		if got != tt.want {
			t.Errorf("%d) got %s want %s", i, got, tt.want)
		}
	}
}
