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
            want: pattern{isVariable: true, content: mustParse("a")},
        },
        {
            pattern: "list",
            literals: []string{"list"},
            want: pattern{isLiteral: true, content: mustParse("list")},
        },
        {
            pattern: "(_ a b ...)",
            want: pattern{
                isList: true,
                listContent: []pattern{
                    {isUnderscore: true},
                    {isVariable: true, content: mustParse("a")},
                    {isVariable: true, hasEllipsis: true, content: mustParse("b")},
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
                        {isVariable: true, content: mustParse("a")},
                        {isVariable: true, hasEllipsis: true, content: mustParse("b")}},
                    hasEllipsis: true},
                },
            },
        },
    } {
        sp := mustParse(tt.pattern)
        gensyms := map[Symbol]Symbol{}
        ellipsis := map[Symbol]int{}
        got := analysePattern(tt.literals, sp, gensyms, ellipsis)
        // gensyms contains a->hashA lookups. for this test, we can reverse those
        // and check against our wanted pattern which uses non-hashed var names
        // NOTE: this makes it harder to check gensymmed vs non-gensymmed vars
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
        literals []string
        gensyms  map[Symbol]Symbol
        ellipsis map[Symbol]int
		want     pattern
	} {
        {
            template: "a",
            gensyms:  map[Symbol]Symbol{ "a": "hashA" },
            want:     pattern{isVariable: true, content: mustParse("a")},
        },
        {
            template: "(list a)",
            literals: []string{"list"},
            gensyms:  map[Symbol]Symbol{ "a": "hashA" },
            want:     pattern{isList: true, listContent: []pattern{
                        {isLiteral: true, content: mustParse("list")},
                        {isVariable: true, content: mustParse("a")}}},
        },
	} {
        st := mustParse(tt.template)
        got := analyseTemplate(tt.literals, st, tt.gensyms, tt.ellipsis)
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
		pattern  string
        input    string
        literals []string
		want     map[Symbol]SExpression
	} {
        {
            pattern: "x",
            input:   "42",
            want:    map[Symbol]SExpression{
                "x": mustParse("42"),
            },
        },
        {
            pattern: "(_ x)",
            input:   "(macro 42)",
            want:    map[Symbol]SExpression{
                "x": mustParse("42"),
            },
        },
        {
            pattern: "(_ a b ...)",
            input:   "(macro 42)",
            want:    map[Symbol]SExpression{
                "a": mustParse("42"),
            },
        },
        {
            pattern: "(_ a b ...)",
            input:   "(macro 1 2 3 4)",
            want:    map[Symbol]SExpression{
                "a":   mustParse("1"),
                "b#0": mustParse("2"),
                "b#1": mustParse("3"),
                "b#2": mustParse("4"),
            },
        },
        {
            pattern: "(_ (a b ...) ...)",
            input:   "(macro (1))",
            want:    map[Symbol]SExpression{
                "a#0": mustParse("1"),
            },
        },
        {
            pattern: "(_ (a b ...) ...)",
            input:   "(macro (1 2) (3 4))",
            want:    map[Symbol]SExpression{
                "a#0":   mustParse("1"),
                "b#0#0": mustParse("2"),
                "a#1":   mustParse("3"),
                "b#1#0": mustParse("4"),
            },
        },
        {
            pattern: "(_ (a b ...) ...)",
            input:   "(macro (1 2 3) (4 5 6))",
            want:    map[Symbol]SExpression{
                "a#0":   mustParse("1"),
                "b#0#0": mustParse("2"),
                "b#0#1": mustParse("3"),
                "a#1":   mustParse("4"),
                "b#1#0": mustParse("5"),
                "b#1#1": mustParse("6"),
            },
        },
        {
            pattern:  "(_ list)",
            literals: []string{"list"},
            input:    "(macro list)",
            want:     map[Symbol]SExpression{},
        },
    } {
        gensyms := map[Symbol]Symbol{}
        ellipsis := map[Symbol]int{}
        pp := analysePattern(tt.literals, mustParse(tt.pattern), gensyms, ellipsis)
        revGensym := map[Symbol]Symbol{}
        for k, v := range gensyms {
            revGensym[v] = k
        }
        s := map[Symbol]SExpression{}
        ok := unify(pp, mustParse(tt.input), s)
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
        literals      []string
		substitutions map[Symbol]SExpression
        ellipsis      map[Symbol]int
        want          string
	} {
        {
            template: "x",
            substitutions: map[Symbol]SExpression{
                "x": mustParse("42"),
            },
            want:   "42",
        },
        {
            template: "(x)",
            substitutions: map[Symbol]SExpression{
                "x": mustParse("42"),
            },
            want:   "(42)",
        },
        {
            template: "(a b ...)",
            substitutions: map[Symbol]SExpression{
                "a": mustParse("42"),
            },
            want:   "(42)",
        },
        {
            template: "(a b ...)",
            substitutions: map[Symbol]SExpression{
                "a":   mustParse("1"),
                "b#0": mustParse("2"),
                "b#1": mustParse("3"),
                "b#2": mustParse("4"),
            },
            ellipsis: map[Symbol]int{"b":1},
            want:   "(1 2 3 4)",
        },
        {
            template: "((a b ...) ...)",
            substitutions: map[Symbol]SExpression{
                "a#0": mustParse("1"),
            },
            ellipsis: map[Symbol]int{"a":1, "b":2},
            want:   "((1))",
        },
        {
            template: "((a b ...) ...)",
            substitutions: map[Symbol]SExpression{
                "a#0":   mustParse("1"),
                "b#0#0": mustParse("2"),
                "a#1":   mustParse("3"),
                "b#1#0": mustParse("4"),
            },
            ellipsis: map[Symbol]int{"a":1, "b":2},
            want:   "((1 2) (3 4))",
        },
        {
            template: "((a b ...) ...)",
            substitutions: map[Symbol]SExpression{
                "a#0":   mustParse("1"),
                "b#0#0": mustParse("2"),
                "b#0#1": mustParse("3"),
                "a#1":   mustParse("4"),
                "b#1#0": mustParse("5"),
                "b#1#1": mustParse("6"),
            },
            ellipsis: map[Symbol]int{"a":1, "b":2},
            want:   "((1 2 3) (4 5 6))",
        },
        {
            template: "(list x)",
            literals: []string{"list"},
            substitutions: map[Symbol]SExpression{
                "x": mustParse("42"),
            },
            want:   "(list 42)",
        },
        /*
        {
            template: "(list x)",
            literals: []string{"list"},
            substitutions: map[Symbol]SExpression{},
            want:   "(list gensymX)",
        },
        */
    } {
        // not using any gensyms here
        want := mustParse(tt.want)
        templ := analyseTemplate(tt.literals, mustParse(tt.template), map[Symbol]Symbol{}, map[Symbol]int{})
        got := substituteTemplate(templ, tt.substitutions, tt.ellipsis)
        if !reflect.DeepEqual(got, want) {
            t.Errorf("%d) got %v want %v", i, got, want)
        }
    }
}

func TestDefSyntax(t *testing.T) {
    main := newProcess()
	env := GlobalEnv()
	for i, tt := range []struct {
		input string
		want  string
	} {
        {
            input: "(define-syntax test-macro (syntax-rules (cons) ((_) 1) ((_ v) (cons v 2))))",
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
            input: "(define-syntax test-macro (syntax-rules (cons) ((_ (a b)) (cons b a))))",
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
            input: "(define-syntax test-macro (syntax-rules (list) ((_ (a b ...) ...) (list (b ... a) ...))))",
        },
        {
            input: "(test-macro (1 list 2 3 4) (5 list 6 7))",
            want:  "((2 3 4 1) (6 7 5))",
        },
        {
            input: "(define-syntax test-macro (syntax-rules (list) ((_ (a b ...) ...) (list (list b ...) ...))))",
        },
        {
            input: "(test-macro (1 2 3 4) (5 6 7))",
            want:  "((2 3 4) (6 7))",
        },
	} {
        p, err := parse(tt.input)
        if err != nil {
            t.Errorf("%d) parse error %v", i, err)
        }
        e, err := main.evalEnv(env, p)
        if err != nil {
            t.Errorf("%d) eval error %v", i, err)
        }
		got := e.String()
		if got != tt.want {
			t.Errorf("%d) got %s want %s", i, got, tt.want)
		}
	}
}
