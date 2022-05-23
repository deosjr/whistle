package main

import (
	"fmt"
	"strconv"
	"strings"
)

type Symbol = string
type Number = float64
type Atom struct {
	isSymbol bool // number if false
	value    any
}

func (a Atom) symbol() string {
	if a.isSymbol == false {
		panic("not a symbol")
	}
	return a.value.(string)
}
func (a Atom) number() float64 {
	if a.isSymbol {
		panic("not a number")
	}
	return a.value.(float64)
}
func (a Atom) String() string {
	if a.isSymbol {
		return a.symbol()
	}
	if _, ok := a.value.(bool); ok {
		return ""
	}
	return strconv.FormatFloat(a.number(), 'f', -1, 64)
}

type Pair struct {
	pcar ExpOrProc
	pcdr ExpOrProc
}

func newPair(car, cdr ExpOrProc) Pair {
	return Pair{pcar: car, pcdr: cdr}
}

func (p Pair) car() ExpOrProc {
	if p == empty {
		panic("() is not a pair")
	}
	return p.pcar
}

func (p Pair) cdr() ExpOrProc {
	if p == empty {
		panic("() is not a pair")
	}
	return p.pcdr
}

func (p Pair) cadr() ExpOrProc {
	return p.cdr().exp().pair().car()
}

func (p Pair) caddr() ExpOrProc {
	return p.cdr().exp().pair().cdr().exp().pair().car()
}

func (p Pair) cadddr() ExpOrProc {
	return p.cdr().exp().pair().cdr().exp().pair().cdr().exp().pair().car()
}

func (p Pair) cdddr() ExpOrProc {
	return p.cdr().exp().pair().cdr().exp().pair().cdr()
}

var empty Pair = Pair{}

func (p Pair) String() string {
	return "(" + strings.Join(p.recString(), " ") + ")"
}

func (p Pair) recString() []string {
	if p == empty {
		return nil
	}
	s := []string{p.pcar.String()}
	cdr := p.pcdr
	if cdr.isExp && cdr.exp().isPair {
		return append(s, cdr.exp().pair().recString()...)
	}
	return append(s, ".", cdr.String())
}

func list2cons(list ...ExpOrProc) Pair {
	if len(list) == 0 {
		return empty
	}
	if len(list) == 1 {
		return Pair{pcar: list[0], pcdr: ExpOrProc{isExp: true, value: Exp{isPair: true, value: empty}}}
	}
	cons := empty
	for i := len(list) - 1; i >= 0; i-- {
		cons = Pair{pcar: list[i], pcdr: ExpOrProc{isExp: true, value: Exp{isPair: true, value: cons}}}
	}
	return cons
}

func cons2list(p Pair) []ExpOrProc {
	list := []ExpOrProc{}
	for p != empty {
		list = append(list, p.pcar)
		p = p.pcdr.exp().pair()
	}
	return list
}

type Exp struct {
	isPair bool // atom if false
	value  any
}

func (e Exp) pair() Pair {
	if e.isPair == false {
		panic("not a pair")
	}
	return e.value.(Pair)
}
func (e Exp) atom() Atom {
	if e.isPair {
		panic("not an atom")
	}
	return e.value.(Atom)
}
func (e Exp) String() string {
	if e.isPair {
		return e.pair().String()
	}
	return e.atom().String()
}

type ExpOrProc struct {
	isExp bool // proc if false
	value any
}

func (e ExpOrProc) exp() Exp {
	if e.isExp == false {
		panic("not an exp")
	}
	return e.value.(Exp)
}
func (e ExpOrProc) proc() Proc {
	if e.isExp {
		panic("not a proc")
	}
	return e.value.(Proc)
}
func (e ExpOrProc) String() string {
	if e.isExp {
		return e.exp().String()
	}
	return "#<proc>"
}

type Proc = func([]ExpOrProc) ExpOrProc

type Env struct {
	dict  map[Symbol]ExpOrProc
	outer *Env
}

func (e Env) find(s Symbol) Env {
	if _, ok := e.dict[s]; ok {
		return e
	}
	if e.outer == nil {
		panic(fmt.Sprintf("not found in env: %s", s))
	}
	return e.outer.find(s)
}

func number(e ExpOrProc) float64 {
	return e.exp().atom().number()
}
func boolean(e ExpOrProc) bool {
	return e.exp().atom().value.(bool)
}

func atomWithValue(x any) ExpOrProc {
	return ExpOrProc{isExp: true, value: Exp{value: Atom{value: x}}}
}

func newSymbol(s string) ExpOrProc {
	return ExpOrProc{isExp: true, value: Exp{value: Atom{isSymbol: true, value: s}}}
}

func pairExpression(p Pair) ExpOrProc {
	return ExpOrProc{isExp: true, value: Exp{isPair: true, value: p}}
}

func isAtom(x ExpOrProc) bool {
	if !x.isExp {
		return false
	}
	e := x.exp()
	return !e.isPair
}

func isTruthy(x ExpOrProc) bool {
	if isAtom(x) {
		a := x.exp().atom()
		if b, ok := a.value.(bool); ok {
			return b
		}
	}
	return true
}
