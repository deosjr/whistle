package main

import (
	"strconv"
	"strings"
)

// sexpression bool flags
// isExpression isAtom    isSymbol
// else Proc    else Pair else Primitive
// if Proc      isBuiltin
//              else user defined procedure
// Primitives escape back to golang types

type SExpression interface {
	IsSymbol() bool
	IsPrimitive() bool
	IsNumber() bool
	IsAtom() bool
	IsPair() bool
	IsExpression() bool
	IsProcedure() bool
	AsSymbol() Symbol
	AsPrimitive() any
	AsNumber() Number
	AsAtom() Atom
	AsPair() Pair
	AsProcedure() Proc //edure
	String() string
}

type sexpression struct {
	isExpression bool
	isAtom       bool
	isSymbol     bool
	value        any
}

// NOTE: the below panics should never occur;
// main loop should guard against that

func (s sexpression) IsSymbol() bool {
	return s.isExpression && s.isAtom && s.isSymbol
}

func (s sexpression) IsPrimitive() bool {
	return s.isExpression && s.isAtom && !s.isSymbol
}

func (s sexpression) IsNumber() bool {
	if !s.IsPrimitive() {
		return false
	}
	_, ok := s.value.(Number)
	return ok
}

func (s sexpression) IsAtom() bool {
	return s.isExpression && s.isAtom
}

func (s sexpression) IsPair() bool {
	return s.isExpression && !s.isAtom
}

func (s sexpression) IsExpression() bool {
	return s.isExpression
}

func (s sexpression) IsProcedure() bool {
	return !s.isExpression
}

func (s sexpression) AsSymbol() Symbol {
	if !s.IsSymbol() {
		panic("not a symbol")
	}
	return s.value.(Symbol)
}

func (s sexpression) AsPrimitive() any {
	if !s.IsPrimitive() {
		panic("not a primitive")
	}
	return s.value
}

func (s sexpression) AsNumber() Number {
	if !s.IsNumber() {
		panic("not a number")
	}
	return s.value.(Number)
}

func (s sexpression) AsAtom() Atom {
	panic("not an atom")
}

func (s sexpression) AsPair() Pair {
	panic("not a pair")
}

func (s sexpression) AsProcedure() Proc {
	panic("not a procedure")
}

type Symbol = string

func NewSymbol(s string) Atom {
	a := NewAtom(s)
	a.isSymbol = true
	return a
}

type Number = float64

func NewPrimitive(v any) Atom {
	return NewAtom(v)
}

type Atom struct {
	sexpression
}

func NewAtom(v any) Atom {
	return Atom{sexpression{
		isExpression: true,
		isAtom:       true,
		value:        v,
	}}
}

func (a Atom) AsAtom() Atom {
	return a
}

func (a Atom) String() string {
	if a.IsSymbol() {
		return a.AsSymbol()
	}
	// TODO: hacked bool type into Number type here!
	if _, ok := a.value.(bool); ok {
		return ""
	}
	if s, ok := a.value.(string); ok {
		return s
	}
	return strconv.FormatFloat(a.AsNumber(), 'f', -1, 64)
}

type Pair struct {
	sexpression
	pcar SExpression
	pcdr SExpression
}

func NewPair(car, cdr SExpression) Pair {
	return Pair{
		sexpression: sexpression{
			isExpression: true,
		},
		pcar: car,
		pcdr: cdr,
	}
}

func (p Pair) AsPair() Pair {
	return p
}

func (p Pair) car() SExpression {
	if p == empty {
		panic("() is not a pair")
	}
	return p.pcar
}

func (p Pair) cdr() SExpression {
	if p == empty {
		panic("() is not a pair")
	}
	return p.pcdr
}

func (p Pair) cadr() SExpression {
	return p.cdr().AsPair().car()
}

func (p Pair) caddr() SExpression {
	return p.cdr().AsPair().cdr().AsPair().car()
}

func (p Pair) cadddr() SExpression {
	return p.cdr().AsPair().cdr().AsPair().cdr().AsPair().car()
}

func (p Pair) cddr() SExpression {
	return p.cdr().AsPair().cdr()
}

func (p Pair) cdddr() SExpression {
	return p.cdr().AsPair().cdr().AsPair().cdr()
}

var empty Pair = NewPair(nil, nil)

func (p Pair) String() string {
	return "(" + strings.Join(p.recString(), " ") + ")"
}

func (p Pair) recString() []string {
	if p == empty {
		return nil
	}
	s := []string{p.pcar.String()}
	cdr := p.pcdr
	if cdr.IsPair() {
		return append(s, cdr.AsPair().recString()...)
	}
	return append(s, ".", cdr.String())
}

func list2cons(list ...SExpression) Pair {
	if len(list) == 0 {
		return empty
	}
	if len(list) == 1 {
		return NewPair(list[0], empty)
	}
	cons := empty
	for i := len(list) - 1; i >= 0; i-- {
		cons = NewPair(list[i], cons)
	}
	return cons
}

func cons2list(p Pair) []SExpression {
	list := []SExpression{}
	for p != empty {
		list = append(list, p.pcar)
		p = p.pcdr.AsPair()
	}
	return list
}

type Proc struct {
	sexpression
	isBuiltin bool // user defined proc if false
}

func (p Proc) AsProcedure() Proc {
	return p
}

func (p Proc) builtin() BuiltinProc {
	if p.isBuiltin == false {
		panic("not a builtin proc")
	}
	return p.value.(BuiltinProc)
}
func (p Proc) defined() DefinedProc {
	if p.isBuiltin {
		panic("not a userdefined proc")
	}
	return p.value.(DefinedProc)
}

func (p Proc) String() string {
	return "#<proc>"
}

type DefinedProc struct {
	params Pair
	body   SExpression
	env    *Env
}

type BuiltinProc = func(*Env, []SExpression) SExpression

func boolean(e SExpression) bool {
	return e.AsAtom().value.(bool)
}

func isTruthy(x SExpression) bool {
	if x.IsAtom() {
		if b, ok := x.AsAtom().value.(bool); ok {
			return b
		}
	}
	return true
}
