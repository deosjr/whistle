package main

import (
    "fmt"
    "strconv"
)

type Symbol = string
type Number = float64
type Atom struct {
    isSymbol bool // number if false
    value any
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
    return strconv.FormatFloat(a.number(), 'f', -1, 64)
}

type List = []Exp
type Exp struct {
    isList bool // atom if false
    value any
}
func (e Exp) list() []ExpOrProc {
    if e.isList == false {
        panic("not a list")
    }
    return e.value.([]ExpOrProc)
}
func (e Exp) atom() Atom {
    if e.isList {
        panic("not an atom")
    }
    return e.value.(Atom)
}
func (e Exp) String() string {
    if e.isList {
        return fmt.Sprintf("%v", e.list())
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
    return e.exp().String()
}

type Proc = func([]ExpOrProc) ExpOrProc

type Env struct {
    dict map[Symbol]ExpOrProc
    outer *Env
}

func (e Env) find(s Symbol) Env {
    if _, ok := e.dict[s]; ok {
        return e
    }
    return e.outer.find(s)
}

func number(e ExpOrProc) float64 {
    return e.exp().atom().number()
}

func atomWithValue(x any) ExpOrProc {
    return ExpOrProc{isExp: true, value: Exp{value: Atom{value: x}}}
}
