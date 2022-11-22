package main

import (
	"fmt"
	"math"
	"os"
	"reflect"
)

func GlobalEnv() *Env {
	return &Env{dict: map[Symbol]SExpression{
		"+":              builtinFunc(add),
		"-":              builtinFunc(sub),
		"*":              builtinFunc(mul),
		"=":              builtinFunc(eq),
		"<":              builtinFunc(lt),
		">":              builtinFunc(gt),
		"<=":             builtinFunc(leq),
		">=":             builtinFunc(geq),
		"#t":             NewPrimitive(true),
		"#f":             NewPrimitive(false),
		"pi":             NewPrimitive(math.Pi),
		"number?":        builtinFunc(isnumber),
		"pair?":          builtinFunc(ispair),
		"car":            builtinFunc(car),
		"cdr":            builtinFunc(cdr),
		"cons":           builtinFunc(cons),
		"null?":          builtinFunc(isnull),
		"procedure?":     builtinFunc(isprocedure),
		"eqv?":           builtinFunc(isequivalent),
		"display":        builtinFunc(display),
		"exit":           builtinFunc(exit),
		"string-append":  builtinFunc(stringappend),
		"number->string": builtinFunc(number2string),
	}, outer: nil}
}

func builtinFunc(f BuiltinProc) Proc {
	return Proc{
		isBuiltin: true,
		sexpression: sexpression{
			value: f,
		},
	}
}

func add(args []SExpression) SExpression {
	return NewPrimitive(args[0].AsNumber() + args[1].AsNumber())
}

func sub(args []SExpression) SExpression {
	return NewPrimitive(args[0].AsNumber() - args[1].AsNumber())
}

func mul(args []SExpression) SExpression {
	return NewPrimitive(args[0].AsNumber() * args[1].AsNumber())
}

func eq(args []SExpression) SExpression {
	return NewPrimitive(args[0].AsNumber() == args[1].AsNumber())
}

func lt(args []SExpression) SExpression {
	return NewPrimitive(args[0].AsNumber() < args[1].AsNumber())
}

func gt(args []SExpression) SExpression {
	return NewPrimitive(args[0].AsNumber() > args[1].AsNumber())
}

func leq(args []SExpression) SExpression {
	return NewPrimitive(args[0].AsNumber() <= args[1].AsNumber())
}

func geq(args []SExpression) SExpression {
	return NewPrimitive(args[0].AsNumber() >= args[1].AsNumber())
}

func isnumber(args []SExpression) SExpression {
	return NewPrimitive(args[0].IsNumber())
}

func ispair(args []SExpression) SExpression {
	x := args[0]
	if !x.IsPair() {
		return NewPrimitive(false)
	}
	return NewPrimitive(x.AsPair() != empty)
}

func car(args []SExpression) SExpression {
	return args[0].AsPair().car()
}

func cdr(args []SExpression) SExpression {
	return args[0].AsPair().cdr()
}

func cons(args []SExpression) SExpression {
	return NewPair(args[0], args[1])
}

func isnull(args []SExpression) SExpression {
	x := args[0]
	if x.IsProcedure() {
		return NewPrimitive(false)
	}
	if x.IsAtom() {
		return NewPrimitive(false)
	}
	return NewPrimitive(x.AsPair() == empty)
}

func isprocedure(args []SExpression) SExpression {
	return NewPrimitive(args[0].IsProcedure())
}

func isequivalent(args []SExpression) SExpression {
	return NewPrimitive(reflect.DeepEqual(args[0], args[1]))
}

func display(args []SExpression) SExpression {
	fmt.Print(args[0])
	return NewPrimitive(true)
}

func exit(args []SExpression) SExpression {
	os.Exit(0)
	return nil
}

func stringappend(args []SExpression) SExpression {
	s := ""
	for _, arg := range args {
		s += arg.AsPrimitive().(string)
	}
	return NewPrimitive(s)
}

func number2string(args []SExpression) SExpression {
	return NewPrimitive(fmt.Sprintf("%v", args[0].AsNumber()))
}
