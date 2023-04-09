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
		"string->symbol": builtinFunc(string2symbol),
        "gensym":         builtinFunc(gensymFunc),
        "eval":           builtinFunc(eval),
        "read":           builtinFunc(read),
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

func add(env *Env, args []SExpression) SExpression {
	return NewPrimitive(args[0].AsNumber() + args[1].AsNumber())
}

func sub(env *Env, args []SExpression) SExpression {
	return NewPrimitive(args[0].AsNumber() - args[1].AsNumber())
}

func mul(env *Env, args []SExpression) SExpression {
	return NewPrimitive(args[0].AsNumber() * args[1].AsNumber())
}

func eq(env *Env, args []SExpression) SExpression {
	return NewPrimitive(args[0].AsNumber() == args[1].AsNumber())
}

func lt(env *Env, args []SExpression) SExpression {
	return NewPrimitive(args[0].AsNumber() < args[1].AsNumber())
}

func gt(env *Env, args []SExpression) SExpression {
	return NewPrimitive(args[0].AsNumber() > args[1].AsNumber())
}

func leq(env *Env, args []SExpression) SExpression {
	return NewPrimitive(args[0].AsNumber() <= args[1].AsNumber())
}

func geq(env *Env, args []SExpression) SExpression {
	return NewPrimitive(args[0].AsNumber() >= args[1].AsNumber())
}

func isnumber(env *Env, args []SExpression) SExpression {
	return NewPrimitive(args[0].IsNumber())
}

func ispair(env *Env, args []SExpression) SExpression {
	x := args[0]
	if !x.IsPair() {
		return NewPrimitive(false)
	}
	return NewPrimitive(x.AsPair() != empty)
}

func car(env *Env, args []SExpression) SExpression {
	return args[0].AsPair().car()
}

func cdr(env *Env, args []SExpression) SExpression {
	return args[0].AsPair().cdr()
}

func cons(env *Env, args []SExpression) SExpression {
	return NewPair(args[0], args[1])
}

func isnull(env *Env, args []SExpression) SExpression {
	x := args[0]
	if x.IsProcedure() {
		return NewPrimitive(false)
	}
	if x.IsAtom() {
		return NewPrimitive(false)
	}
	return NewPrimitive(x.AsPair() == empty)
}

func isprocedure(env *Env, args []SExpression) SExpression {
	return NewPrimitive(args[0].IsProcedure())
}

func isequivalent(env *Env, args []SExpression) SExpression {
	return NewPrimitive(reflect.DeepEqual(args[0], args[1]))
}

func display(env *Env, args []SExpression) SExpression {
	fmt.Print(args[0])
	return NewPrimitive(true)
}

func exit(env *Env, args []SExpression) SExpression {
	os.Exit(0)
	return nil
}

func stringappend(env *Env, args []SExpression) SExpression {
	s := ""
	for _, arg := range args {
		s += arg.AsPrimitive().(string)
	}
	return NewPrimitive(s)
}

func number2string(env *Env, args []SExpression) SExpression {
	return NewPrimitive(fmt.Sprintf("%v", args[0].AsNumber()))
}

func string2symbol(env *Env, args []SExpression) SExpression {
	return NewSymbol(args[0].AsPrimitive().(string))
}

func gensymFunc(env *Env, args []SExpression) SExpression {
    return NewSymbol(gensym())
}

func eval(env *Env, args []SExpression) SExpression {
    return evalEnv(env, args[0])
}

func read(env *Env, args []SExpression) SExpression {
    return parse(args[0].AsPrimitive().(string))
}
