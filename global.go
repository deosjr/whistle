package main

import (
    "bufio"
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
        "newline":        NewPrimitive("\n"),
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
        "read-string":    builtinFunc(readString),
        "environment":    builtinFunc(environment),
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

func add(p *process, env *Env, args []SExpression) SExpression {
	return NewPrimitive(args[0].AsNumber() + args[1].AsNumber())
}

func sub(p *process, env *Env, args []SExpression) SExpression {
	return NewPrimitive(args[0].AsNumber() - args[1].AsNumber())
}

func mul(p *process, env *Env, args []SExpression) SExpression {
	return NewPrimitive(args[0].AsNumber() * args[1].AsNumber())
}

func eq(p *process, env *Env, args []SExpression) SExpression {
	return NewPrimitive(args[0].AsNumber() == args[1].AsNumber())
}

func lt(p *process, env *Env, args []SExpression) SExpression {
	return NewPrimitive(args[0].AsNumber() < args[1].AsNumber())
}

func gt(p *process, env *Env, args []SExpression) SExpression {
	return NewPrimitive(args[0].AsNumber() > args[1].AsNumber())
}

func leq(p *process, env *Env, args []SExpression) SExpression {
	return NewPrimitive(args[0].AsNumber() <= args[1].AsNumber())
}

func geq(p *process, env *Env, args []SExpression) SExpression {
	return NewPrimitive(args[0].AsNumber() >= args[1].AsNumber())
}

func isnumber(p *process, env *Env, args []SExpression) SExpression {
	return NewPrimitive(args[0].IsNumber())
}

func ispair(p *process, env *Env, args []SExpression) SExpression {
	x := args[0]
	if !x.IsPair() {
		return NewPrimitive(false)
	}
	return NewPrimitive(x.AsPair() != empty)
}

func car(p *process, env *Env, args []SExpression) SExpression {
	return args[0].AsPair().car()
}

func cdr(p *process, env *Env, args []SExpression) SExpression {
	return args[0].AsPair().cdr()
}

func cons(p *process, env *Env, args []SExpression) SExpression {
	return NewPair(args[0], args[1])
}

func isnull(p *process, env *Env, args []SExpression) SExpression {
	x := args[0]
	if x.IsProcedure() {
		return NewPrimitive(false)
	}
	if x.IsAtom() {
		return NewPrimitive(false)
	}
	return NewPrimitive(x.AsPair() == empty)
}

func isprocedure(p *process, env *Env, args []SExpression) SExpression {
	return NewPrimitive(args[0].IsProcedure())
}

func isequivalent(p *process, env *Env, args []SExpression) SExpression {
	return NewPrimitive(reflect.DeepEqual(args[0], args[1]))
}

func display(p *process, env *Env, args []SExpression) SExpression {
	fmt.Print(args[0])
	return NewPrimitive(true)
}

func exit(p *process, env *Env, args []SExpression) SExpression {
	os.Exit(0)
	return nil
}

func stringappend(p *process, env *Env, args []SExpression) SExpression {
	s := ""
	for _, arg := range args {
		s += arg.AsPrimitive().(string)
	}
	return NewPrimitive(s)
}

func number2string(p *process, env *Env, args []SExpression) SExpression {
	return NewPrimitive(fmt.Sprintf("%v", args[0].AsNumber()))
}

func string2symbol(p *process, env *Env, args []SExpression) SExpression {
	return NewSymbol(args[0].AsPrimitive().(string))
}

func gensymFunc(p *process, env *Env, args []SExpression) SExpression {
    return NewSymbol(gensym())
}

// (eval expression [env]), defaults to env=env
func eval(p *process, env *Env, args []SExpression) SExpression {
    if len(args) == 1 {
        return p.evalEnv(env, args[0])
    }
    return p.evalEnv(args[1].AsPrimitive().(*Env), args[0])
}

func read(p *process, env *Env, args []SExpression) SExpression {
    scanner := bufio.NewScanner(os.Stdin)
    scanner.Scan()
    return parse(scanner.Text())
}

func readString(p *process, env *Env, args []SExpression) SExpression {
    return parse(args[0].AsPrimitive().(string))
}

func environment(p *process, env *Env, args []SExpression) SExpression {
    return NewPrimitive(env)
}
