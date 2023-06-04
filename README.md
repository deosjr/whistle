# Whistle

LISP interpreter in Golang, plus a collection of interesting add-ons implemented in Go/LISP/both.
Emphasis is on hackability over performance and bugs remain: don't use this in production!

![http://poohpics.narod.ru/pictures/disney/gopher/gopher1.gif](http://poohpics.narod.ru/pictures/disney/gopher/gopher1.gif)

## LISP

Actually more of a Scheme, based on Peter Norvig's excellent [Lispy](https://norvig.com/lispy.html).
It includes a partial implementation of macros based on `syntax-rules` and an optional continuation-passing style interpreter.

### Example

The lisp.New function creates a new running process with a default evaluation environment. This includes builtin functions like `cons` and `car`, but also some standard macros. 
Eval returns an sexpression and an error. Only evaluating a single sexpression is supported at the moment, but wrapping multiple expressions in `begin` serves to eval all at once.

```go
import (
    "fmt"
    "log"
    "github.com/deosjr/lispadventures/lisp"
)

func main() {
    l := lisp.New()
    e, err := l.Eval("(* 6 7)")
    if err != nil {
    	log.Fatal(err)
    }
    fmt.Println(e) // prints 42
}
```

## Extending LISP using Go

We can extend the set of builtin functions and types quite easily. The type system is kept small; anything that is not included is a Primitive. These need typecasting in Go. Builtin functions are introduced to the environment as follows:
```go
    l := lisp.New()
    l.Env.AddBuiltin("sin", func(args []lisp.SExpression) (lisp.SExpression, error) {
        return lisp.NewPrimitive(math.Sin(args[0].AsNumber())), nil
    })
    l.Env.AddBuiltin("cos", func(args []lisp.SExpression) (lisp.SExpression, error) {
        return lisp.NewPrimitive(math.Cos(args[0].AsNumber())), nil
    })
    // hereafter, 'sin' and 'cos' are recognised as builtin functions in lisp
```

Another example can be found in this [repo](https://github.com/deosjr/lispgraphics/blob/main/pixel.go) where I wrap the faiface/pixel lib and call it from lisp.

## MiniKanren

As per [µKanren: A Minimal Functional Core for Relational Programming](http://webyrd.net/scheme-2013/papers/HemannMuKanren2013.pdf), implementation taken straight from the paper. Allows you to use a pure logic language in Go through Scheme. See the paper for details.

### Example
```go
func main() {
    l := lisp.New()
    kanren.Load(l)
    l.Eval("(display (car (run* (fresh (q) (equalo q 42)))))") // prints 42
}
```

## Concurrency

One motivation for starting this repo was to explore concurrency in minikanren, since I couldn't get that to work in either Racket or Scheme minikanren implementations. I ended up implementing some of Erlang's concurrency semantics and actor model on top of Go's CSP, using minikanren for pattern matching messages. The end result looks somewhat like [LFE, or Lisp Flavoured Erlang](https://github.com/lfe/lfe).

### Example
Here we define a REPL function and a restarter, which kicks off the REPL and restarts it whenever it goes down with an error. There is some hacking involved to make sure the evaluation environment survives; here be dragons!
```go
func main() {
    l := lisp.New()
    kanren.Load(l)
    erlang.Load(l)
    l.Eval(`(define REPL (lambda (env)
        (begin (display "> ")
               (display (eval (read) env))
               (display newline)
               (REPL env))))`)
    l.Eval(`(define restarter (lambda (env)
        (begin (process_flag 'trap_exit #t)
               (let ((pid (spawn_link (lambda () (begin (process_flag 'eval_with_continuation #t) (REPL env))) (quote ()))))
                    (receive
                        ((reason) (quasiquote (EXIT ,pid ,reason)) ->
                            (if (eqv? reason "normal") #t
                            (begin (display "** exception error: ") (display reason) (display newline) (restarter env)))))))))`)
    l.Eval("(restarter (environment))") // starts an interactive REPL
}
```

## Continuations

Continuation passing style interpretation is supported. The process flag `eval_with_continuation` switches evaluation modes. l.Continue only works if the process is in CPS mode and otherwise errors.

### Example
```go
func main() {
    l := lisp.New()
    l.Eval("(process_flag 'eval_with_continuation #t)")
    l.Eval("(define x 6)")
    e, _ := l.Eval("(begin (display (* x y)) (display newline))")
    l.Eval("(define y 7)")
    l.Continue(e) // prints 42
}
```

## Datalog

A late addition, this is a basic attempt at implementing Datalog using minikanren for most of the heavy lifting and some macro-hacking to hook it all up. Definitely the least production-ready part of all.

### Example
Here we define a directed graph with 5 vertices and some edges between them. We then introduce rules to determine the `reachable` relation between vertices, and manually trigger fixpoint analysis.
After all that, we ask "which vertices are reachable from themselves?" (ie find cycles).
```go
func main() {
    l := lisp.New()
    kanren.Load(l)
    datalog.Load(l)
    l.Eval(`(begin
        (define a (dl_record 'vertex))
        (define b (dl_record 'vertex))
        (define c (dl_record 'vertex))
        (define d (dl_record 'vertex))
        (define e (dl_record 'vertex)))`)
    l.Eval("(define dl_edge (lambda (x y) (dl_assert x 'edge y)))")
	l.Eval(`(begin
        (dl_edge a c)
        (dl_edge b a)
        (dl_edge b d)
        (dl_edge c d)
        (dl_edge d a)
        (dl_edge d e))`)
    l.Eval("(dl_rule (reachable ,?x ,?y) :- (edge ,?x ,?y))")
    l.Eval("(dl_rule (reachable ,?x ,?y) :- (edge ,?x ,?z) (reachable ,?z ,?y))")
    l.Eval("(dl_fixpoint)")
    // prints (1 3 4) or a permutation thereof
    l.Eval(`(display (dl_find ,?id where ( (,?id reachable ,?id))))`)
}
```
