package main

import (
	"github.com/deosjr/whistle/lisp"
)

func main() {
	l := lisp.New()
	l.Eval("(process_flag 'eval_with_continuation #t)")
	l.Eval("(define x 6)")
	e, _ := l.Eval("(begin (display (* x y)) (display newline))")
	l.Eval("(define y 7)")
	l.Continue(e) // prints 42
}
