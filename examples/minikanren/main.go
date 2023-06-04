package main

import (
	"fmt"
	"log"

	"github.com/deosjr/whistle/kanren"
	"github.com/deosjr/whistle/lisp"
)

func main() {
	l := lisp.New()
	kanren.Load(l)
	e, err := l.Eval("(display (car (run* (fresh (q) (equalo q 42)))))")
	if err != nil {
		log.Fatal(err)
	}
	fmt.Println(e) // prints 42
}
