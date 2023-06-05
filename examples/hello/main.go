package main

import (
	"fmt"
	"log"

	"github.com/deosjr/whistle/lisp"
)

func main() {
	l := lisp.New()
	e, err := l.Eval("(* 6 7)")
	if err != nil {
		log.Fatal(err)
	}
	fmt.Println(e) // prints 42
}
