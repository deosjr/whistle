package datalog

import (
	_ "embed"

	"github.com/deosjr/whistle/lisp"
)

//go:embed datalog.lisp
var datalog string

func Load(l lisp.Lisp) {
	// TODO: should fail if kanren hasnt been loaded
	sexprs, err := lisp.Multiparse(datalog)
	if err != nil {
		panic(err)
	}
	for _, def := range sexprs {
		_, err := l.EvalExpr(def)
		if err != nil {
			panic(err)
		}
	}
}
