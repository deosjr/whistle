package datalog

import (
	_ "embed"

	"github.com/deosjr/whistle/lisp"
)

//go:embed datalog.lisp
var datalog string

func Load(l lisp.Lisp) {
	// TODO: should fail if kanren hasnt been loaded
	err := l.Load(datalog)
	if err != nil {
		panic(err)
	}
}
