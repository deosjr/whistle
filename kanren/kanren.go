package kanren

import (
	_ "embed"

	"github.com/deosjr/whistle/lisp"
)

//go:embed kanren.lisp
var kanren string

func Load(l lisp.Lisp) {
	sexprs, err := lisp.Multiparse(kanren)
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
