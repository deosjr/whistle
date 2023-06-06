package kanren

import (
	_ "embed"

	"github.com/deosjr/whistle/lisp"
)

//go:embed kanren.lisp
var kanren string

func Load(l lisp.Lisp) {
	err := l.Load(kanren)
	if err != nil {
		panic(err)
	}
}
