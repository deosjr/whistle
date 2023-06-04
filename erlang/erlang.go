package erlang

import (
	_ "embed"

	"github.com/deosjr/whistle/lisp"
)

func Load(l lisp.Lisp) {
	lisp.LoadErlang(l)
}
