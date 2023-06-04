package erlang

import (
	_ "embed"

	"github.com/deosjr/whistle/lisp"
)

func Load(l lisp.Lisp) {
	// TODO: move macros to erlang.lisp
	// TODO: should fail if kanren hasnt been loaded
	lisp.LoadErlang(l)
}
