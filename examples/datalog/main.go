package main

import (
	"github.com/deosjr/whistle/datalog"
	"github.com/deosjr/whistle/kanren"
	"github.com/deosjr/whistle/lisp"
)

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
