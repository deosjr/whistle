(begin
    (define a (dl_record 'vertex))
    (define b (dl_record 'vertex))
    (define c (dl_record 'vertex))
    (define d (dl_record 'vertex))
    (define e (dl_record 'vertex)))

(define dl_edge (lambda (x y) (dl_assert x 'edge y)))

(begin
    (dl_edge a c)
    (dl_edge b a)
    (dl_edge b d)
    (dl_edge c d)
    (dl_edge d a)
    (dl_edge d e))

(dl_rule (reachable ,?x ,?y) :- (edge ,?x ,?y))
(dl_rule (reachable ,?x ,?y) :- (edge ,?x ,?z) (reachable ,?z ,?y))
(dl_fixpoint)

(display (dl_find ,?id where ( (,?id reachable ,?id))))
(display newline)
