#| dl_db is a global db to query (not supporting multiple dbs/db as arg)
   for starters, this will be a simple list of EAV tuples
   split into EDB (extensional, input), IDB (intentional, derived), and RDB (rules) |#
(define dl_edb (make-hashmap))
(define dl_idb (make-hashmap))
(define dl_rdb '())

#| further split into entity/attr indices etc to optimise search
   NOTE: these will include derived tuples |#
(define dl_idx_entity (make-hashmap))
(define dl_idx_attr (make-hashmap))

#| dl_nextID is a closure around an ever-increasing counter
   TODO: proper closure hiding dl_counter instead of this global nonsense |#
(define dl_counter 0)
(define dl_nextID (lambda () (begin
   (set! dl_counter (+ dl_counter 1))
   dl_counter)))

#| dl_assert and dl_retract operate on the global edb
   inserting datoms, 3-value tuples (im ignoring tx, the 4th in EAVT)
   NOTE: order is thrown out, since we use hashmaps underneath
   updates entity/attr indices as well (opting not to use a value index yet) |#
(define dl_assert (lambda (entity attr value) (begin
  (hashmap-set! dl_edb (list entity attr value) #t)
  (dl_update_indices (list entity attr value)))))
#| TODO: retract |#
(define dl_update_indices (lambda (tuple)
   (let ((entity (car tuple))
         (attr (car (cdr tuple))))
     (let ((m (hashmap-ref dl_idx_entity entity #f)))
       (if m (hashmap-set! m tuple #t)
         (let ((new (make-hashmap)))
           (hashmap-set! dl_idx_entity entity new)
           (hashmap-set! new tuple #t))))
     (let ((m (hashmap-ref dl_idx_attr attr #f)))
       (if m (hashmap-set! m tuple #t)
         (let ((new (make-hashmap)))
           (hashmap-set! dl_idx_attr attr new)
           (hashmap-set! new tuple #t)))))))

#| dl_record is a convenience macro to add structured data
   returns id of entry added |#
(define-syntax dl_record
   (syntax-rules (list let dl_nextID dl_assert)
     ((_ type (attr value) ...) (let ((id (dl_nextID)))
       (dl_assert id (list type attr) value) ... id))))

#| NOTE: theres a lot going on in this macro.
   dl_vars looks for all vars recursively in a list starting with ?
   when we want to run minikanren we need to explicitly name all vars using 'fresh'
   NOTE: using more dynamic fresh, not as macro, able to take an interpreted list of varnames
   if there is a way to do this in the macro system, I havent found it yet (eval hacking instead..)
   TODO: add unquote to every dl_var instance so we dont have to type it in dl_find input |#
(define-syntax dl_find
   (syntax-rules (where run* equalo dl_edb dl_idb dl_vars let list->set cons eval hashmap-keys fresh dl_findo)
     ((_ x where ( match ... ))
      (let ((vars (list->set (dl_vars '(x match ...))))
            (edb (hashmap-keys dl_edb))
            (idb (hashmap-keys dl_idb)))
        (run* (eval (cons 'fresh (cons (cons 'q vars) '((equalo q `x) (dl_findo `match edb idb) ...)))))))))

#| TODO: start at breaking out logic of dl_find macro so we can use indices and speed this up a bit |#
(define dl_findo (lambda (m edb idb)
   (fresh (x y entity attr db)
   (conso entity x m)
   (conso attr y x)
     (conde
       [(boundo entity) (lookupo dl_idx_entity entity db) (membero m db)]
       [(unboundo entity) (boundo attr) (lookupo dl_idx_attr attr db) (membero m db)] ))))

(define dl_var? (lambda (s) (if (symbol? s) (prefix? (symbol->string s) "?"))))

(define dl_vars (lambda (l)
   (list->set (foldl (lambda (x acc)
     (cond
       [(dl_var? x) (cons x acc)]
       [(pair? x) (append (dl_vars x) acc)]
       [else acc])) l '()))))

#| introduce a rule into the (separate!) rule db
   TODO: only deals with arity/2 predicates atm |#
(define-syntax dl_rule
   (syntax-rules (list dl_assert_rule :-)
     ((_ (head hx hy) :- (body bx by) ...)
      (dl_assert_rule '(hx head hy) (list '(bx body by) ...)))))

(define dl_assert_rule (lambda (head body)
  (set! dl_rdb (cons (cons head body) dl_rdb))))
#| TODO: define dl_retract_rule |#

#| manually trigger fixpoint analysis, repeatedly running rules against all known entities
   starts by clearing the IDB each time
   then finds ways to satisfy head of each rule in RDB
   if this finds any new pairs, repeat, until we reach a fixed point (no more additions)
   TODO: semi-naive implementation?
   https://courses.cs.duke.edu/cps216/fall16/Lectures/Lecture-21-Datalog.pdf |#
(define dl_fixpoint (lambda () (begin
    (set! dl_idb (make-hashmap))
    (dl_fixpoint_iterate))))

#| one iteration of fixpoint analysis |# 
(define dl_fixpoint_iterate (lambda ()
   (let ((new (hashmap-keys (set_difference (foldl (lambda (x y) (set-extend! y x)) (map dl_apply_rule dl_rdb) (make-hashmap)) dl_idb))))
     (set-extend! dl_idb new)
     (map dl_update_indices new)
     (if (not (null? new)) (dl_fixpoint_iterate)))))

#| TODO: assumes rule head AND each member of body is a predicate of arity/2
   find all pairs for which rule holds, this can include facts already explored
   rule is stored in db (and input to this function) as a list of predicates with vars
   the first item in the list is the head, the rest is the body |#
(define dl_apply_rule (lambda (rule)
   (let ((head (car rule))
         (body (cdr rule)))
     (eval `(dl_find ,head where ,body)))))

#| HELPER FUNCTIONS |#
(define set-extend! (lambda (m keys)
   (if (null? keys) m (begin (hashmap-set! m (car keys) #t) (set-extend! m (cdr keys))))))

#| NOTE: no 'not' in last clause, so can count double. db semantics probably prevent that |# 
(define membero (lambda (x l)
   (fresh (a d)
     (equalo (cons a d) l)
     (conde
       [(equalo a x)]
       [(membero x d)]))))

(define foldl (lambda (f l acc)
   (if (null? l) acc
     (foldl f (cdr l) (f (car l) acc)))))

#| NOTE: I dont care about order |#
(define append (lambda (a b)
   (if (null? b) a (append (cons (car b) a) (cdr b)))))

(define member? (lambda (l x)
   (cond
     [(null? l) #f]
     [(eqv? (car l) x) #t]
     [else (member? (cdr l) x)])))

(define list->set (lambda (x) (begin
   (define list->set_ (lambda (a b)
     (cond
       [(null? a) b]
       [(member? b (car a)) (list->set_ (cdr a) b)]
       [else (list->set_ (cdr a) (cons (car a) b))])))
   (list->set_ x '()))))

(define set_difference (lambda (a b) (begin
   (define check_keys (lambda (k m)
     (if (null? k) (make-hashmap) (let ((rec (check_keys (cdr k) m)))
       (if (not (hashmap-ref m (car k) #f)) (hashmap-set! rec (car k) #t))
       rec ))))
   (check_keys (hashmap-keys a) b))))

(define conso (lambda (a b l) (equalo (cons a b) l)))
#| NOTE: this breaks logical purity; we can no longer freely reorder goals |#
(define boundo (lambda (v)
    (lambda (s/c)
      (if (var? v)
        (let ((x (walk v (car s/c))))
          (if (var? x) mzero (unit s/c)))
        (unit s/c)))))
(define unboundo (lambda (v)
    (lambda (s/c)
      (if (var? v)
        (let ((x (walk v (car s/c))))
          (if (var? x) (unit s/c) mzero))
        mzero))))
(define lookupo (lambda (m key value)
    (lambda (s/c)
      (let ((k 
        (if (var? key)
          (walk key (car s/c))
          key)))
        (let ((v (hashmap-ref m k #f))) (if v ((equalo value (hashmap-keys v)) s/c) mzero))))))
