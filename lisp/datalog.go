package lisp

var datalog = []string{
	// dl_db is a global db to query (not supporting multiple dbs/db as arg)
	// for starters, this will be a simple list of EAV tuples
	"(define dl_db (quote ()))",

	// dl_nextID is a closure around an ever-increasing counter
	// TODO: proper closure hiding dl_counter instead of this global nonsense
	"(define dl_counter 0)",
	`(define dl_nextID (lambda () (begin
      (set! dl_counter (+ dl_counter 1))
      dl_counter)))`,

	// dl_assert and dl_retract operate on the global db
	// inserting datoms, 3-value tuples (im ignoring tx, the 4th in EAVT)
	// TODO: currently asserta, when we expect assertz, due to cons
    // TODO: what if you assert smth that has already been inserted?
	`(define dl_assert (lambda (entity attr value)
      (set! dl_db (cons (list entity attr value) dl_db))))`,
	// TODO: retract

	// dl_record is a convenience macro to add structured data
	// returns id of entry added
	`(define-syntax dl_record
       (syntax-rules (list let dl_nextID dl_assert)
         ((_ type (attr value) ...) (let ((id (dl_nextID)))
           (dl_assert id (list type attr) value) ... id))))`,

    // NOTE: theres a lot going on in this macro.
    // dl_vars looks for all vars recursively in a list starting with ?
    // when we want to run minikanren we need to explicitly name all vars using 'fresh'
    // TODO: set difference used to dedup dl_vars since we introduce (x) separately
    // no longer needed once we use a full pattern instead of (x), smth like (equalo q p) where p = `(,?id ?name)
    // NOTE: using more dynamic fresh, not as macro, able to take an interpreted list of varnames
    // if there is a way to do this in the macro system, I havent found it yet (eval hacking instead..)
    // TODO: add unquote to every dl_var istance so we dont have to type it in dl_find input
	`(define-syntax dl_find
       (syntax-rules (where run* equalo membero dl_db dl_vars let set_difference cons fresh eval)
         ((_ (x) where ( match ... ))
          (let ((vars (set_difference (dl_vars (quote (match ...))) (quote (x)))))
            (run* (eval (cons 'fresh (cons (cons 'q (cons 'x vars)) (quote ((equalo q x) (membero (quasiquote match) dl_db) ...))))))))))`,

    `(define dl_var? (lambda (s) (if (symbol? s) (prefix? (symbol->string s) "?"))))`,

    `(define dl_vars (lambda (l)
       (list->set (foldl (lambda (x acc)
         (cond
           [(dl_var? x) (cons x acc)]
           [(pair? x) (append (dl_vars x) acc)]
           [else acc])) l (quote ())))))`,

	// then rules will be the interesting bit, where we'll start with naive eval

    // TODO: introduce a rule into the (separate!) rule db
    // define dl_rule

    // TODO: manually trigger fixpoint analysis, repeatedly running rules against all known entities
    // define dl_fixpoint

    // HELPER FUNCTIONS

	// NOTE: no 'not' in last clause, so can count double. db semantics probably prevent that
	`(define membero (lambda (x l)
       (fresh (a d)
         (equalo (cons a d) l)
         (conde
           [(equalo a x)]
           [(membero x d)]))))`,

    `(define foldl (lambda (f l acc)
       (if (null? l) acc
         (foldl f (cdr l) (f (car l) acc)))))`,

    // NOTE: I dont care about order
    `(define append (lambda (a b)
       (if (null? b) a (append (cons (car b) a) (cdr b)))))`,

    `(define member? (lambda (l x)
       (cond
         [(null? l) #f]
         [(eqv? (car l) x) #t]
         [else (member? (cdr l) x)])))`,

    `(define list->set (lambda (x) (begin
       (define list->set_ (lambda (a b)
         (cond
           [(null? a) b]
           [(member? b (car a)) (list->set_ (cdr a) b)]
           [else (list->set_ (cdr a) (cons (car a) b))])))
       (list->set_ x (quote ())))))`,

    `(define set_difference (lambda (a b)
       (cond
         [(null? a) (quote ())]
         [(member? b (car a)) (set_difference (cdr a) b)]
         [else (cons (car a) (set_difference (cdr a) b))])))`,
}

func loadDatalog(p *process, env *Env) {
	for _, def := range datalog {
		p.evalEnv(env, mustParse(def))
	}
}
