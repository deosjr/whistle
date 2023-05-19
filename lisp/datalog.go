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
	`(define dl_assert (lambda (entity attr value)
      (set! dl_db (cons (list entity attr value) dl_db))))`,
	// TODO: retract

	// dl_record is a convenience macro to add structured data
	// returns id of entry added
	`(define-syntax dl_record
       (syntax-rules (list let dl_nextID dl_assert)
         ((_ type (attr value) ...) (let ((id (dl_nextID)))
           (dl_assert id (list type attr) value) ... id))))`,

	// dl_find is probably a macro?
	// for starters, will do smth like foreach membero dl_db
	// NOTE: no 'not' in last clause, so can count double. db semantics probably prevent that
	`(define membero (lambda (x l)
       (fresh (a d)
         (equalo (cons a d) l)
         (conde
           [(equalo a x)]
           [(membero x d)]))))`,

	`(define-syntax dl_find
       (syntax-rules (where run fresh membero dl_db)
         ((_ (x ...) where ( match ... ))
          (run 1 (fresh (x ...) (membero (quasiquote match) dl_db) ...)))))`,

	// then rules will be the interesting bit, where we'll start with naive eval
}

func loadDatalog(p *process, env *Env) {
	for _, def := range datalog {
		p.evalEnv(env, mustParse(def))
	}
}
