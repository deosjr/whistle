package main

// http://webyrd.net/scheme-2013/papers/HemannMuKanren2013.pdf

var kanren = []string{
    // instead of using vector, we just use numbers for vars
    "(define var (lambda (c) c))",
    "(define var? (lambda (x) (number? x)))",
    "(define var=? (lambda (x y) (= x y)))",

    // walk needs assp, but only cares about true/false
    `(define assp (lambda (proc list) 
       (if (null? list) #f 
         (let ((x (car list)))
           (cond
             ((proc (car x)) x)
             (else (assp proc (cdr list))))))))`,

    `(define walk (lambda (u s)
       (let ((pr (and (var? u) (assp (lambda (v) (var=? u v)) s))))
         (if pr (walk (cdr pr) s) u))))`,
    "(define ext-s (lambda (x v s) (cons (cons x v) s)))",

    `(define equalo (lambda (u v)
       (lambda (s/c)
         (let ((s (unify u v (car s/c))))
           (if s (unit (cons s (cdr s/c))) mzero)))))`,

    "(define unit (lambda (s/c) (cons s/c mzero)))",
    "(define mzero (quote ()))",

    `(define unify (lambda (u v s)
       (let ((u (walk u s)) (v (walk v s)))
         (cond
           ((and (var? u) (var? v) (var=? u v)) s)
           ((var? u) (ext-s u v s))
           ((var? v) (ext-s v u s))
           ((and (pair? u) (pair? v))
            (let ((s (unify (car u) (car v) s)))
              (and s (unify (cdr u) (cdr v) s))))
           (else (and (eqv? u v) s))))))`,

    `(define call/fresh (lambda (f)
       (lambda (s/c)
         (let ((c (cdr s/c)))
           ((f (var c)) (cons (car s/c) (+ c 1)))))))`,

    "(define disj (lambda (g1 g2) (lambda (s/c) (mplus (g1 s/c) (g2 s/c)))))",
    "(define conj (lambda (g1 g2) (lambda (s/c) (bind (g1 s/c) g2))))",

    `(define mplus (lambda (s1 s2)
       (cond
         ((null? s1) s2)
         ((procedure? s1) (lambda () (mplus s2 (s1))))
         (else (cons (car s1) (mplus (cdr s1) s2))))))`,

    `(define bind (lambda (s g)
       (cond
         ((null? s) mzero)
         ((procedure? s) (lambda () (bind (s) g)))
         (else (mplus (g (car s)) (bind (cdr s) g))))))`,

    "(define pull (lambda (s) (if (procedure? s) (pull (s)) s)))",
    `(define take-all (lambda (s)
       (let ((s (pull s)))
         (if (null? s) (quote ()) (cons (car s) (take-all (cdr s)))))))`,
    `(define take (lambda (n s)
       (if (= n 0) (quote ())
         (let ((s (pull s)))
           (cond
             ((null? s) (quote ()))
             (else (cons (car s) (take (- n 1) (cdr s)))))))))`,
}

func loadKanren(env Env) {
    for _, def := range kanren {
        evalEnv(parse(def), env)
    }
}
