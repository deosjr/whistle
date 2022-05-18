package main

// http://webyrd.net/scheme-2013/papers/HemannMuKanren2013.pdf

var kanren = []string{
    // (define (var c) (vector c)) 
    "(define var (lambda (c) c))",
    // (define (var? x) (vector? x))
    "(define var? (lambda (x) (number? x)))",
    // (define (var=? x1 x2) (= (vector-ref x1 0) (vector-ref x2 0)))
    "(define var=? (lambda (x y) (= x y)))",

    // walk needs assp, but only cares about true/false
    `(define assp (lambda (proc list) 
       (if (null? list) #f 
         (let ((x (car list)))
           (cond
             ((proc (car x)) x)
             (else (assp proc (cdr list))))))))`,

    // (define (walk u s)
    //   (let ((pr (and (var? u) (assp (lambda (v) (var=? u v)) s))))
    //     (if pr (walk (cdr pr) s) u)))
    `(define walk (lambda (u s)
       (let ((pr (and (var? u) (assp (lambda (v) (var=? u v)) s))))
         (if pr (walk (cdr pr) s) u))))`,
    // (define (ext-s x v s) `((,x . ,v) . ,s))
    "(define ext-s (lambda (x v s) (cons (cons x v) s)))",

    // (define (equalo u v)
    //   (lambda_g (s/c)
    //     (let ((s (unify u v (car s/c))))
    //       (if s (unit `(,s . ,(cdr s/c))) mzero))))
    `(define equalo (lambda (u v)
       (lambda (s/c)
         (let ((s (unify u v (car s/c))))
           (if s (unit (cons s (paircdr s/c))) mzero)))))`,
    // (define (unit s/c) (cons s/c mzero))
    "(define unit (lambda (s/c) (cons s/c mzero)))",
    // (define mzero '())
    "(define mzero (quote ()))",

    // (define (unify u v s)
    //   (let ((u (walk u s)) (v (walk v s)))
    //     (cond
    //       ((and (var? u) (var? v) (var=? u v)) s)
    //       ((var? u) (ext-s u v s))
    //       ((var? v) (ext-s v u s))
    //       ((and (pair? u) (pair? v))
    //        (let ((s (unify (car u) (car v) s)))
    //          (and s (unify (cdr u) (cdr v) s))))
    //       (else (and (eqv? u v) s)))))
    `(define unify (lambda (u v s)
       (let ((u (walk u s)) (v (walk v s)))
         (cond
           ((and (var? u) (var? v) (var=? u v)) s)
           ((var? u) (ext-s u v s))
           ((var? v) (ext-s v u s))
           ((and (pair? u) (pair? v))
            (let ((s (unify (car u) (car v) s)))
              (and s (unify (paircdr u) (paircdr v) s))))
           (else (and (eqv? u v) s))))))`,

    // (define (call/fresh f)
    //   (lambda_g (s/c)
    //     (let ((c (cdr s/c)))
    //       ((f (var c)) `(,(car s/c) . ,(+ c 1))))))
    `(define call/fresh (lambda (f)
       (lambda (s/c)
         (let ((c (paircdr s/c)))
           ((f (var c)) (cons (car s/c) (+ c 1)))))))`,
}

func loadKanren(env Env) {
    for _, def := range kanren {
        evalEnv(parse(def), env)
    }
}
