(define REPL (lambda (env)
    (begin (display "> ")
        (display (eval (read) env))
        (display newline)
        (REPL env))))

(REPL (environment))
