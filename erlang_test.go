package main

import (
    "fmt"
    "testing"
    "time"
)

// https://learnyousomeerlang.com/more-on-multiprocessing
func TestErlangReceiveMacro(t *testing.T) {
    pidi := 0
    pidFunc = func() string {
        pidi++
        return fmt.Sprintf("<%02d>", pidi)
    }
    main := newProcess()
	env := GlobalEnv()
	loadErlang(main, env)
    loadKanren(main, env)
	for i, tt := range []struct {
		input string
		want  string
	} {
        {
            input: "(self)",
            want:  "<01>",
        },
        {
            input: `(define rec (lambda (sender)
                        (receive
                          ((priority) (quasiquote (,priority 'request)) (when (equalo priority 1)) ->
                            (send sender (quote ('high 'response)))
                            (rec sender))
                          ((priority) (quasiquote (,priority 'request)) (when (equalo priority 2)) ->
                            (send sender (quote ('normal 'response)))
                            (rec sender))
                          ((priority) (quasiquote (,priority 'request)) (when (equalo priority 3)) ->
                            (send sender (quote ('low 'response)))
                            (rec sender))
                        )))`,
        },
        {
            input: `(define pid (let ((this (self)))
                      (spawn rec (quote (this)))))`,
        },
        {
            // doesnt match anything, ignored in mailbox!
            input: "(send pid (quote (4 'request)))",
            want:  "(4 (quote request))",
        },
        {
            input: "(send pid (quote (2 'request)))",
            want:  "(2 (quote request))",
        },
        {
            input: "(receive ((x) (quasiquote (,x 'response)) -> x))",
            want:  "(quote normal)",
        },
        {
            input: `(define important (lambda ()
                      (receive 
                        ((priority message) (quasiquote (,priority ,message)) (when (equalo priority 1)) ->
                          (cons message (important)))
                        (after 0 -> (normal)))))`,
        },
        {
            input: `(define normal (lambda ()
                      (receive 
                        ((priority message) (quasiquote (,priority ,message)) (when (equalo priority 3)) ->
                          (cons message (normal)))
                        (after 0 -> (quote ()) )))))`,
        },
        {
            input: "(begin (send (self) (quote (1 high))) (send (self) (quote (3 low))) (send (self) (quote (3 low))) (send (self) (quote (1 high))))",
            want:  "(1 high)",
        },
        {
            input: "(important)",
            want:  "(high high low low)",
        },
    } {
        p, err := parse(tt.input)
        if err != nil {
            t.Errorf("%d) parse error %v", i, err)
        }
        e, err := main.evalEnv(env, p)
        if err != nil {
            t.Errorf("%d) eval error %v", i, err)
        }
		got := e.String()
		if got != tt.want {
			t.Errorf("%d) got %s want %s", i, got, tt.want)
		}
    }
}

// https://learnyousomeerlang.com/errors-and-processes
func TestErlangExit(t *testing.T) {
    if testing.Short() {
        t.Skip()
    }
    pidi := 0
    pidFunc = func() string {
        pidi++
        return fmt.Sprintf("<%02d>", pidi)
    }
    main := newProcess()
	env := GlobalEnv()
	loadErlang(main, env)
    loadKanren(main, env)
	for i, tt := range []struct {
		input   string
		want    string
        wantErr string
        wait    bool
	} {
        {
            input: "(define myproc (lambda () (begin (sleep 1000) (exit 'reason))))",
        },
        {
            input: "(spawn myproc (quote ()))",
            want:  "<02>",
            wait:  true,
        },
        {
            input:   "(spawn_link myproc (quote ()))",
            want:    "<03>",
            wait:    true,
            wantErr: "** exception error: reason",
        },
        {
            input: `(define chain (lambda (n)
                      (if (eqv? n 0) (receive ((x) x -> #t) (after 500 -> (exit "chain dies here")))
                      (let ((pid (spawn (lambda () (chain (- n 1))) (quote ()) ))) (link pid) (receive ((x) x -> #t))))))`,
        },
        {
            input: "(spawn_link chain (quote (3)))",
            want:    "<05>", // <04> is the restarted main proc after last error
            wait:    true,
            wantErr: `** exception error: "chain dies here"`,
        },
        {
            input: "(process_flag 'trap_exit #t)",
        },
        {
            input: "(spawn_link chain (quote (3)))",
            want:  "<10>", // previous chain x3 + another main restart, so main is <09> now
            wait:  true,
        },
        {
            input: "(receive ((x) x -> x))",
            want:  `(EXIT <13> "chain dies here")`,
        },
    } {
        p, err := parse(tt.input)
        if err != nil {
            t.Errorf("%d) parse error %v", i, err)
        }
        e, err := main.evalEnv(env, p)
        if err != nil {
            t.Errorf("%d) eval error %v", i, err)
        }
		got := e.String()
		if got != tt.want {
			t.Errorf("%d) got %s want %s", i, got, tt.want)
		}
        if tt.wait {
            time.Sleep(3 * time.Second)
        }
        perr := main.err
        if perr == nil {
            perr = fmt.Errorf("")
        }
        if perr.Error() != tt.wantErr {
			t.Errorf("%d) got error %s want %s", i, perr.Error(), tt.wantErr)
        }
        if tt.wantErr != "" {
            // restart the main process, error will have caused it to die
            main = newProcess()
        }
    }
}
