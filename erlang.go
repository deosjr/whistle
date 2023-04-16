package main

import (
    "fmt"
    "math/rand"
    "sync"
    "time"
)

// https://learnyousomeerlang.com/the-hitchhikers-guide-to-concurrency
// adding concurrency to our LISP based on actor model of Erlang,
// but implemented using Golang's CSP (which is different!)

// For this second attempt we will spawn each process with a copy of the env
// in which the process was spawned, and storing '$PID' on the process struct

type process struct {
    sync.Mutex
    pid string
    mailbox []SExpression
    err   error
    // process flags
    trapExit bool
    // TODO: rand seed, etc
}

type processError struct {
    err error
    pid string
}

func newProcess() *process {
    p := &process{
        pid: pidFunc(),
    }
    ch := make(chan SExpression)
    mailboxes[p.pid] = ch
    errch := make(chan processError)
    errchannels[p.pid] = errch
    processlinks[p.pid] = map[string]struct{}{}
    go func() {
        for {
            select {
            case msg := <-ch:
                p.Lock()
                p.mailbox = append(p.mailbox, msg)
                p.Unlock()
            case perr := <-errch:
                if p.trapExit && perr.pid != p.pid {
                    go func() {
                        ch <- list2cons(NewPrimitive("EXIT"), NewPrimitive(perr.pid), NewPrimitive(perr.err.Error()))
                    }()
                    continue
                }
                p.err = perr.err
                for link := range processlinks[p.pid] {
                    errchannels[link] <- perr
                }
                return
            }
        }
    }()
    return p
}

var mailboxes = make(map[string]chan SExpression)
var errchannels = make(map[string]chan processError)
var processlinks = make(map[string]map[string]struct{})

func loadErlang(p *process, env *Env) {
    env.dict["self"]  = builtinFunc(self)
    env.dict["spawn"] = builtinFunc(spawn)
    env.dict["send"]  = builtinFunc(send)
    env.dict["link"]  = builtinFunc(link)
    env.dict["spawn_link"] = builtinFunc(spawnLink)
    env.dict["unlink"] = builtinFunc(unlink)
    env.dict["process_flag"] = builtinFunc(processFlag)
    // receive as 2 macros AND a function call...
    env.dict["receive_builtin"] = builtinFunc(receive)
    macromap["receive"] = syntaxRules("receive", mustParse(`(syntax-rules (receive_builtin receive_ after ->)
        ((_ ((v ...) rest ... ) ... (after millis -> expression ...))
         (receive_builtin (quote (after millis expression ...)) (receive_ (v ...) rest ...) ...))
        ((_ ((v ...) rest ... ) ...)
         (receive_builtin (receive_ (v ...) rest ...) ...)))`).AsPair())
    macromap["receive_"] = syntaxRules("receive_", mustParse(`(syntax-rules (when -> run fresh equalo)
        ((_ (vars ...) pattern -> expression ...)
         (quasiquote ((vars ...) (unquote (lambda (msg)
           (run 1 (fresh (q vars ...) (equalo q (quasiquote ((unquote vars) ...))) (equalo msg pattern) )))) expression ...)))
        ((_ (vars ...) pattern (when guard ...) -> expression ...)
         (quasiquote ((vars ...) (unquote (lambda (msg)
           (run 1 (fresh (q vars ...) (equalo q (quasiquote ((unquote vars) ...))) (equalo msg pattern) guard ...)))) expression ...))))`).AsPair())

    // these "builtins" can be defined in code now
    p.evalEnv(env, mustParse(`(define flush (lambda () (receive ((x) x ->
        (display (self)) (display " got ") (display x) (display newline) (flush))
        (after 0 -> #t))))`))
    p.evalEnv(env, mustParse("(define sleep (lambda (t) (receive (after t -> #t))))"))
}

var pidFunc func() string = generatePid

func generatePid() string {
    return "<pid" + fmt.Sprint(rand.Intn(9999999999)) + ">"
}

func self(p *process, env *Env, args []SExpression) (SExpression, error) {
    return NewPrimitive(p.pid), nil
}

// (spawn module function (args ...))
// TODO: module argument not implemented right now
func spawn(spawning *process, env *Env, args []SExpression) (SExpression, error) {
    p := newProcess()
    e := copyEnv(env)
    go eval(p, e, []SExpression{NewPair(args[0], args[1])})
    return NewPrimitive(p.pid), nil
}

func link(p *process, env *Env, args []SExpression) (SExpression, error) {
    other := args[0].AsPrimitive().(string)
    processlinks[p.pid][other] = struct{}{}
    processlinks[other][p.pid] = struct{}{}
    return NewPrimitive(true), nil
}

func spawnLink(spawning *process, env *Env, args []SExpression) (SExpression, error) {
    // link before even kicking off the process so we are sure to be updated
    p := newProcess()
    e := copyEnv(env)
    processlinks[spawning.pid][p.pid] = struct{}{}
    processlinks[p.pid][spawning.pid] = struct{}{}
    go eval(p, e, []SExpression{NewPair(args[0], args[1])})
    return NewPrimitive(p.pid), nil
}

func unlink(p *process, env *Env, args []SExpression) (SExpression, error) {
    other := args[0].AsPrimitive().(string)
    delete(processlinks[p.pid], other)
    delete(processlinks[other], p.pid)
    return NewPrimitive(true), nil
}

// (send to msg)
func send(p *process, env *Env, args []SExpression) (SExpression, error) {
    ch := mailboxes[args[0].AsPrimitive().(string)]
    ch <- args[1]
    return args[1], nil
}

type receiveClause struct {
    vars []SExpression
    lambdaMsg Proc
    body Pair
}

// (receive ((vars ...) pattern [(when guard ...)] -> expression ... ) ... [(after duration expression ...)])
func receive(p *process, env *Env, args []SExpression) (SExpression, error) {
    first := args[0].AsPair()
    var after time.Duration
    expires := false
    var afterBody Pair
    start := time.Now()
    if first.car().IsSymbol() && first.car().AsSymbol() == "after" {
        evalDuration, err := p.evalEnv(env, first.cadr())
        if err != nil {
            return nil, err
        }
        after = time.Millisecond * time.Duration(evalDuration.AsNumber())
        expires = true
        afterBody = first.cddr().AsPair()
        args = args[1:]
    }
    clauses := []receiveClause{}
    for _, arg := range args {
        parg := arg.AsPair()
        vars := cons2list(parg.car().AsPair())
        lambda := parg.cadr().AsProcedure()
        body := parg.cddr().AsPair()
        clauses = append(clauses, receiveClause{vars, lambda, body})
    }
    seen := []SExpression{}
    for {
        if len(p.mailbox) == 0 {
            // TODO: hot loop :(
            if expires && time.Now().Sub(start) > after {
                p.Lock()
                p.mailbox = append(seen, p.mailbox...)
                p.Unlock()
                return p.evalEnv(env, NewPair(NewSymbol("begin"), afterBody))
            }
            continue
        }
        var msg SExpression
        p.Lock()
        msg, p.mailbox = p.mailbox[0], p.mailbox[1:]
        p.Unlock()
        qmsg := list2cons(NewSymbol("quote"), msg)
        for _, clause := range clauses {
            pmatch, err := p.evalEnv(env, list2cons(clause.lambdaMsg, qmsg))
            match := pmatch.AsPair()
            if err != nil {
                return nil, err
            }
            if match == empty {
                continue
            }
            p.Lock()
            p.mailbox = append(seen, p.mailbox...)
            p.Unlock()
            // execute clause.body matching vars!
            // (let (zip vars match) body ...)
            zip := make([]SExpression, len(clause.vars))
            for i, m := range cons2list(match.car().AsPair()) {
                zip[i] = list2cons(clause.vars[i], list2cons(NewSymbol("quote"), m))
            }
            let := NewPair(NewSymbol("let"), NewPair(list2cons(zip...), clause.body))
            return p.evalEnv(env, let)
        }
        seen = append(seen, msg)
    }
}

func processFlag(p *process, env *Env, args []SExpression) (SExpression, error) {
    flag := args[0].AsSymbol()
    if flag != "trap_exit" {
        return nil, fmt.Errorf("unknown process flag %s", flag)
    }
    b := args[1].AsPrimitive().(bool)
    p.trapExit = b
    return NewPrimitive(true), nil
}

func copyEnv(env *Env) *Env {
    if env == nil {
        return nil
    }
    m := map[Symbol]SExpression{}
    for k, v := range env.dict {
        m[k] = v
    }
    return &Env{dict: m, outer: copyEnv(env.outer)}
}
