package main

import (
    "fmt"
    "math/rand"
    "sync"
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
    // TODO: rand seed, etc
}

func newProcess() *process {
    p := &process{
        pid: pidFunc(),
    }
    ch := make(chan SExpression)
    processmap[p.pid] = ch
    go func() {
        for msg := range ch {
            p.Lock()
            p.mailbox = append(p.mailbox, msg)
            p.Unlock()
        }
    }()
    return p
}

var processmap = make(map[string]chan SExpression)

// self/0, spawn/3, send/2, receive/*, flush/0

func loadErlang(env *Env) {
    env.dict["self"]  = builtinFunc(self)
    env.dict["spawn"] = builtinFunc(spawn)
    env.dict["send"]  = builtinFunc(send)
    env.dict["flush"] = builtinFunc(flush)
    env.dict["receive_builtin"] = builtinFunc(receive)
    macromap["receive"] = syntaxRules("receive", parse(`(syntax-rules (receive_builtin receive_)
        ((_ clause ...) (receive_builtin (receive_ clause) ...)))`).AsPair())
    macromap["receive_"] = syntaxRules("receive_", parse(`(syntax-rules (when -> run fresh equalo)
        ((_ ((vars ...) pattern -> expression ...))
         (quasiquote ((vars ...) (unquote (lambda (msg) (run 1 (fresh (vars ... q) (equalo q pattern) (equalo q msg) )))) expression ...)))
        ((_ ((vars ...) pattern (when guard ...) -> expression ...))
         (quasiquote ((vars ...) (unquote (lambda (msg) (run 1 (fresh (vars ... q) (equalo q pattern) (equalo q msg) guard ...)))) expression ...))))`).AsPair())
}

var pidFunc func() string = generatePid

func generatePid() string {
    return "<pid" + fmt.Sprint(rand.Intn(9999999999)) + ">"
}

func self(p *process, env *Env, args []SExpression) SExpression {
    return NewPrimitive(p.pid)
}

// (spawn module function (args ...))
// TODO: module argument not implemented right now
func spawn(spawning *process, env *Env, args []SExpression) SExpression {
    p := newProcess()
    e := copyEnv(env)
    go p.evalEnv(e, NewPair(args[0], args[1]))
    return NewPrimitive(p.pid)
}

// (send to msg)
func send(p *process, env *Env, args []SExpression) SExpression {
    ch := processmap[args[0].AsPrimitive().(string)]
    ch <- args[1]
    return args[1]
}

type receiveClause struct {
    vars []SExpression
    lambdaMsg Proc
    body Pair
}

// (receive ((vars ...) pattern [(when guard ...)] -> expression ... ) ... [(after duration -> expression ...)])
func receive(p *process, env *Env, args []SExpression) SExpression {
    // TODO: after
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
            continue
        }
        var msg SExpression
        p.Lock()
        msg, p.mailbox = p.mailbox[0], p.mailbox[1:]
        p.Unlock()
        msg = list2cons(NewSymbol("quote"), msg)
        for _, clause := range clauses {
            match := p.evalEnv(env, list2cons(clause.lambdaMsg, msg)).AsPair()
            if match == empty {
                continue
            }
            p.Lock()
            p.mailbox = append(seen, p.mailbox...)
            p.Unlock()
            // execute clause.body matching vars!
            // (let (zip vars match) body ...)
            zip := make([]SExpression, len(clause.vars))
            for i, m := range cons2list(match) {
                zip[i] = list2cons(clause.vars[i], list2cons(NewSymbol("quote"), m))
            }
            let := NewPair(NewSymbol("let"), NewPair(list2cons(zip...), clause.body))
            return p.evalEnv(env, let)
        }
        seen = append(seen, msg)
    }
}

func flush(p *process, env *Env, args []SExpression) SExpression {
    p.Lock()
    mailbox := p.mailbox
    p.mailbox = []SExpression{}
    p.Unlock()
    for _, msg := range mailbox {
        fmt.Printf("%s got %s\n", p.pid, msg)
    }
    return NewPrimitive(true)
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
