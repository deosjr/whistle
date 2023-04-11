package main

import (
    "fmt"
    "math/rand"
)

// https://learnyousomeerlang.com/the-hitchhikers-guide-to-concurrency
// adding concurrency to our LISP based on actor model of Erlang,
// but implemented using Golang's CSP (which is different!)

// For this second attempt we will spawn each process with a copy of the env
// in which the process was spawned, and storing '$PID' on the process struct

type process struct {
    pid string
    mailbox chan SExpression
    // TODO: rand seed, etc
}

func newProcess() process {
    // erlang uses async message passing, which we can emulate using buffered channels
    // TODO: message box stores messages, matching starts over each time?
    // NOTE: choice of 1000 is completely arbitrary
    p := process{
        pid: pidFunc(),
        mailbox: make(chan SExpression, 1000),
    }
    processmap[p.pid] = p.mailbox
    return p
}

var processmap = make(map[string]chan SExpression)

// self/0, spawn/3, send/2, receive_msg/0, receive as macro, flush/0

func loadErlang(env *Env) {
    pid := pidFunc()
    processmap[pid] = make(chan SExpression, 1000)
    env.dict["self"]  = builtinFunc(self)
    env.dict["spawn"] = builtinFunc(spawn)
    env.dict["send"]  = builtinFunc(send)
    env.dict["flush"] = builtinFunc(flush)
    env.dict["receive_msg"] = builtinFunc(receive)
    // (receive (pattern expression) ...)
    // FIRST ATTEMPT: pattern only useful as constant, no unification with vars yet
    receiveMacro := `(syntax-rules (let receive_msg receive_)
                       ((_ (pattern expression) ...)
                        (let ((msg (receive_msg))) (receive_ msg (pattern expression) ...))))`
    receive2 := `(syntax-rules (eqv?)
                   ((_ _) #f)
                   ((_ msg (pattern expression) b ...)
                    (if (eqv? pattern msg) expression (receive_ msg b ...))))`
    macromap["receive"]   = syntaxRules("receive", parse(receiveMacro).AsPair())
    macromap["receive_"]  = syntaxRules("receive_", parse(receive2).AsPair())
}

var pidFunc func() string = generatePid

func generatePid() string {
    return "<pid" + fmt.Sprint(rand.Intn(9999999999)) + ">"
}

func self(p process, env *Env, args []SExpression) SExpression {
    return NewPrimitive(p.pid)
}

// (spawn module function (args ...))
// TODO: module argument not implemented right now
func spawn(spawning process, env *Env, args []SExpression) SExpression {
    p := newProcess()
    e := copyEnv(env)
    go p.evalEnv(e, NewPair(args[0], args[1]))
    return NewPrimitive(p.pid)
}

// (send to msg)
func send(p process, env *Env, args []SExpression) SExpression {
    ch := processmap[args[0].AsPrimitive().(string)]
    ch <- args[1]
    return args[1]
}

// receive_msg is the builtin func that actually receives a msg from a channel
func receive(p process, env *Env, args []SExpression) SExpression {
    return <-processmap[p.pid]
}

func flush(p process, env *Env, args []SExpression) SExpression {
    for {
        select {
        case msg := <-p.mailbox:
            fmt.Printf("%s got %s\n", p.pid, msg)
        default:
            return NewPrimitive(true)
        }
    }
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
