package main

import (
    "fmt"
    "math/rand"
)

// https://learnyousomeerlang.com/the-hitchhikers-guide-to-concurrency
// adding concurrency to our LISP based on actor model of Erlang,
// but implemented using Golang's CSP (which is different!)

// For this first attempt we will spawn each process with a copy of the env
// in which the process was spawned, and using '$PID' in env to store pid

var processmap map[string]chan SExpression

// self/0, spawn/3, send/2, receive_msg/0, receive as macro, flush/0

func loadErlang(env *Env) {
    processmap = make(map[string]chan SExpression)
    pid := pid()
    processmap[pid] = make(chan SExpression, 1000)
    env.dict["$PID"]  = NewSymbol(pid)
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

func pid() string {
    return "<pid" + fmt.Sprint(rand.Intn(9999999999)) + ">"
}

func self(env *Env, args []SExpression) SExpression {
    e, _ := env.find("$PID")
    return e.dict["$PID"]
}

// (spawn module function (args ...))
func spawn(env *Env, args []SExpression) SExpression {
    // erlang uses async message passing, which we can emulate using buffered channels
    // NOTE: choice of 1000 is completely arbitrary
    pid := pid()
    processmap[pid] = make(chan SExpression, 1000)
    e := copyEnv(env)
    d, _ := e.find("$PID")
    d.dict["$PID"] = NewSymbol(pid)
    go evalEnv(e, NewPair(args[0], args[1]))
    return NewSymbol(pid)
}

// (send to msg)
func send(env *Env, args []SExpression) SExpression {
    ch := processmap[args[0].AsSymbol()]
    ch <- args[1]
    return args[1]
}

// receive_msg is the builtin func that actually receives a msg from a channel
func receive(env *Env, args []SExpression) SExpression {
    pid := self(env, args).AsSymbol()
    return <-processmap[pid]
}

func flush(env *Env, args []SExpression) SExpression {
    pid := self(env, args).AsSymbol()
    ch := processmap[pid]
    for {
        select {
        case msg := <-ch:
            fmt.Printf("%s got %s\n", pid, msg)
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
