package lisp

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

// TODO: this file is only partially migrated to erlang/erlang.go due to
// some very nasty dependencies on eval within receive!

type process struct {
	sync.Mutex
	pid     string
	mailbox []SExpression
	err     error
	// process flags
	trapExit             bool
	evalWithContinuation bool
	// TODO: rand seed, etc
}

type processError struct {
	err error
	pid string
	// if we encounter error and we are in evalK, we can restore
	// using value under evaluation (v) and continuation (k)
	k continuation
	v SExpression
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
						ch <- list2cons(NewSymbol("EXIT"), NewSymbol(perr.pid), NewPrimitive(perr.err.Error()))
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

func (p *process) Errorf(k continuation, v SExpression, s string, args ...any) SExpression {
	return NewPrimitive(processError{
		err: fmt.Errorf(s, args...),
		pid: p.pid,
		k:   k,
		v:   v,
	})
}

var mailboxes = make(map[string]chan SExpression)
var errchannels = make(map[string]chan processError)
var processlinks = make(map[string]map[string]struct{})

// TODO: move to erlang package, first need to figure out
// how to untangle the evals in receive builtin func
func LoadErlang(l Lisp) {
	p, env := l.process, l.Env
	env.addBuiltin("self", self)
	env.addBuiltin("send", send)
	env.addBuiltin("link", link)
	env.addBuiltin("unlink", unlink)
	env.addBuiltin("spawn", spawn)
	env.addBuiltin("spawn_link", spawnLink)
	env.addBuiltin("process_flag", processFlag)

	// receive as 2 macros AND a function call...
	env.addBuiltin("receive_builtin", receive)
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

// TODO: until this lives in erlang package
func SetPidFuncForTest() {
	pidi := 0
	pidFunc = func() string {
		pidi++
		return fmt.Sprintf("<%02d>", pidi)
	}
}

func generatePid() string {
	return "<pid" + fmt.Sprint(rand.Intn(999999999)) + ">"
}

func self(p *process, env *Env, args []SExpression) (SExpression, error) {
	return NewSymbol(p.pid), nil
}

// (spawn module function (args ...))
// TODO: module argument not implemented right now
func spawn(spawning *process, env *Env, args []SExpression) (SExpression, error) {
	p := newProcess()
	e := copyEnv(env)
	f := args[0]
	// TODO: I'm sure env can still sneak in via args[1], causing race conditions
	if f.IsProcedure() && !f.AsProcedure().isBuiltin {
		d := f.AsProcedure().defined()
		d.env = copyEnv(d.env)
		f = Proc{sexpression: sexpression{value: d}}
	}
	go eval(p, e, []SExpression{NewPair(f, args[1])})
	return NewSymbol(p.pid), nil
}

func link(p *process, env *Env, args []SExpression) (SExpression, error) {
	other := args[0].AsSymbol()
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
	f := args[0]
	// TODO: I'm sure env can still sneak in via args[1], causing race conditions
	if f.IsProcedure() && !f.AsProcedure().isBuiltin {
		d := f.AsProcedure().defined()
		d.env = copyEnv(d.env)
		f = Proc{sexpression: sexpression{value: d}}
	}
	go eval(p, e, []SExpression{NewPair(f, args[1])})
	return NewSymbol(p.pid), nil
}

func unlink(p *process, env *Env, args []SExpression) (SExpression, error) {
	other := args[0].AsSymbol()
	delete(processlinks[p.pid], other)
	delete(processlinks[other], p.pid)
	return NewPrimitive(true), nil
}

// (send to msg)
func send(p *process, env *Env, args []SExpression) (SExpression, error) {
	ch := mailboxes[args[0].AsSymbol()]
	ch <- args[1]
	return args[1], nil
}

type receiveClause struct {
	vars      []SExpression
	lambdaMsg Proc
	body      Pair
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
		var msgs int
		p.Lock()
		msgs = len(p.mailbox)
		p.Unlock()
		if msgs == 0 {
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
	b := args[1].AsPrimitive().(bool)
	switch flag {
	case "trap_exit":
		p.trapExit = b
	case "eval_with_continuation":
		p.evalWithContinuation = b
	default:
		return nil, fmt.Errorf("unknown process flag %s", flag)
	}
	return NewPrimitive(true), nil
}
