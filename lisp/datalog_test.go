package lisp

import (
	"sort"
	"testing"
)

func TestDatalog(t *testing.T) {
	main := newProcess()
	env := GlobalEnv()
	loadKanren(main, env)
	loadDatalog(main, env)
	for i, tt := range []struct {
		input string
		want  string
	}{
		{
			input: `(define dl_person (lambda (name born)
                      (dl_record 'person
                        ('name name)
                        ('born born))))`,
		},
		{
			input: `(begin
                      (define james_cameron (dl_person "James Cameron" "1954-08-16"))
                      (define john_mctiernan (dl_person "John McTiernan" "1951-01-08")))`,
		},
		{
			input: `(define dl_movie (lambda (title year director)
                      (dl_record 'movie
                        ('title title)
                        ('year year)
                        ('director director))))`,
		},
		{
			input: `(begin
                      (dl_movie "The Terminator" 1984 james_cameron)
                      (dl_movie "Predator" 1987 john_mctiernan))`,
			// begin returns id of last value, in this case 4 for Predator
			want: "4",
		},
		{
			input: `(dl_find ,?id where (
                    (,?id (movie year) 1987)
                ))`,
			want: "(4)",
		},
		{
			input: `(dl_find ,?directorName where (
                    (,?movieID (movie title) "The Terminator")
                    (,?movieID (movie director) ,?directorID)
                    (,?directorID (person name) ,?directorName)
                ))`,
			want: `("James Cameron")`,
		},
		{
			input: `(dl_find (,?directorName ,?year) where (
                    (,?movieID (movie title) "Predator")
                    (,?movieID (movie year) ,?year)
                    (,?movieID (movie director) ,?directorID)
                    (,?directorID (person name) ,?directorName)
                ))`,
			want: `(("John McTiernan" 1987))`,
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

func TestDatalogFixpoint(t *testing.T) {
	main := newProcess()
	env := GlobalEnv()
	loadKanren(main, env)
	loadDatalog(main, env)
	for i, tt := range []struct {
		input  string
		want   string
		sorted bool
	}{
		// awkardly this works, record without attrs allows us to refer to a new entityID
		// without any records being associated to it yet
		{
			input: `(begin
                (define a (dl_record 'vertex))
                (define b (dl_record 'vertex))
                (define c (dl_record 'vertex))
                (define d (dl_record 'vertex))
                (define e (dl_record 'vertex))
            )`,
		},
		{
			input: "(define dl_edge (lambda (x y) (dl_assert x 'edge y)))",
		},
		{
			input: `(begin
                (dl_edge a c)
                (dl_edge b a)
                (dl_edge b d)
                (dl_edge c d)
                (dl_edge d a)
                (dl_edge d e)
            )`,
		},
		{
			input: `(dl_find ,?id where (
                    (,b edge ,?id)
                ))`,
			want:   "(1 4)",
			sorted: true,
		},
		// TODO: at some point lets look at dl_assert and see if we can unify into one func
		{
			input: `(begin
                (dl_rule (reachable ,?x ,?y) :- (edge ,?x ,?y))
                (dl_rule (reachable ,?x ,?y) :- (edge ,?x ,?z) (reachable ,?z ,?y))
                (dl_fixpoint)
            )`,
		},
		{
			input: `(dl_find ,?id where (
                    (,?id reachable ,?id)
                ))`,
			want:   "(1 3 4)",
			sorted: true,
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
		if tt.sorted {
			list := cons2list(e.AsPair())
			sort.Slice(list, func(i, j int) bool {
				return list[i].AsNumber() < list[j].AsNumber()
			})
			e = list2cons(list...)
		}
		got := e.String()
		if got != tt.want {
			t.Errorf("%d) got %s want %s", i, got, tt.want)
		}
	}
}

func TestDatalogImpure(t *testing.T) {
	main := newProcess()
	env := GlobalEnv()
	loadKanren(main, env)
	loadDatalog(main, env)
	for i, tt := range []struct {
		input  string
		want   string
		sorted bool
	}{
		// first, lets check if we can figure out if a var is bound or not
		// NOTE: this breaks logical purity; we can no longer freely reorder goals
		{
			input: `(define boundo (lambda (v)
                (lambda (s/c)
                  (if (var? v)
                    (let ((x (walk v (car s/c))))
                      (if (var? x) mzero (unit s/c)))
                    (unit s/c))
            )))`,
		},
		{
			input: "(run* (fresh (q x) (equalo q x) (boundo x) (equalo x 42)))",
			want:  "()",
		},
		{
			input: "(run* (fresh (q x) (equalo q x) (equalo x 42) (boundo x)))",
			want:  "(42)",
		},
		{
			input: `(define unboundo (lambda (v)
                (lambda (s/c)
                  (if (var? v)
                    (let ((x (walk v (car s/c))))
                      (if (var? x) (unit s/c) mzero))
                    mzero)
            )))`,
		},
		// then lets write a relation using conde and var checking
		{
			input: `(define varswitcho (lambda (x y)
                      (conde 
                        [(boundo x) (equalo y 1)]
                        [(unboundo x) (equalo y 2)])))`,
		},
		{
			input: "(run* (fresh (q x y) (equalo q y) (varswitcho x y) (equalo x 42)))",
			want:  "(2)",
		},
		{
			input: "(run* (fresh (q x y) (equalo q y) (equalo x 42) (varswitcho x y)))",
			want:  "(1)",
		},
		// binds y to whatever v is bound to, or 'unbound if not bound
		{
			input: `(define boundo (lambda (v w)
                (lambda (s/c)
                  (if (var? v)
                    (let ((x (walk v (car s/c))))
                      (if (var? x) ((equalo w 'unbound) s/c) ((equalo w x) s/c)))
                    ((equalo w v) s/c))
            )))`,
		},
		{
			input: "(run* (fresh (q x y) (equalo q y) (boundo x y) (equalo x 42)))",
			want:  "(unbound)",
		},
		{
			input: "(run* (fresh (q x y) (equalo q y) (equalo x 42) (boundo x y)))",
			want:  "(42)",
		},
		// after that, we can write smth like 'if (car tuple) is bound, match against entry_index'
		// NOTE: looking a value up in a hashmap like this: definitely not logically pure!
		{
			input: "(define hm (make-hashmap))",
		},
		{
			input: "(hashmap-set! hm 1 2)",
		},
		{
			input: `(define lookupo (lambda (m key value)
                (lambda (s/c)
                  (let ((k 
                    (if (var? key)
                      (walk key (car s/c))
                      key)))
                    (let ((v (hashmap-ref m k #f))) (if v ((equalo value v) s/c) mzero))
            ))))`,
		},
		{
			input: "(run* (fresh (q x) (equalo q x) (lookupo hm 1 x)))",
			want:  "(2)",
		},
		{
			input: "(run* (fresh (q x y) (equalo q y) (equalo x 1) (lookupo hm x y)))",
			want:  "(2)",
		},
		{
			input: "(run* (fresh (q x y) (equalo q y) (lookupo hm x y) (equalo x 1)))",
			want:  "()",
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
		if tt.sorted {
			list := cons2list(e.AsPair())
			sort.Slice(list, func(i, j int) bool {
				return list[i].AsNumber() < list[j].AsNumber()
			})
			e = list2cons(list...)
		}
		got := e.String()
		if got != tt.want {
			t.Errorf("%d) got %s want %s", i, got, tt.want)
		}
	}
}
