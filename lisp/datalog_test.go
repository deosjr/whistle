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
