package lisp

import (
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
			input: `(define james_cameron (dl_record 'person
                        ('name "James Cameron")
                        ('born "1954-08-16")))`,
		},
		{
			input: `(define john_mctiernan (dl_record 'person
                        ('name "John McTiernan")
                        ('born "1951-01-08")))`,
		},
		{
			input: `(dl_record 'movie
                        ('title "The Terminator")
                        ('year  1984)
                        ('director james_cameron))`,
			want: "3",
		},
		{
			input: `(dl_record 'movie
                        ('title "Predator")
                        ('year  1987)
                        ('director john_mctiernan))`,
			want: "4",
		},
		{
			input: `(dl_find (?id) where (
                    (,?id (movie year) 1987)
                ))`,
			want: "(4)",
		},
		// TODO: syntactic quirk: we have to tell find about all free variables
		// first in the list is returned as the answer
		// Should be able to inspect and add as free all symbols starting with '?' maybe
		{
			input: `(dl_find (?directorName ?directorID ?movieID) where (
                    (,?movieID (movie title) "The Terminator")
                    (,?movieID (movie director) ,?directorID)
                    (,?directorID (person name) ,?directorName)
                ))`,
			want: `("James Cameron")`,
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
