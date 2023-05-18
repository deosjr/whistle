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
	} {
        // NOTE: to add director, we need to be able to refer to James Cameron by id
        {
            input: `(dl_record 'movie
                        ('title "The Terminator")
                        ('year  1984)
                        )`,
        },
        {
            input: `(dl_record 'movie
                        ('title "Predator")
                        ('year  1987)
                        )`,
        },
        // records start counting at 1, we only want Predator back
        {
            input: `(dl_find (?id) where (
                    (,?id (movie year) 1987)
                ))`,
            want:  "(2)",
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
