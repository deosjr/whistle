package lisp

import "testing"

func TestLoader(t *testing.T) {
	l := New()
	l.Load("(define r 10)")

	for i, tt := range []struct {
		input string
		want  string
	}{
		{
			input: "(* pi (* r r))",
			want:  "314.1592653589793",
		},
	} {
		e, err := l.Eval(tt.input)
		if err != nil {
			t.Errorf("%d) eval error %v", i, err)
		}
		got := e.String()
		if got != tt.want {
			t.Errorf("%d) got %s want %s", i, got, tt.want)
		}
	}
}
