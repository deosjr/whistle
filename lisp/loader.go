package lisp

// Load a string of lisp code/data into the environment.
func (l Lisp) Load(data string) error {
	sexprs, err := Multiparse(data)
	if err != nil {
		return err
	}
	for _, def := range sexprs {
		_, err := l.EvalExpr(def)
		if err != nil {
			return err
		}
	}

	return nil
}
