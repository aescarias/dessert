package lang

import (
	"fmt"
	"io"
	"strings"
)

func doError(kind ErrorKind, message string, args ...any) LangError {
	return LangError{Kind: kind, Message: fmt.Sprintf(message, args...)}
}

func printFn(w io.Writer) FuncResult {
	return func(r *Runtime, args []Result) (Result, error) {
		items := []string{}
		for _, arg := range args {
			items = append(items, fmt.Sprint(arg))
		}

		fmt.Fprintln(w, strings.Join(items, " "))

		return nil, nil
	}
}

func seekFn(fp io.ReadSeeker) FuncResult {
	return func(r *Runtime, args []Result) (Result, error) {
		if len(args) < 1 {
			return nil, doError(ErrorType, "seek expects at least %d argument(s), got %d", 1, len(args))
		}

		if len(args) > 2 {
			return nil, doError(ErrorType, "seek expects at most %d argument(s), got %d", 2, len(args))
		}

		offset, ok := args[0].(IntResult)
		if !ok {
			return nil, doError(ErrorType, "argument 1 must be integer")
		}

		var whence string
		if len(args) == 2 {
			whenceStr, ok := args[1].(StringResult)
			if !ok {
				return nil, doError(ErrorType, "argument 2 must be string")
			}
			whence = string(whenceStr)
		} else {
			whence = "start"
		}

		var whenceValue int

		switch whence {
		case "start":
			whenceValue = io.SeekStart
		case "current":
			whenceValue = io.SeekCurrent
		case "end":
			whenceValue = io.SeekEnd
		default:
			return nil, doError(ErrorValue, `argument 2 must be "start", "current", or "end", not %s`, whence)
		}

		value, _ := fp.Seek(offset.Int64(), whenceValue)

		return NewIntResult(value), nil
	}
}

func addStructBuiltins(r *Runtime, fp io.ReadSeeker, tw io.Writer) {
	r.Globals.Set("seek", seekFn(fp))
	r.Globals.Set("print", printFn(tw))
}
