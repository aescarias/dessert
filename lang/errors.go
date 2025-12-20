package lang

import (
	"bytes"
	"fmt"
	"strings"
	"unicode"
)

type ErrorKind string

const (
	ErrorDomain  ErrorKind = "domain"  // Math domain errors such as division by zero.
	ErrorAccess  ErrorKind = "access"  // Access errors such as undefined reference access.
	ErrorRuntime ErrorKind = "runtime" // Runtime internal errors.
	ErrorSyntax  ErrorKind = "syntax"  // Syntax errors.
	ErrorType    ErrorKind = "type"    // Type errors (wrong type).
	ErrorValue   ErrorKind = "value"   // Value errors (correct type, wrong value).
)

type LangError struct {
	Kind     ErrorKind
	Position Position
	Message  string
}

func (le LangError) Error() string {
	return fmt.Sprintf("error(%s): %s", le.Kind, le.Message)
}

// ReportError prints an error report for a file at filepath with byte contents source
// containing details about err.
func ReportError(filepath string, source []byte, err error) {
	if lerr, ok := err.(LangError); ok {
		line, column, offset := 0, 0, 0
		var ch byte

		for offset, ch = range source {
			column += 1

			if ch == '\n' {
				line += 1
				column = 0
			}

			if offset >= lerr.Position.Start {
				break
			}
		}

		for idx, lineStr := range bytes.Split(bytes.TrimSuffix(source, []byte("\n")), []byte("\n")) {
			if idx == line {
				length := lerr.Position.End - lerr.Position.Start
				fmt.Printf("in %s:%d:%d-%d\n", filepath, line+1, column+1, column+1+length)
				fmt.Println(lerr)

				trimmed := strings.TrimLeftFunc(string(lineStr), unicode.IsSpace)
				diff := len(string(lineStr)) - len(trimmed)

				arrowAlign := max(column-diff-1, 0)
				fmt.Println("   ", trimmed)
				fmt.Println("   ", strings.Repeat(" ", arrowAlign)+strings.Repeat("^", length))
				break
			}
		}
	} else {
		fmt.Println(err)
	}
}
