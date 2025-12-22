package main

import (
	"fmt"
	"os"

	"github.com/aescarias/dessert/lang"
)

const (
	ExitFailure int = 1
	ExitUsage   int = 2
)

func main() {
	if len(os.Args) < 2 {
		fmt.Println("usage: dessert [command] [options]")
		os.Exit(ExitUsage)
	}

	switch command := os.Args[1]; command {
	case "debug":
		if len(os.Args) < 4 {
			fmt.Println("usage: dessert debug [process] [filename]")
			os.Exit(ExitUsage)
		}

		process := os.Args[2]
		filename := os.Args[3]

		contents, err := os.ReadFile(filename)
		if err != nil {
			fmt.Println(err)
			os.Exit(ExitFailure)
		}

		switch process {
		case "tokens":
			lx := lang.NewLexer(contents)
			if err := lx.Process(); err != nil {
				lang.ReportError(filename, contents, err)
				os.Exit(ExitFailure)
			}

			for _, tok := range lx.Tokens {
				fmt.Printf("%s %q %d..%d\n", tok.Kind, tok.Value, tok.Position.Start, tok.Position.End)
			}
		case "ast":
			lx := lang.NewLexer(contents)
			if err := lx.Process(); err != nil {
				lang.ReportError(filename, contents, err)
				os.Exit(ExitFailure)
			}

			ps := lang.NewParser(lx.Tokens)
			statements, err := ps.Parse()
			if err != nil {
				lang.ReportError(filename, contents, err)
				os.Exit(ExitFailure)
			}

			for _, stmt := range statements {
				ShowSyntaxTree(stmt, 0)
			}
		default:
			fmt.Printf("unknown process %q\n", process)
			fmt.Printf("processes: ast, tokens")
			os.Exit(ExitUsage)
		}

	default:
		fmt.Printf("unknown command %q\n", command)
		fmt.Printf("commands: debug\n")
		os.Exit(ExitUsage)
	}
}
