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

func ParseFileWithDefinition(inputHandle *os.File, defPath string, defContents []byte) {
	lx := lang.NewLexer(defContents)
	if err := lx.Process(); err != nil {
		lang.ReportError(defPath, defContents, err)
		os.Exit(ExitFailure)
	}

	ps := lang.NewParser(lx.Tokens)
	statements, err := ps.Parse()
	if err != nil {
		lang.ReportError(defPath, defContents, err)
		os.Exit(ExitFailure)
	}

	eval := lang.NewRuntime()
	results, err := eval.Run(statements)
	if err != nil {
		lang.ReportError(defPath, defContents, err)
		os.Exit(ExitFailure)
	}

	if err := lang.ParseFromReader(inputHandle, results); err != nil {
		fmt.Println(err)
		os.Exit(ExitFailure)
	}
}

func ShowAST(filename string, contents []byte) {
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
}

func ShowEvalBlocks(filename string, contents []byte) {
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

	eval := lang.NewRuntime()
	results, err := eval.Run(statements)
	if err != nil {
		lang.ReportError(filename, contents, err)
		os.Exit(ExitFailure)
	}

	for _, result := range results {
		fmt.Printf("%#v\n", result)
	}
}

func main() {
	if len(os.Args) < 2 {
		fmt.Println("usage: dessert [command] [options]")
		os.Exit(ExitUsage)
	}

	switch command := os.Args[1]; command {
	case "debug":
		if len(os.Args) < 4 {
			fmt.Println("usage: dessert debug [process] [definition file]")
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
		case "ast":
			ShowAST(filename, contents)
		case "eval":
			ShowEvalBlocks(filename, contents)
		default:
			fmt.Printf("unknown process %q for command debug\n", process)
			fmt.Printf("processes: ast, eval\n")
			os.Exit(ExitUsage)
		}
	case "parse":
		if len(os.Args) < 4 {
			fmt.Println("usage: dessert parse [input file] [definition file]")
			os.Exit(ExitUsage)
		}

		inputFile := os.Args[2]
		defPath := os.Args[3]

		defContents, err := os.ReadFile(defPath)
		if err != nil {
			fmt.Println(err)
			os.Exit(ExitFailure)
		}

		inputHandle, err := os.Open(inputFile)
		if err != nil {
			fmt.Println(err)
			os.Exit(ExitFailure)
		}

		ParseFileWithDefinition(inputHandle, defPath, defContents)
	default:
		fmt.Printf("unknown command %q\n", command)
		fmt.Printf("commands: debug, parse\n")
		os.Exit(ExitUsage)
	}
}
