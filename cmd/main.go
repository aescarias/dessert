package main

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"

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

	def, err := lang.ParseFromReader(inputHandle, results)
	if err != nil {
		fmt.Println(err)
		os.Exit(ExitFailure)
	}

	fmt.Printf("%s - dessert %s\n", filepath.Base(defPath), def.Dessert)
	fmt.Printf("name: %s\n", def.Name)
	if len(def.Mime) > 0 {
		fmt.Printf("mime: %s\n", strings.Join(def.Mime, ", "))
	}
	if len(def.Extensions) > 0 {
		fmt.Printf("exts: %s\n", strings.Join(def.Extensions, ", "))
	}
	if def.Docs != "" {
		fmt.Printf("docs:\n%s\n", def.Docs)
	}

	fmt.Printf("---\n")

	PrintDefinition(def.Items, 0)
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

func ShowStatements(filename string, contents []byte) {
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

	rt := lang.NewRuntime()
	results, err := rt.Run(statements)
	if err != nil {
		lang.ReportError(filename, contents, err)
		os.Exit(ExitFailure)
	}

	for _, res := range results {
		fmt.Printf("%#v\n", res)
	}
}

func main() {
	if len(os.Args) < 2 {
		fmt.Println("usage: dessert [command] [options]")
		os.Exit(ExitUsage)
	}

	switch command := os.Args[1]; command {
	case "ast":
		if len(os.Args) < 3 {
			fmt.Println("usage: dessert ast [filename]")
			os.Exit(ExitUsage)
		}

		filename := os.Args[2]

		contents, err := os.ReadFile(filename)
		if err != nil {
			fmt.Println(err)
			os.Exit(ExitFailure)
		}

		ShowAST(filename, contents)
	case "results":
		if len(os.Args) < 3 {
			fmt.Println("usage: dessert results [filename]")
			os.Exit(ExitUsage)
		}

		filename := os.Args[2]

		contents, err := os.ReadFile(filename)
		if err != nil {
			fmt.Println(err)
			os.Exit(ExitFailure)
		}

		ShowStatements(filename, contents)
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
		fmt.Printf("commands: ast, parse\n")
		os.Exit(ExitUsage)
	}
}
