package main

import (
	"fmt"
	"os"
	"path/filepath"
	"strconv"
	"strings"

	"github.com/aescarias/dessert/lang"
)

const (
	ExitFailure int = 1
	ExitUsage   int = 2
)

func printDefinition(items []lang.DefinitionItem, indent int) {
	tab := func(n int) string { return strings.Repeat(" ", indent) }

	for _, it := range items {
		label := it.Name
		if label == "" {
			label = it.Id
		}

		if byteStr, ok := it.Value.([]byte); ok {
			fmt.Printf("%s%s: %q\n", tab(indent), label, byteStr)
		} else if children, ok := it.Value.([]lang.DefinitionItem); ok {
			fmt.Printf("%s%s:\n", tab(indent), label)
			printDefinition(children, indent+2)
		} else if arr, ok := it.Value.([]any); ok {
			children := []lang.DefinitionItem{}
			for idx, child := range arr {
				children = append(children, lang.DefinitionItem{Id: strconv.Itoa(idx), Value: child})
			}

			fmt.Printf("%s%s (%d):\n", tab(indent), label, len(children))
			printDefinition(children, indent+2)
		} else {
			fmt.Printf("%s%s: %v\n", tab(indent), label, it.Value)
		}
	}

}

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

	printDefinition(def.Items, 0)
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
