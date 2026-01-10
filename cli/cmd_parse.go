package main

import (
	"fmt"
	"os"
	"path/filepath"
	"strconv"
	"strings"

	"github.com/aescarias/dessert/lang"
)

type ParseCmd struct {
	Input      string `arg:"" help:"Input file to process."`
	Definition string `arg:"" help:"Definition to process input with."`
}

func (p *ParseCmd) Run() error {
	defContents, err := os.ReadFile(p.Definition)
	if err != nil {
		return err
	}

	inputHandle, err := os.Open(p.Input)
	if err != nil {
		return err
	}
	defer inputHandle.Close()

	return ParseFileWithDefinition(inputHandle, p.Definition, defContents)
}

func ParseFileWithDefinition(inputHandle *os.File, defPath string, defContents []byte) error {
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
		return err
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
	return nil
}

// PrintDefinition prints out a list of items representing the parsed contents of a definition.
func PrintDefinition(items []lang.DefinitionItem, indent int) {
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
			PrintDefinition(children, indent+2)
		} else if arr, ok := it.Value.([]any); ok {
			children := []lang.DefinitionItem{}
			for idx, child := range arr {
				children = append(children, lang.DefinitionItem{Id: strconv.Itoa(idx), Value: child})
			}

			fmt.Printf("%s%s (%d):\n", tab(indent), label, len(children))
			PrintDefinition(children, indent+2)
		} else {
			fmt.Printf("%s%s: %v\n", tab(indent), label, it.Value)
		}
	}
}
