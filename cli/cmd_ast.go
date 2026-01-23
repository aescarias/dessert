package main

import (
	"fmt"
	"os"
	"strings"

	"github.com/aescarias/dessert/lang"
)

type AstCmd struct {
	Input string `arg:"" help:"Filepath of the definition to parse."`
}

func (a *AstCmd) Run() error {
	contents, err := os.ReadFile(a.Input)
	if err != nil {
		return err
	}

	ShowAST(a.Input, contents)
	return nil
}

// ShowAST takes a filename and the contents of the definition file and prints
// an abstract syntax tree from the parsed statements.
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

// ShowSyntaxTree prints the abstract syntax tree (AST) defined by node starting
// at the specified indent level.
func ShowSyntaxTree(node lang.Node, indent int) {
	tab := func(n int) string { return strings.Repeat(" ", n) }

	switch node.Type() {
	case lang.NodeBinOp:
		binOp := node.(*lang.BinOpNode)
		fmt.Printf("%s- %s (%s)\n", tab(indent), binOp.Type(), binOp.Op.Value)

		ShowSyntaxTree(binOp.Left, indent+1)
		ShowSyntaxTree(binOp.Right, indent+1)
	case lang.NodeUnaryOp:
		unaryOp := node.(*lang.UnaryOpNode)
		fmt.Printf("%s- %s (%s)\n", tab(indent), unaryOp.Type(), unaryOp.Op.Value)

		ShowSyntaxTree(unaryOp.Node, indent+1)
	case lang.NodeLiteral:
		litNode := node.(*lang.LiteralNode)

		fmt.Printf("%s- %s (%s: %q)\n", tab(indent), litNode.Type(), litNode.Token.Kind, litNode.Token.Value)
	case lang.NodeMap:
		mapNode := node.(*lang.MapNode)

		fmt.Printf("%s- %s\n", tab(indent), mapNode.Type())

		for key, value := range mapNode.Items {
			ShowSyntaxTree(key, indent+1)
			ShowSyntaxTree(value, indent+2)
		}
	case lang.NodeList:
		listNode := node.(*lang.ListNode)

		fmt.Printf("%s- %s\n", tab(indent), listNode.Type())

		for _, key := range listNode.Items {
			ShowSyntaxTree(key, indent+1)
		}
	case lang.NodeAttr:
		attrNode := node.(*lang.AttrNode)
		fmt.Printf("%s- %s\n", tab(indent), attrNode.Type())

		ShowSyntaxTree(attrNode.Expr, indent+1)
		ShowSyntaxTree(attrNode.Attr, indent+1)
	case lang.NodeSubscript:
		subNode := node.(*lang.SubscriptNode)
		fmt.Printf("%s- %s\n", tab(indent), subNode.Type())

		ShowSyntaxTree(subNode.Expr, indent+1)
		ShowSyntaxTree(subNode.Item, indent+1)
	case lang.NodeCall:
		callNode := node.(*lang.CallNode)
		fmt.Printf("%s- %s\n", tab(indent), callNode.Type())

		ShowSyntaxTree(callNode.Expr, indent+1)
		for _, arg := range callNode.Arguments {
			ShowSyntaxTree(arg, indent+1)
		}
	case lang.StmtMeta:
		metaStmt := node.(*lang.MetaStmt)
		fmt.Printf("%s> %s\n", tab(indent), metaStmt.Type())

		for key, value := range metaStmt.Metadata {
			fmt.Printf("%s- %s\n", tab(indent+1), key)
			ShowSyntaxTree(value, indent+2)
		}
	case lang.StmtStruct:
		structStmt := node.(*lang.StructStmt)
		fmt.Printf("%s> %s (%s)\n", tab(indent), structStmt.Type(), structStmt.Name)

		fmt.Printf("%s* body (%d)\n", tab(indent+1), len(structStmt.Body))
		for _, field := range structStmt.Body {
			ShowSyntaxTree(field, indent+2)
		}

		fmt.Printf("%s* modifiers (%d)\n", tab(indent+1), len(structStmt.Modifiers))
		for key, value := range structStmt.Modifiers {
			fmt.Printf("%s- %s\n", tab(indent+2), key)
			if value != nil {
				ShowSyntaxTree(value, indent+3)
			}
		}
	case lang.StmtDecl:
		declStmt := node.(*lang.DeclStmt)
		fmt.Printf("%s> %s (%s)\n", tab(indent), declStmt.Type(), declStmt.Name.Value)
		fmt.Printf("%s* value\n", tab(indent+1))
		ShowSyntaxTree(declStmt.Kind, indent+2)

		fmt.Printf("%s* modifiers (%d)\n", tab(indent+1), len(declStmt.Modifiers))
		for key, value := range declStmt.Modifiers {
			fmt.Printf("%s- %s\n", tab(indent+2), key)
			if value != nil {
				ShowSyntaxTree(value, indent+3)
			}
		}
	case lang.StmtExpr:
		exprStmt := node.(*lang.ExprStmt)
		fmt.Printf("%s> %s\n", tab(indent), exprStmt.Type())
		ShowSyntaxTree(exprStmt.Expr, indent+1)
	default:
		fmt.Printf("%s> %#v\n", tab(indent), node)
	}
}
