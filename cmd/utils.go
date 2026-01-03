package main

import (
	"fmt"
	"strings"

	"github.com/aescarias/dessert/lang"
)

// ShowSyntaxTree prints the abstract syntax tree (AST) defined by node starting at the
// specified indent level.
func ShowSyntaxTree(node lang.Node, indent int) {
	tabbed := strings.Repeat(" ", indent)

	switch node.Type() {
	case lang.NodeBinOp:
		binOp := node.(*lang.BinOpNode)
		fmt.Printf("%s- %s (%s)\n", tabbed, binOp.Type(), binOp.Op.Value)

		ShowSyntaxTree(binOp.Left, indent+1)
		ShowSyntaxTree(binOp.Right, indent+1)
	case lang.NodeUnaryOp:
		unaryOp := node.(*lang.UnaryOpNode)
		fmt.Printf("%s- %s (%s)\n", tabbed, unaryOp.Type(), unaryOp.Op.Value)

		ShowSyntaxTree(unaryOp.Node, indent+1)
	case lang.NodeLiteral:
		litNode := node.(*lang.LiteralNode)

		fmt.Printf("%s- %s (%s: %q)\n", tabbed, litNode.Type(), litNode.Token.Kind, litNode.Token.Value)
	case lang.NodeMap:
		mapNode := node.(*lang.MapNode)

		fmt.Printf("%s- %s\n", tabbed, mapNode.Type())

		for key, value := range mapNode.Items {
			ShowSyntaxTree(key, indent+1)
			ShowSyntaxTree(value, indent+2)
		}
	case lang.NodeList:
		listNode := node.(*lang.ListNode)

		fmt.Printf("%s- %s\n", tabbed, listNode.Type())

		for _, key := range listNode.Items {
			ShowSyntaxTree(key, indent+1)
		}
	case lang.NodeAttr:
		attrNode := node.(*lang.AttrNode)
		fmt.Printf("%s- %s\n", tabbed, attrNode.Type())

		ShowSyntaxTree(attrNode.Expr, indent+1)
		ShowSyntaxTree(attrNode.Attr, indent+1)
	case lang.NodeSubscript:
		subNode := node.(*lang.SubscriptNode)
		fmt.Printf("%s- %s\n", tabbed, subNode.Type())

		ShowSyntaxTree(subNode.Expr, indent+1)
		ShowSyntaxTree(subNode.Item, indent+1)
	case lang.NodeCall:
		callNode := node.(*lang.CallNode)
		fmt.Printf("%s- %s\n", tabbed, callNode.Type())

		ShowSyntaxTree(callNode.Expr, indent+1)
		for _, arg := range callNode.Arguments {
			ShowSyntaxTree(arg, indent+1)
		}
	case lang.StmtMeta:
		metaStmt := node.(*lang.MetaStmt)
		fmt.Printf("%s> %s\n", tabbed, metaStmt.Type())

		for key, value := range metaStmt.Metadata {
			ShowSyntaxTree(key, indent+1)
			ShowSyntaxTree(value, indent+2)
		}
	case lang.StmtStruct:
		structStmt := node.(*lang.StructStmt)
		fmt.Printf("%s> %s (%s)\n", tabbed, structStmt.Type(), structStmt.Name)

		fmt.Printf("%s- fields (%d)\n", tabbed, len(structStmt.Fields))
		for _, field := range structStmt.Fields {
			ShowSyntaxTree(field.Name, indent+2)
			ShowSyntaxTree(field.Value, indent+3)
		}

		fmt.Printf("%s- modifiers (%d)\n", tabbed, len(structStmt.Modifiers))
		for key, value := range structStmt.Modifiers {
			ShowSyntaxTree(key, indent+2)
			if value != nil {
				ShowSyntaxTree(value, indent+3)
			}
		}
	case lang.StmtExpr:
		exprStmt := node.(*lang.ExprStmt)
		fmt.Printf("%s> %s\n", tabbed, exprStmt.Type())
		ShowSyntaxTree(exprStmt.Expr, indent+1)
	default:
		fmt.Printf("%s> %#v\n", tabbed, node)
	}
}
