package lang

import (
	"testing"
)

func assertToken(t *testing.T, context string, target Token, isKind TokenKind, isValue string) {
	if target.Kind != isKind || target.Value != isValue {
		t.Fatalf("%s: expected %s %q, got %s %q", context, isKind, isValue, target.Kind, target.Value)
	}
}

func assertNode[T Node](t *testing.T, context string, target Node) T {
	if expected, ok := target.(T); ok {
		return expected
	}
	var empty T
	t.Fatalf("%s: expected %s, got %s", context, empty.Type(), target.Type())
	return empty
}

func parseInput(t *testing.T, input string) []Node {
	lx := NewLexer([]byte(input))
	if err := lx.Process(); err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	ps := NewParser(lx.Tokens)
	statements, err := ps.Parse()
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	return statements
}

func assertLiteral(t *testing.T, context string, node Node, kind TokenKind, value string) *LiteralNode {
	lit := assertNode[*LiteralNode](t, context, node)
	assertToken(t, context, lit.Token, kind, value)
	return lit
}

func TestMetaStatement(t *testing.T) {
	statements := parseInput(t, `meta { dessert: "0.1", name: "Test" };`)

	meta := assertNode[*MetaStmt](t, "meta", statements[0])
	dstNode, ok := meta.Metadata["dessert"]
	if !ok {
		t.Fatalf(`meta: missing key "dessert"`)
	}

	dst := assertNode[*LiteralNode](t, "dessert", dstNode)
	assertToken(t, "dessert", dst.Token, TokenString, "0.1")

	nameNode, ok := meta.Metadata["name"]
	if !ok {
		t.Fatalf(`meta: missing key "name"`)
	}

	name := assertNode[*LiteralNode](t, "name", nameNode)
	assertToken(t, "name", name.Token, TokenString, "Test")
}

func TestStructStmt(t *testing.T) {
	statements := parseInput(t, "struct Point { x: uint16; y: uint16; };")

	structure := assertNode[*StructStmt](t, "struct", statements[0])
	if structure.Name != "Point" {
		t.Fatalf("expected struct name %s, got %s", "Point", structure.Name)
	}

	xField := assertNode[*DeclStmt](t, "x", structure.Body[0])
	assertToken(t, "x", xField.Name, TokenIdentifier, "x")
	assertLiteral(t, "x", xField.Kind, TokenIdentifier, "uint16")

	yField := assertNode[*DeclStmt](t, "y", structure.Body[1])
	assertToken(t, "y", yField.Name, TokenIdentifier, "y")
	assertLiteral(t, "y", yField.Kind, TokenIdentifier, "uint16")
}

func TestBinaryOp(t *testing.T) {
	statements := parseInput(t, "10 * 20 + 30;")

	expr := assertNode[*ExprStmt](t, "10 * 20 + 30", statements[0])

	// (a * b) + y
	root := assertNode[*BinOpNode](t, "10 * 20 + 30", expr.Expr)
	assertToken(t, "10 * 20 + 30", root.Op, TokenPlus, "+")

	x := assertNode[*BinOpNode](t, "10 * 20", root.Left)
	assertToken(t, "10 * 20", x.Op, TokenMul, "*")

	assertLiteral(t, "10", x.Left, TokenInteger, "10")
	assertLiteral(t, "20", x.Right, TokenInteger, "20")
	assertLiteral(t, "30", root.Right, TokenInteger, "30")
}

func TestCall(t *testing.T) {
	statements := parseInput(t, "foo(bar, baz, 20);")

	expr := assertNode[*ExprStmt](t, "foo(bar, baz, 20)", statements[0])

	call := assertNode[*CallNode](t, "foo(bar, baz, 20)", expr.Expr)
	assertLiteral(t, "foo", call.Expr, TokenIdentifier, "foo")

	assertLiteral(t, "bar", call.Arguments[0], TokenIdentifier, "bar")
	assertLiteral(t, "baz", call.Arguments[1], TokenIdentifier, "baz")
	assertLiteral(t, "20", call.Arguments[2], TokenInteger, "20")
}
