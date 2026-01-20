package lang

import (
	"testing"
)

func TestLexerOperator(t *testing.T) {
	input := "* ** / + - == = < > <= >= | & ~ ^ >> << % ! != && || ( ) { } [ ] , : ; . ? @"
	expected := []TokenKind{
		TokenMul, TokenPow, TokenDiv, TokenPlus, TokenMinus, TokenEquals,
		TokenAssign, TokenLt, TokenGt, TokenLtEq, TokenGtEq, TokenBitwiseOr,
		TokenBitwiseAnd, TokenBitwiseNot, TokenBitwiseXor, TokenBitwiseRight,
		TokenBitwiseLeft, TokenRemainder, TokenNot, TokenNotEq, TokenLogicalAnd,
		TokenLogicalOr, TokenLParen, TokenRParen, TokenLBrace, TokenRBrace,
		TokenLBracket, TokenRBracket, TokenComma, TokenColon, TokenSemicolon,
		TokenDot, TokenQuestion, TokenAt,
	}

	lx := NewLexer([]byte(input))
	if err := lx.Process(); err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	if len(lx.Tokens) != len(expected) {
		t.Errorf("expected %d tokens, got %d", len(expected), len(lx.Tokens))
	}

	for idx, got := range lx.Tokens {
		if got.Kind != expected[idx] {
			t.Fatalf("token %d: expected %v, got %v", idx, expected, got)
		}
	}

}

func TestLexerNumeric(t *testing.T) {
	input := "1000 2_000 0xFF 0b100 0o777 1.00 2_300.4"
	expected := []Token{
		{Kind: TokenInteger, Value: "1000"},
		{Kind: TokenInteger, Value: "2000"},
		{Kind: TokenInteger, Value: "0xFF"},
		{Kind: TokenInteger, Value: "0b100"},
		{Kind: TokenInteger, Value: "0o777"},
		{Kind: TokenFloat, Value: "1.00"},
		{Kind: TokenFloat, Value: "2300.4"},
	}

	lx := NewLexer([]byte(input))
	if err := lx.Process(); err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	if len(lx.Tokens) != len(expected) {
		t.Errorf("expected %d tokens, got %d", len(expected), len(lx.Tokens))
	}

	for idx, got := range lx.Tokens {
		if got.Kind != expected[idx].Kind || got.Value != expected[idx].Value {
			t.Fatalf("token %d: expected %v, got %v", idx, expected, got)
		}
	}
}

func TestLexerIdentifier(t *testing.T) {
	input := "foo bar foo_bar _foobar FooBar1 while switch true"
	expected := []Token{
		{Kind: TokenIdentifier, Value: "foo"},
		{Kind: TokenIdentifier, Value: "bar"},
		{Kind: TokenIdentifier, Value: "foo_bar"},
		{Kind: TokenIdentifier, Value: "_foobar"},
		{Kind: TokenIdentifier, Value: "FooBar1"},
		{Kind: TokenKeyword, Value: "while"},
		{Kind: TokenKeyword, Value: "switch"},
		{Kind: TokenKeyword, Value: "true"},
	}

	lx := NewLexer([]byte(input))
	if err := lx.Process(); err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	if len(lx.Tokens) != len(expected) {
		t.Errorf("expected %d tokens, got %d", len(expected), len(lx.Tokens))
	}

	for idx, got := range lx.Tokens {
		if got.Kind != expected[idx].Kind || got.Value != expected[idx].Value {
			t.Fatalf("token %d: expected %v, got %v", idx, expected, got)
		}
	}
}

func TestLexerString(t *testing.T) {
	input := `'' 'foobar' "\xff!" "\050\051" "\"Hello, world!\"\r\n"`
	expected := []string{"", "foobar", "\xff!", "\050\051", "\"Hello, world!\"\r\n"}

	lx := NewLexer([]byte(input))
	if err := lx.Process(); err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	if len(lx.Tokens) != len(expected) {
		t.Errorf("expected %d tokens, got %d", len(expected), len(lx.Tokens))
	}

	for idx, got := range lx.Tokens {
		if got.Kind != TokenString || got.Value != expected[idx] {
			t.Fatalf("token %d: expected %v, got %v", idx, expected, got)
		}
	}
}

func TestLexerComment(t *testing.T) {
	input := "// abc\n/* def */"

	lx := NewLexer([]byte(input))
	if err := lx.Process(); err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	if len(lx.Tokens) != 0 {
		t.Errorf("expected no tokens, got %d", len(lx.Tokens))
	}
}

func TestLexerUnterminated(t *testing.T) {
	t.Run("unterminated string", func(t *testing.T) {
		lx := NewLexer([]byte("'"))
		if err := lx.Process(); err == nil {
			t.Fatalf("expected error for unterminated string")
		} else if _, ok := err.(LangError); !ok {
			t.Fatalf("expected LangError, got %T", err)
		}
	})

	t.Run("unterminated block comment", func(t *testing.T) {
		lx := NewLexer([]byte("/*"))
		if err := lx.Process(); err == nil {
			t.Fatalf("expected error for unterminated block comment")
		} else if _, ok := err.(LangError); !ok {
			t.Fatalf("expected LangError, got %T", err)
		}
	})
}

func TestLexerStatement(t *testing.T) {
	input := "struct Point { x: uint8; y: uint8 }"
	expected := []Token{
		{Kind: TokenKeyword, Value: "struct"},
		{Kind: TokenIdentifier, Value: "Point"},
		{Kind: TokenLBrace, Value: "{"},
		{Kind: TokenIdentifier, Value: "x"},
		{Kind: TokenColon, Value: ":"},
		{Kind: TokenIdentifier, Value: "uint8"},
		{Kind: TokenSemicolon, Value: ";"},
		{Kind: TokenIdentifier, Value: "y"},
		{Kind: TokenColon, Value: ":"},
		{Kind: TokenIdentifier, Value: "uint8"},
		{Kind: TokenRBrace, Value: "}"},
	}

	lx := NewLexer([]byte(input))
	if err := lx.Process(); err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	if len(lx.Tokens) != len(expected) {
		t.Errorf("expected %d tokens, got %d", len(expected), len(lx.Tokens))
	}

	for idx, got := range lx.Tokens {
		if got.Kind != expected[idx].Kind || got.Value != expected[idx].Value {
			t.Fatalf("token %d: expected %v, got %v", idx, expected, got)
		}
	}
}
