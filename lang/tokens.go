package lang

import (
	"fmt"
	"slices"
)

type Token struct {
	Kind     TokenKind
	Value    string
	Position Position
}

type Position struct {
	Start int
	End   int
}

type TokenKind int

const (
	TokenLParen       TokenKind = iota // (
	TokenRParen                        // )
	TokenLBrace                        // {
	TokenRBrace                        // }
	TokenLBracket                      // [
	TokenRBracket                      // ]
	TokenMul                           // *
	TokenPow                           // **
	TokenDiv                           // /
	TokenPlus                          // +
	TokenMinus                         // -
	TokenComma                         // ,
	TokenColon                         // :
	TokenSemicolon                     // ;
	TokenDot                           // .
	TokenEquals                        // ==
	TokenAssign                        // =
	TokenLt                            // <
	TokenGt                            // >
	TokenLtEq                          // <=
	TokenGtEq                          // >=
	TokenBitwiseOr                     // |
	TokenBitwiseAnd                    // &
	TokenBitwiseNot                    // ~
	TokenBitwiseXor                    // ^
	TokenBitwiseLeft                   // <<
	TokenBitwiseRight                  // >>
	TokenRemainder                     // %
	TokenNot                           // !
	TokenNotEq                         // !=
	TokenLogicalAnd                    // &&
	TokenLogicalOr                     // ||
	TokenQuestion                      // ?
	TokenAt                            // @
	TokenIdentifier
	TokenKeyword
	TokenInteger
	TokenFloat
	TokenString
)

func (t TokenKind) String() string {
	switch t {
	case TokenLParen:
		return "LParen"
	case TokenRParen:
		return "RParen"
	case TokenLBrace:
		return "LBrace"
	case TokenRBrace:
		return "RBrace"
	case TokenLBracket:
		return "LBracket"
	case TokenRBracket:
		return "RBracket"
	case TokenMul:
		return "Mul"
	case TokenPow:
		return "Pow"
	case TokenDiv:
		return "Div"
	case TokenPlus:
		return "Plus"
	case TokenMinus:
		return "Minus"
	case TokenComma:
		return "Comma"
	case TokenColon:
		return "Colon"
	case TokenSemicolon:
		return "Semicolon"
	case TokenDot:
		return "Dot"
	case TokenEquals:
		return "Equals"
	case TokenAssign:
		return "Assign"
	case TokenLt:
		return "Lt"
	case TokenGt:
		return "Gt"
	case TokenLtEq:
		return "LtEq"
	case TokenGtEq:
		return "GtEq"
	case TokenBitwiseOr:
		return "BitwiseOr"
	case TokenBitwiseAnd:
		return "BitwiseAnd"
	case TokenBitwiseNot:
		return "BitwiseNot"
	case TokenBitwiseXor:
		return "BitwiseXor"
	case TokenBitwiseLeft:
		return "BitwiseLeft"
	case TokenBitwiseRight:
		return "BitwiseRight"
	case TokenRemainder:
		return "Remainder"
	case TokenNot:
		return "Not"
	case TokenNotEq:
		return "NotEq"
	case TokenLogicalAnd:
		return "LogicalAnd"
	case TokenLogicalOr:
		return "LogicalOr"
	case TokenQuestion:
		return "Question"
	case TokenAt:
		return "At"
	case TokenIdentifier:
		return "Identifier"
	case TokenKeyword:
		return "Keyword"
	case TokenInteger:
		return "Integer"
	case TokenFloat:
		return "Float"
	case TokenString:
		return "String"
	default:
		return fmt.Sprintf("TokenKind(%d)", t)
	}
}

type KeywordKind string

const (
	KeywordTrue   KeywordKind = "true"
	KeywordFalse  KeywordKind = "false"
	KeywordStruct KeywordKind = "struct"
	KeywordEnum   KeywordKind = "enum"
	KeywordImport KeywordKind = "import"
	KeywordMeta   KeywordKind = "meta"
	KeywordLet    KeywordKind = "let"
	KeywordConst  KeywordKind = "const"
	KeywordIf     KeywordKind = "if"
	KeywordElse   KeywordKind = "else"
	KeywordSwitch KeywordKind = "switch"
	KeywordFor    KeywordKind = "for"
	KeywordWhile  KeywordKind = "while"
	KeywordFunc   KeywordKind = "func"
	KeywordReturn KeywordKind = "return"
)

var AvailableKeywords = []KeywordKind{
	KeywordTrue,
	KeywordFalse,
	KeywordStruct,
	KeywordEnum,
	KeywordImport,
	KeywordMeta,
	KeywordLet,
	KeywordConst,
	KeywordIf,
	KeywordElse,
	KeywordSwitch,
	KeywordFor,
	KeywordWhile,
	KeywordFunc,
	KeywordReturn,
}

// IsASCIILetter reports whether a character ch is an ASCII letter, i.e. a character
// in the range a-z or A-Z.
func IsASCIILetter(ch byte) bool {
	return 'a' <= ch && ch <= 'z' || 'A' <= ch && ch <= 'Z'
}

// IsDecimalDigit reports whether a character ch is a decimal digit, i.e. a character
// in the range 0-9.
func IsDecimalDigit(ch byte) bool {
	return '0' <= ch && ch <= '9'
}

// IsHexDigit reports whether a character ch is a hexadecimal digit, i.e. a character
// in the range 0-9 then A-F or a-f.
func IsHexDigit(ch byte) bool {
	return ('0' <= ch && ch <= '9') || ('a' <= ch && ch <= 'f') || ('A' <= ch && ch <= 'F')
}

// IsOctalDigit reports whether a character ch is an octal digit, i.e. a character
// in the range 0-7.
func IsOctalDigit(ch byte) bool {
	return '0' <= ch && ch <= '7'
}

// IsBinaryDigit reports whether a character ch is a binary digit, i.e. 0 or 1.
func IsBinaryDigit(ch byte) bool {
	return ch == '0' || ch == '1'
}

// IsASCIIWhitespace reports whether a character ch is ASCII whitespace, i.e. any
// of the following characters: horizontal tab ('\t'), line feed ('\n'),
// vertical tab ('\v'), form feed ('\f'), carriage return ('\r'), space (' '),
// next line (U+0085; NEL), and non-breaking space (U+00A0; NBSP).
func IsASCIIWhitespace(ch byte) bool {
	return slices.Contains([]byte{'\t', '\n', '\v', '\f', '\r', ' ', '\x85', '\xa0'}, ch)
}

// IsIdentifier reports whether a character ch can be part of a valid identifier.
//
// An identifier is a string of alphanumeric characters including the underscore (_).
// An identifier may not start with a digit and may not contain whitespace within.
func IsIdentifier(ch byte) bool {
	return IsASCIILetter(ch) || IsDecimalDigit(ch) || ch == '_'
}

// IsStartOfIdentifier reports whether a character ch can be the start of an identifier.
// An identifier may not start with a digit.
func IsStartOfIdentifier(ch byte) bool {
	return (IsASCIILetter(ch) || ch == '_') && !IsDecimalDigit(ch)
}
