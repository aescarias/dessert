package lang

import (
	"bytes"
	"fmt"
	"slices"
	"strconv"
)

var escapeMap = map[byte]byte{
	'\\': '\\',
	'\'': '\'',
	'"':  '"',
	'n':  '\n',
	'r':  '\r',
	't':  '\t',
}

var singleTokenMap = map[byte]TokenKind{
	'(': TokenLParen,
	')': TokenRParen,
	'{': TokenLBrace,
	'}': TokenRBrace,
	'[': TokenLBracket,
	']': TokenRBracket,
	',': TokenComma,
	':': TokenColon,
	';': TokenSemicolon,
	'?': TokenQuestion,
	'.': TokenDot,
	'+': TokenPlus,
	'-': TokenMinus,
	'%': TokenRemainder,
	'^': TokenBitwiseXor,
	'~': TokenBitwiseNot,
	'@': TokenAt,
}

type Lexer struct {
	Scanner[byte]
	Tokens []Token
}

func NewLexer(Contents []byte) Lexer {
	return Lexer{Scanner: NewScanner(Contents), Tokens: []Token{}}
}

func (lx *Lexer) LexNumeric() Token {
	start := lx.Position
	digits := []byte{lx.Cursor()}
	lx.Advance(1)

	type match func(ch byte) bool
	var isDigit match

	if digits[0] == '0' && !lx.IsDone() {
		switch lx.Cursor() {
		case 'b': // binary
			isDigit = IsBinaryDigit
			digits = append(digits, lx.Cursor())
			lx.Advance(1)
		case 'o': // octal
			isDigit = IsOctalDigit
			digits = append(digits, lx.Cursor())
			lx.Advance(1)
		case 'x': // hex
			isDigit = IsHexDigit
			digits = append(digits, lx.Cursor())
			lx.Advance(1)
		}
	}

	if isDigit == nil {
		isDigit = func(ch byte) bool { return IsDecimalDigit(ch) || lx.Cursor() == '.' }
	}

	for !lx.IsDone() && (isDigit(lx.Cursor()) || lx.Cursor() == '_') {
		if lx.Cursor() == '_' {
			lx.Advance(1)
			continue
		}

		digits = append(digits, lx.Cursor())
		lx.Advance(1)
	}

	pos := Position{start, lx.Position}
	if bytes.Contains(digits, []byte{'.'}) {
		return Token{Kind: TokenFloat, Value: string(digits), Position: pos}
	} else {
		return Token{Kind: TokenInteger, Value: string(digits), Position: pos}
	}
}

func (lx *Lexer) LexIdentifier() Token {
	start := lx.Position
	ident := []byte{lx.Cursor()}
	lx.Advance(1)

	for !lx.IsDone() && IsIdentifier(lx.Cursor()) {
		ident = append(ident, lx.Cursor())
		lx.Advance(1)
	}

	pos := Position{start, lx.Position}
	if slices.Contains(AvailableKeywords, KeywordKind(ident)) {
		return Token{Kind: TokenKeyword, Value: string(ident), Position: pos}
	} else {
		return Token{Kind: TokenIdentifier, Value: string(ident), Position: pos}
	}
}

func (lx *Lexer) LexString(delimiter byte) (Token, error) {
	start := lx.Position

	strSeq := []byte{}
	lx.Advance(1) // for the single-byte start quote

	closed := false
	for !lx.IsDone() {
		cur := lx.Cursor()

		if cur == '\\' {
			escapeStart := lx.Position
			lx.Advance(1)

			nc := lx.Cursor()
			if escape, ok := escapeMap[nc]; ok {
				strSeq = append(strSeq, escape)
				lx.Advance(1)
				continue
			}

			if nc == 'x' {
				const byteSize int = 2
				hexSeq := string(lx.Peek(byteSize))
				hexVal, err := strconv.ParseInt(hexSeq, 16, 16)
				if err != nil {
					return Token{}, LangError{
						ErrorSyntax,
						Position{escapeStart, lx.Position + byteSize + 1},
						fmt.Sprintf("invalid hex sequence %s", hexSeq),
					}
				}

				lx.Advance(byteSize + 1)
				strSeq = append(strSeq, byte(hexVal))
				continue
			} else if IsDecimalDigit(nc) {
				const octSize int = 3

				octSeq := string(lx.Cursor()) + string(lx.Peek(octSize-1))
				octVal, err := strconv.ParseInt(octSeq, 8, 8)
				if err != nil {
					return Token{}, LangError{
						ErrorSyntax,
						Position{escapeStart, lx.Position + octSize},
						fmt.Sprintf("invalid octal sequence %s", octSeq),
					}
				}

				lx.Advance(octSize)
				strSeq = append(strSeq, byte(octVal))
				continue
			}

			return Token{}, LangError{
				ErrorSyntax,
				Position{escapeStart, lx.Position},
				fmt.Sprintf("unknown escape sequence %q", lx.Cursor()),
			}
		}

		if cur == delimiter {
			lx.Advance(1)
			closed = true
			break
		}

		strSeq = append(strSeq, cur)
		lx.Advance(1)
	}

	if !closed {
		return Token{}, LangError{
			ErrorSyntax,
			Position{start, lx.Position},
			"string was never closed",
		}
	}

	return Token{
		Kind:     TokenString,
		Value:    string(strSeq),
		Position: Position{start, lx.Position},
	}, nil
}

func (lx *Lexer) addToken(kind TokenKind, value string) {
	pos := Position{Start: lx.Position, End: lx.Position + len(value)}
	lx.Tokens = append(lx.Tokens, Token{Kind: kind, Value: value, Position: pos})
}

func (lx *Lexer) Process() error {
	for !lx.IsDone() {
		ch := lx.Cursor()

		// single-byte tokens
		if kind, ok := singleTokenMap[ch]; ok {
			lx.addToken(kind, string(ch))
			lx.Advance(1)
			continue
		}

		// (possibly) two-byte tokens
		switch ch {
		case '=':
			switch nc := string(lx.Peek(1)); nc {
			case "=":
				lx.addToken(TokenEquals, string(ch)+nc)
				lx.Advance(2)
				continue
			default:
				lx.addToken(TokenAssign, string(ch))
				lx.Advance(1)
				continue
			}
		case '>':
			switch nc := string(lx.Peek(1)); nc {
			case "=":
				lx.addToken(TokenGtEq, string(ch)+nc)
				lx.Advance(1)
			case ">":
				lx.addToken(TokenBitwiseRight, string(ch)+nc)
				lx.Advance(1)
			default:
				lx.addToken(TokenGt, string(ch))
			}
			lx.Advance(1)
			continue

		case '<':
			switch nc := string(lx.Peek(1)); nc {
			case "=":
				lx.addToken(TokenLtEq, string(ch)+nc)
				lx.Advance(1)
			case "<":
				lx.addToken(TokenBitwiseLeft, string(ch)+nc)
				lx.Advance(1)
			default:
				lx.addToken(TokenLt, string(ch))
			}
			lx.Advance(1)
			continue

		case '/':
			switch nc := string(lx.Peek(1)); nc {
			case "/":
				for !lx.IsDone() && lx.Cursor() != '\n' {
					lx.Advance(1)
				}
				continue
			case "*":
				for !lx.IsDone() {
					if lx.Position+1 >= len(lx.Data) {
						return LangError{
							Kind:     ErrorSyntax,
							Position: Position{Start: lx.Position, End: lx.Position + 1},
							Message:  "unterminated block comment",
						}
					}

					maybeEnd := []byte{lx.Cursor(), lx.Peek(1)[0]}
					if slices.Equal(maybeEnd, []byte("*/")) {
						lx.Advance(2)
						break
					}

					lx.Advance(1)
				}
				continue
			default:
				lx.addToken(TokenDiv, string(ch))
				lx.Advance(1)
				continue
			}
		case '*':
			switch nc := string(lx.Peek(1)); nc {
			case "*":
				lx.addToken(TokenPow, string(ch)+nc)
				lx.Advance(1)
			default:
				lx.addToken(TokenMul, string(ch))
			}
			lx.Advance(1)
			continue
		case '!':
			switch nc := string(lx.Peek(1)); nc {
			case "=":
				lx.addToken(TokenNotEq, string(ch)+nc)
				lx.Advance(1)
			default:
				lx.addToken(TokenNot, string(ch))
			}
			lx.Advance(1)
			continue
		case '|':
			switch nc := string(lx.Peek(1)); nc {
			case "|":
				lx.addToken(TokenLogicalOr, string(ch)+nc)
				lx.Advance(1)
			default:
				lx.addToken(TokenBitwiseOr, string(ch))
			}
			lx.Advance(1)
			continue
		case '&':
			switch nc := string(lx.Peek(1)); nc {
			case "&":
				lx.addToken(TokenLogicalAnd, string(ch)+nc)
				lx.Advance(1)
			default:
				lx.addToken(TokenBitwiseAnd, string(ch))
			}
			lx.Advance(1)
			continue
		case '\'', '"':
			strTok, err := lx.LexString(ch)
			if err != nil {
				return err
			}

			lx.Tokens = append(lx.Tokens, strTok)
			continue
		}

		if IsStartOfIdentifier(ch) {
			lx.Tokens = append(lx.Tokens, lx.LexIdentifier())
			continue
		}

		if IsDecimalDigit(ch) {
			lx.Tokens = append(lx.Tokens, lx.LexNumeric())
			continue
		}

		if IsASCIIWhitespace(ch) {
			lx.Advance(1)
			continue
		}

		return LangError{
			Kind:     ErrorSyntax,
			Position: Position{Start: lx.Position, End: lx.Position + 1},
			Message:  fmt.Sprintf("unknown character %q", lx.Cursor()),
		}
	}

	return nil
}
