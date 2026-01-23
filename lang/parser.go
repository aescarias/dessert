package lang

import (
	"errors"
	"fmt"
	"slices"
)

type Parser struct {
	Scanner[Token]
}

func NewParser(tokens []Token) Parser {
	return Parser{Scanner: NewScanner(tokens)}
}

func (ps *Parser) matchesToken(kind TokenKind) bool {
	return !ps.IsDone() && ps.Cursor().Kind == kind
}

func (ps *Parser) tryPostfix(left Node) (Node, error) {
	for !ps.IsDone() {
		switch ps.Cursor().Kind {
		case TokenLBracket:
			ps.Advance(1)
			if ps.matchesToken(TokenRBracket) {
				return nil, LangError{ErrorSyntax, ps.Cursor().Position, "subscript expects at least one parameter"}
			}

			item, err := ps.ParseExpr()
			if err != nil {
				return nil, err
			}
			if !ps.matchesToken(TokenRBracket) {
				return nil, LangError{ErrorSyntax, item.Position(), "expected end of subscript access"}
			}
			ps.Advance(1)
			left = &SubscriptNode{Expr: left, Item: item}
		case TokenLParen:
			start := left.Position().Start
			ps.Advance(1)
			arguments := []Node{}
			for !ps.IsDone() && ps.Cursor().Kind != TokenRParen {
				arg, err := ps.ParseExpr()
				if err != nil {
					return nil, err
				}
				arguments = append(arguments, arg)
				if ps.matchesToken(TokenComma) {
					ps.Advance(1)
				} else if !ps.matchesToken(TokenRParen) {
					return nil, LangError{ErrorSyntax, arg.Position(), "expected end or continuation of function call"}
				}
			}
			end := ps.Cursor().Position.End
			ps.Advance(1)
			left = &CallNode{Expr: left, Arguments: arguments, pos: Position{start, end}}
		case TokenDot:
			ps.Advance(1)
			if !ps.matchesToken(TokenIdentifier) {
				return nil, LangError{ErrorSyntax, left.Position(), "expected attribute after dot"}
			}
			attr := &LiteralNode{Token: ps.Cursor()}
			ps.Advance(1)
			left = &AttrNode{Expr: left, Attr: attr}
		default:
			return left, nil
		}
	}

	return left, nil
}

var ErrNotModifiers = fmt.Errorf("not the start of a statement modifier list")

func (ps *Parser) ParseModifierList() (map[string]Node, error) {
	if !ps.matchesToken(TokenAt) {
		return nil, ErrNotModifiers
	}

	start := ps.Cursor().Position.Start
	ps.Advance(1)

	if !ps.matchesToken(TokenLParen) {
		return nil, LangError{ErrorSyntax, Position{Start: start, End: start + 1}, "expected start of modifier list"}
	}
	ps.Advance(1)

	modifiers := map[string]Node{}
	for !ps.IsDone() && ps.Cursor().Kind != TokenRParen {
		var key Token

		if ps.Cursor().Kind == TokenIdentifier {
			key = ps.Cursor()
			ps.Advance(1)
		} else {
			return nil, LangError{ErrorSyntax, ps.Cursor().Position, "modifier name must be an identifier"}
		}

		var value Node
		if ps.matchesToken(TokenColon) {
			ps.Advance(1)

			var err error
			value, err = ps.ParseExpr()
			if err != nil {
				return nil, err
			}
		} else {
			value = nil
		}

		if ps.matchesToken(TokenComma) {
			ps.Advance(1)
		} else if !ps.matchesToken(TokenRParen) {
			pos := Position{Start: value.Position().End, End: value.Position().End + 1}
			return nil, LangError{ErrorSyntax, pos, "expected end or continuation of modifier list"}
		}

		modifiers[key.Value] = value
	}

	if ps.IsDone() {
		return nil, LangError{ErrorSyntax, Position{start, start + 1}, "expected closing paren"}
	}

	ps.Advance(1)

	return modifiers, nil
}

func (ps *Parser) ParseMap() (*MapNode, error) {
	start := ps.Cursor().Position.Start
	ps.Advance(1)

	items := map[Node]Node{}
	for !ps.IsDone() && ps.Cursor().Kind != TokenRBrace {
		key, err := ps.ParseExpr()
		if err != nil {
			return nil, err
		}

		if !ps.matchesToken(TokenColon) {
			pos := Position{Start: key.Position().End, End: key.Position().End + 1}
			return nil, LangError{ErrorSyntax, pos, "expected colon after key in mapping"}
		}
		ps.Advance(1)

		value, err := ps.ParseExpr()
		if err != nil {
			return nil, err
		}

		if ps.matchesToken(TokenComma) {
			ps.Advance(1)
		} else if !ps.matchesToken(TokenRBrace) {
			pos := Position{Start: value.Position().End, End: value.Position().End + 1}
			return nil, LangError{ErrorSyntax, pos, "expected end or continuation of mapping"}
		}

		items[key] = value
	}

	if ps.IsDone() {
		return nil, LangError{ErrorSyntax, Position{start, start + 1}, "expected closing brace"}
	}

	end := ps.Cursor().Position.End
	ps.Advance(1)

	return &MapNode{Items: items, pos: Position{start, end}}, nil
}

func (ps *Parser) ParseMetaStmt(start int) (*MetaStmt, error) {
	if ps.IsDone() || ps.Cursor().Kind != TokenLBrace {
		return nil, LangError{ErrorSyntax, Position{start, start + 1}, "expected start of metadata mapping"}
	}
	ps.Advance(1)

	items := map[string]Node{}
	for !ps.IsDone() && ps.Cursor().Kind != TokenRBrace {
		var key Token

		if ps.Cursor().Kind == TokenIdentifier {
			key = ps.Cursor()
			ps.Advance(1)
		} else {
			return nil, LangError{ErrorSyntax, ps.Cursor().Position, "key must be an identifier"}
		}

		if !ps.matchesToken(TokenColon) {
			return nil, LangError{ErrorSyntax, key.Position, "expected colon after key in metadata mapping"}
		}
		ps.Advance(1)

		value, err := ps.ParseExpr()
		if err != nil {
			return nil, err
		}

		if ps.matchesToken(TokenComma) {
			ps.Advance(1)
		} else if !ps.matchesToken(TokenRBrace) {
			pos := Position{Start: value.Position().End, End: value.Position().End + 1}
			return nil, LangError{ErrorSyntax, pos, "expected end or continuation of metadata mapping"}
		}

		items[key.Value] = value
	}

	if ps.IsDone() {
		return nil, LangError{ErrorSyntax, Position{start, start + 1}, "expected closing brace"}
	}

	end := ps.Cursor().Position.End
	ps.Advance(1)

	if !ps.matchesToken(TokenSemicolon) {
		return nil, LangError{
			ErrorSyntax,
			Position{end - 1, end},
			"expected semicolon",
		}
	}
	ps.Advance(1)

	return &MetaStmt{Metadata: items, pos: Position{start, end}}, nil
}

func (ps *Parser) ParseList() (*ListNode, error) {
	start := ps.Cursor().Position.Start
	ps.Advance(1)

	items := []Node{}
	for !ps.IsDone() && ps.Cursor().Kind != TokenRBracket {
		item, err := ps.ParseExpr()
		if err != nil {
			return nil, err
		}

		if ps.matchesToken(TokenComma) {
			ps.Advance(1)
		} else if !ps.matchesToken(TokenRBracket) {
			pos := Position{Start: item.Position().End, End: item.Position().End + 1}
			return nil, LangError{ErrorSyntax, pos, "expected end or continuation of list"}
		}

		items = append(items, item)
	}

	if ps.IsDone() {
		return nil, LangError{ErrorSyntax, Position{start, start + 1}, "expected closing bracket"}
	}

	end := ps.Cursor().Position.End
	ps.Advance(1)

	return &ListNode{Items: items, pos: Position{start, end}}, nil
}

func (ps *Parser) ParseLiteral() (Node, error) {
	var left Node

	if ps.IsDone() {
		return nil, LangError{
			ErrorSyntax,
			ps.Data[len(ps.Data)-1].Position,
			"expected literal, reached end of data",
		}
	}

	switch ps.Cursor().Kind {
	case TokenInteger, TokenFloat, TokenIdentifier, TokenString, TokenKeyword:
		left = &LiteralNode{Token: ps.Cursor()}
		ps.Advance(1)
	case TokenPlus, TokenMinus, TokenBitwiseNot, TokenNot:
		op := ps.Cursor()
		ps.Advance(1)
		expr, err := ps.ParseLiteral()
		if err != nil {
			return nil, err
		}
		left = &UnaryOpNode{Op: op, Node: expr}
	case TokenLParen:
		ps.Advance(1)
		expr, err := ps.ParseExpr()
		if err != nil {
			return nil, err
		}

		if !ps.matchesToken(TokenRParen) {
			pos := Position{Start: expr.Position().End, End: expr.Position().End + 1}
			return nil, LangError{ErrorSyntax, pos, "unclosed parentheses"}
		}
		ps.Advance(1)

		left = expr
	case TokenLBrace:
		if mapping, err := ps.ParseMap(); err != nil {
			return nil, err
		} else {
			left = mapping
		}
	case TokenLBracket:
		if list, err := ps.ParseList(); err != nil {
			return nil, err
		} else {
			left = list
		}
	}

	if left == nil {
		return nil, LangError{
			ErrorRuntime,
			ps.Cursor().Position,
			fmt.Sprintf("unexpected token %s", ps.Cursor().Kind),
		}
	}

	postfix, err := ps.tryPostfix(left)
	if err != nil {
		return nil, err
	}

	return postfix, nil
}

func (ps *Parser) ParsePower() (Node, error) {
	var (
		left Node
		err  error
	)

	if left, err = ps.ParseLiteral(); err != nil {
		return nil, err
	}

	for !ps.IsDone() && ps.Cursor().Kind == TokenPow {
		tok := ps.Cursor()
		ps.Advance(1)

		right, err := ps.ParseLiteral()
		if err != nil {
			return nil, err
		}

		left = &BinOpNode{Left: left, Op: tok, Right: right}
	}

	return left, nil
}

func (ps *Parser) ParseFactor() (Node, error) {
	var (
		left Node
		err  error
	)

	if left, err = ps.ParsePower(); err != nil {
		return nil, err
	}

	ops := []TokenKind{
		TokenMul, TokenDiv, TokenRemainder,
		TokenBitwiseLeft, TokenBitwiseRight, TokenBitwiseAnd,
	}

	for !ps.IsDone() && slices.Contains(ops, ps.Cursor().Kind) {
		tok := ps.Cursor()
		ps.Advance(1)

		right, err := ps.ParsePower()
		if err != nil {
			return nil, err
		}

		left = &BinOpNode{Left: left, Op: tok, Right: right}
	}

	return left, nil

}

func (ps *Parser) ParseTerm() (Node, error) {
	var (
		left Node
		err  error
	)

	if left, err = ps.ParseFactor(); err != nil {
		return nil, err
	}

	ops := []TokenKind{TokenPlus, TokenMinus, TokenBitwiseOr, TokenBitwiseXor}

	for !ps.IsDone() && slices.Contains(ops, ps.Cursor().Kind) {
		tok := ps.Cursor()
		ps.Advance(1)

		right, err := ps.ParseFactor()
		if err != nil {
			return nil, err
		}

		left = &BinOpNode{Left: left, Op: tok, Right: right}
	}

	return left, nil
}

func (ps *Parser) ParseComparison() (Node, error) {
	var (
		left Node
		err  error
	)

	if left, err = ps.ParseTerm(); err != nil {
		return nil, err
	}

	ops := []TokenKind{TokenEquals, TokenNotEq, TokenLt, TokenLtEq, TokenGt, TokenGtEq}

	for !ps.IsDone() && slices.Contains(ops, ps.Cursor().Kind) {
		tok := ps.Cursor()
		ps.Advance(1)

		right, err := ps.ParseTerm()
		if err != nil {
			return nil, err
		}

		left = &BinOpNode{Left: left, Op: tok, Right: right}
	}

	return left, nil
}

func (ps *Parser) ParseLogicalAnd() (Node, error) {
	var (
		left Node
		err  error
	)

	if left, err = ps.ParseComparison(); err != nil {
		return nil, err
	}

	for !ps.IsDone() && ps.Cursor().Kind == TokenLogicalAnd {
		tok := ps.Cursor()
		ps.Advance(1)

		right, err := ps.ParseComparison()
		if err != nil {
			return nil, err
		}

		left = &BinOpNode{Left: left, Op: tok, Right: right}
	}

	return left, nil
}

func (ps *Parser) ParseLogicalOr() (Node, error) {
	var (
		left Node
		err  error
	)

	if left, err = ps.ParseLogicalAnd(); err != nil {
		return nil, err
	}

	for !ps.IsDone() && ps.Cursor().Kind == TokenLogicalOr {
		tok := ps.Cursor()
		ps.Advance(1)

		right, err := ps.ParseLogicalAnd()
		if err != nil {
			return nil, err
		}

		left = &BinOpNode{Left: left, Op: tok, Right: right}
	}

	return left, nil
}

func (ps *Parser) ParseStructStmt(start int, modifiers map[string]Node) (Node, error) {
	if ps.IsDone() || ps.Cursor().Kind != TokenIdentifier {
		return nil, LangError{
			ErrorSyntax,
			Position{start, start + 1},
			"expected struct identifier",
		}
	}

	structName := ps.Cursor()
	ps.Advance(1)

	if !ps.matchesToken(TokenLBrace) {
		return nil, LangError{
			ErrorSyntax,
			structName.Position,
			"expected start of struct block",
		}
	}

	ps.Advance(1)

	block := []Node{}
	for !ps.IsDone() && ps.Cursor().Kind != TokenRBrace {
		stmt, err := ps.ParseStmt()
		if err != nil {
			return nil, err
		}
		block = append(block, stmt)
	}

	if ps.IsDone() {
		return nil, LangError{
			ErrorSyntax,
			ps.Data[len(ps.Data)-1].Position,
			"unclosed struct block",
		}
	}

	end := ps.Cursor().Position.End
	ps.Advance(1)

	if !ps.matchesToken(TokenSemicolon) {
		return nil, LangError{
			ErrorSyntax,
			Position{end - 1, end},
			"expected semicolon",
		}
	}
	ps.Advance(1)

	return &StructStmt{Name: structName.Value, Modifiers: modifiers, Body: block, pos: Position{start, end}}, nil
}

func (ps *Parser) ParseExpr() (Node, error) {
	return ps.ParseLogicalOr()
}

func (ps *Parser) ParseExprStmt() (Node, error) {
	expr, err := ps.ParseExpr()
	if err != nil {
		return nil, err
	}

	if !ps.matchesToken(TokenSemicolon) {
		return nil, LangError{
			ErrorSyntax,
			expr.Position(),
			"expected semicolon",
		}
	}
	ps.Advance(1)

	return &ExprStmt{Expr: expr}, nil
}

var ErrNotDecl = fmt.Errorf("not the start of a declaration statement")

func (ps *Parser) tryParseDeclaration(start int, modifiers map[string]Node) (Node, error) {
	if ps.IsDone() || ps.Cursor().Kind != TokenIdentifier {
		return nil, ErrNotDecl
	}

	startTok := ps.Position
	name := ps.Cursor()
	ps.Advance(1)

	if !ps.matchesToken(TokenColon) {
		ps.Position = startTok
		return nil, ErrNotDecl
	}
	ps.Advance(1)

	kind, err := ps.ParseExpr()
	if err != nil {
		return nil, err
	}

	end := ps.Cursor().Position.End

	if !ps.matchesToken(TokenSemicolon) {
		return nil, LangError{
			ErrorSyntax,
			kind.Position(),
			"expected semicolon",
		}
	}
	ps.Advance(1)

	return &DeclStmt{Name: name, Kind: kind, Modifiers: modifiers, pos: Position{start, end}}, nil
}

func (ps *Parser) ParseStmt() (Node, error) {
	start := ps.Cursor().Position.Start

	modifiers, err := ps.ParseModifierList()
	if errors.Is(err, ErrNotModifiers) {
		modifiers = nil
	} else if err != nil {
		return nil, err
	}

	switch ps.Cursor().Kind {
	case TokenKeyword:
		switch kw := KeywordKind(ps.Cursor().Value); kw {
		case KeywordMeta:
			ps.Advance(1)
			return ps.ParseMetaStmt(start)
		case KeywordStruct:
			ps.Advance(1)
			return ps.ParseStructStmt(start, modifiers)
		default:
			return nil, LangError{
				ErrorSyntax,
				ps.Cursor().Position,
				fmt.Sprintf("keyword %q was not expected at this point", ps.Cursor().Value),
			}
		}
	case TokenIdentifier:
		decl, err := ps.tryParseDeclaration(start, modifiers)
		if errors.Is(err, ErrNotDecl) {
			return ps.ParseExprStmt()
		} else if err != nil {
			return nil, err
		}
		return decl, nil
	default:
		return ps.ParseExprStmt()
	}
}

func (ps *Parser) Parse() ([]Node, error) {
	statements := []Node{}

	for !ps.IsDone() {
		stmt, err := ps.ParseStmt()
		if err != nil {
			return nil, err
		}

		statements = append(statements, stmt)
	}

	return statements, nil
}
