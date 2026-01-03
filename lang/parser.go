package lang

import (
	"errors"
	"fmt"
	"slices"
)

// A Node has a Type and a Position. It is the generic Node interface for all nodes
// returned by the parser.
type Node interface {
	Type() NodeKind
	Position() Position
}

type NodeKind string

const (
	/* expressions */
	NodeLiteral   NodeKind = "Literal"
	NodeBinOp     NodeKind = "BinOp"
	NodeUnaryOp   NodeKind = "UnaryOp"
	NodeMap       NodeKind = "Map"
	NodeList      NodeKind = "List"
	NodeAttr      NodeKind = "Attr"
	NodeSubscript NodeKind = "Subscript"
	NodeCall      NodeKind = "Call"

	/* statements */
	StmtExpr   NodeKind = "Expr"
	StmtMeta   NodeKind = "Meta"
	StmtStruct NodeKind = "Struct"
)

// A UnaryOpNode represents a unary operation such as -x.
type UnaryOpNode struct {
	Op   Token
	Node Node
}

// A BinOpNode represents a binary operation such as x + y.
type BinOpNode struct {
	Left  Node
	Op    Token
	Right Node
}

// A LiteralNode represents literals such as strings, numbers, or identifiers.
type LiteralNode struct {
	Token Token
}

// A MapNode represents a mapping of key-value pairs.
type MapNode struct {
	Items map[Node]Node
	pos   Position
}

// A ListNode represents an ordered collection of items.
type ListNode struct {
	Items []Node
	pos   Position
}

// An AttrNode represents an attribute access operation such as x.y.
type AttrNode struct {
	Expr Node
	Attr Node
}

// A SubscriptNode represents a subscript access or index into a collection such as x[y].
type SubscriptNode struct {
	Expr Node
	Item Node
}

// A CallNode represents a function call such as foo(x, y, z).
type CallNode struct {
	Expr      Node
	Arguments []Node
	pos       Position
}

func (ln *LiteralNode) Type() NodeKind { return NodeLiteral }
func (ln *LiteralNode) Position() Position {
	return ln.Token.Position
}

func (bn *BinOpNode) Type() NodeKind { return NodeBinOp }
func (bn *BinOpNode) Position() Position {
	return Position{Start: bn.Left.Position().Start, End: bn.Right.Position().End}
}

func (un *UnaryOpNode) Type() NodeKind { return NodeUnaryOp }
func (un *UnaryOpNode) Position() Position {
	return Position{Start: un.Op.Position.Start, End: un.Node.Position().End}
}

func (mn *MapNode) Type() NodeKind     { return NodeMap }
func (mn *MapNode) Position() Position { return mn.pos }

func (ln *ListNode) Type() NodeKind     { return NodeList }
func (ln *ListNode) Position() Position { return ln.pos }

func (an *AttrNode) Type() NodeKind { return NodeAttr }
func (an *AttrNode) Position() Position {
	return Position{
		Start: an.Expr.Position().Start,
		End:   an.Attr.Position().End,
	}
}

func (in *SubscriptNode) Type() NodeKind { return NodeSubscript }
func (in *SubscriptNode) Position() Position {
	return Position{
		Start: in.Expr.Position().Start,
		End:   in.Item.Position().End,
	}
}

func (cn *CallNode) Type() NodeKind     { return NodeCall }
func (cn *CallNode) Position() Position { return cn.pos }

// An ExprStmt represents an expression statement.
type ExprStmt struct {
	Expr Node
}

// A MetaStmt represents a "meta" statement which specifies metadata for the protocol
// being described.
type MetaStmt struct {
	Metadata map[Node]Node
	pos      Position
}

// A StructStmt represents a "struct" statement which declares a structure.
type StructStmt struct {
	Name      Node
	Modifiers map[Node]Node
	Fields    []StructStmtMember
	pos       Position
}

type StructStmtMember struct {
	Name  Node
	Value Node
}

func (es *ExprStmt) Type() NodeKind     { return StmtExpr }
func (es *ExprStmt) Position() Position { return es.Expr.Position() }

func (ms *MetaStmt) Type() NodeKind     { return StmtMeta }
func (ms *MetaStmt) Position() Position { return ms.pos }

func (ss *StructStmt) Type() NodeKind     { return StmtStruct }
func (ss *StructStmt) Position() Position { return ss.pos }

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

func (ps *Parser) ParseModifierList() (map[Node]Node, error) {
	if !ps.matchesToken(TokenAt) {
		return nil, ErrNotModifiers
	}

	start := ps.Cursor().Position.Start
	ps.Advance(1)

	if !ps.matchesToken(TokenLParen) {
		return nil, LangError{ErrorSyntax, Position{Start: start, End: start + 1}, "expected start of modifier list"}
	}
	ps.Advance(1)

	modifiers := map[Node]Node{}
	for !ps.IsDone() && ps.Cursor().Kind != TokenRParen {
		key, err := ps.ParseExpr()
		if err != nil {
			return nil, err
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

		modifiers[key] = value
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

	items := map[Node]Node{}
	for !ps.IsDone() && ps.Cursor().Kind != TokenRBrace {
		var key *LiteralNode

		if ps.Cursor().Kind == TokenIdentifier {
			key = &LiteralNode{Token: ps.Cursor()}
			ps.Advance(1)
		} else {
			return nil, LangError{ErrorSyntax, ps.Cursor().Position, "key must be an identifier"}
		}

		if !ps.matchesToken(TokenColon) {
			return nil, LangError{ErrorSyntax, key.Position(), "expected colon after key in metadata mapping"}
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

		items[key] = value
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

func (ps *Parser) ParseStructStmt(start int, modifiers map[Node]Node) (Node, error) {
	lit, err := ps.ParseLiteral()
	if err != nil {
		return nil, err
	}

	if ident, ok := lit.(*LiteralNode); ok {
		if ident.Token.Kind != TokenIdentifier {
			return nil, LangError{
				ErrorSyntax,
				ident.Token.Position,
				fmt.Sprintf("expected identifier, not %s", ident.Token.Kind),
			}
		}

		if !ps.matchesToken(TokenLBrace) {
			return nil, LangError{
				ErrorSyntax,
				lit.Position(),
				"expected start of struct block",
			}
		}

		ps.Advance(1)

		fields := []StructStmtMember{}
		for !ps.IsDone() && ps.Cursor().Kind != TokenRBrace {
			key, err := ps.ParseExpr()
			if err != nil {
				return nil, err
			}

			if !ps.matchesToken(TokenColon) {
				pos := Position{Start: key.Position().End, End: key.Position().End + 1}
				return nil, LangError{ErrorSyntax, pos, "expected colon after struct member name"}
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
				return nil, LangError{ErrorSyntax, pos, "expected end or continuation of struct"}
			}

			fields = append(fields, StructStmtMember{key, value})
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

		return &StructStmt{Name: ident, Modifiers: modifiers, Fields: fields, pos: Position{start, end}}, nil
	} else {
		return nil, LangError{
			ErrorSyntax,
			lit.Position(),
			fmt.Sprintf("expected identifier, not %s", lit.Type()),
		}
	}
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
