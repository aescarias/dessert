package lang

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
	Metadata map[string]Node
	pos      Position
}

// A StructStmt represents a "struct" statement which declares a structure
// containing a collection of fields.
type StructStmt struct {
	Name      string
	Modifiers map[string]Node
	Fields    []StructStmtField
	pos       Position
}

// A StructStmtField represents a field inside a "struct" statement.
type StructStmtField struct {
	Name      string
	Value     Node
	Modifiers map[string]Node
}

func (es *ExprStmt) Type() NodeKind     { return StmtExpr }
func (es *ExprStmt) Position() Position { return es.Expr.Position() }

func (ms *MetaStmt) Type() NodeKind     { return StmtMeta }
func (ms *MetaStmt) Position() Position { return ms.pos }

func (ss *StructStmt) Type() NodeKind     { return StmtStruct }
func (ss *StructStmt) Position() Position { return ss.pos }
