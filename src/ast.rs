use crate::token::Token;

pub struct Node;

pub trait NodeInterface {
    fn token_literal(&self) -> String;
}

pub struct Statement {
    node: Node,
}

pub trait StatementInterface {
    fn statement_node();
}

impl NodeInterface for Statement {
    fn token_literal(&self) -> String {
        "statement".to_string()
    }
}

impl StatementInterface for Statement {
    fn statement_node() {
        todo!()
    }
}

pub struct Expression {
    node: Node,
}

pub trait ExpressionInterface {
    fn expression_node();
}

impl ExpressionInterface for Expression {
    fn expression_node() {
        todo!()
    }
}

impl Expression {
    pub fn new() -> Self {
        Self { node: Node }
    }
}
// This Program node is going to be the root node of every AST our parser produces. Every valid
// Monkey program is a series of statements. These statements are contained in the
// Program.Statements, which is just a slice of AST nodes that implement the Statement interface.

pub struct Program<T>
    where
        T: NodeInterface + StatementInterface {
    pub statements: Vec<T>,
}

impl<T> NodeInterface for Program<T>
    where
        T: NodeInterface + StatementInterface {
    fn token_literal(&self) -> String {
        if self.statements.len() > 0 {
            self.statements[0].token_literal()
        } else {
            "".to_string()
        }
    }
}

impl<T> Program<T>
    where
        T: NodeInterface + StatementInterface {
    pub fn new() -> Self {
        Self { statements: Vec::new() }
    }
}

/*
type LetStatement struct {
    Token token.Token // the token.LET token
    Name  *Identifier
    Value Expression
}
*/
pub struct LetStatement {
    pub token: Token,
    pub name: Option<Identifier>,
    pub value: Expression,
}

impl LetStatement {
    pub fn new(token: Token) -> Self {
        Self { token, name: None, value: Expression { node: Node }  }
    }
}

impl StatementInterface for LetStatement {
    fn statement_node() {
        todo!()
    }
}

impl NodeInterface for LetStatement {
    fn token_literal(&self) -> String {
        self.token.literal()
    }
}

pub struct Identifier {
    pub token: Token,
    pub value: String,
}

impl NodeInterface for Identifier {
    fn token_literal(&self) -> String {
        self.token.literal()
    }
}

impl Identifier {
    pub fn new(token: Token, value: String) -> Self {
        Identifier { token, value }
    }
}
