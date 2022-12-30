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

struct Expression {
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

// This Program node is going to be the root node of every AST our parser produces. Every valid
// Monkey program is a series of statements. These statements are contained in the
// Program.Statements, which is just a slice of AST nodes that implement the Statement interface.

pub struct Program {
    pub statements: Vec<Statement>,
}

impl NodeInterface for Program {
    fn token_literal(&self) -> String {
        if self.statements.len() > 0 {
            self.statements[0].token_literal()
        } else {
            "".to_string()
        }
    }
}

impl Program {
    pub fn new() -> Self {
        Self { statements: Vec::new() }
    }
}

pub struct LetStatement {
    pub token: Token,
    pub name: Option<Identifier>,
}

impl NodeInterface for LetStatement {
    fn token_literal(&self) -> String {
        todo!()
    }
}

impl LetStatement {
    pub fn new(token: Token) -> Self {
        Self { token, name: None }
    }
}

pub struct Identifier {
    token: Token,
    value: String,
}

impl NodeInterface for Identifier {
    fn token_literal(&self) -> String {
        todo!()
    }
}

impl Identifier {
    pub fn new(token: Token, value: String) -> Self {
        Identifier { token, value }
    }
}
