use crate::token::Token;

// -----------------------------------------------------------------------------
//  Node
// -----------------------------------------------------------------------------

pub struct Node;

pub trait NodeInterface {
    fn token_literal(&self) -> String;
}

// -----------------------------------------------------------------------------
//  Statement 
// -----------------------------------------------------------------------------
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

// -----------------------------------------------------------------------------
//  Expression
// -----------------------------------------------------------------------------

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
// -----------------------------------------------------------------------------
//  Program
// -----------------------------------------------------------------------------
// This Program node is going to be the root node of every AST our parser produces. Every valid
// Monkey program is a series of statements. These statements are contained in the
// Program.Statements, which is just a slice of AST nodes that implement the Statement interface.

pub enum StatementType {
    Let(LetStatement),
    Return(ReturnStatement),
}

pub struct Program {
    pub statements: Vec<StatementType>,
}

impl NodeInterface for Program {
    fn token_literal(&self) -> String {
        if self.statements.len() > 0 {
            //self.statements[0].token_literal()
            "I'm coming back for you baby!".to_string()
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

// -----------------------------------------------------------------------------
//  LetStatement
// -----------------------------------------------------------------------------
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

// -----------------------------------------------------------------------------
//  ReturnStatement
// -----------------------------------------------------------------------------

pub struct ReturnStatement {
    pub token: Token,
    pub return_value: Expression,
}

impl StatementInterface for ReturnStatement {
    fn statement_node() {
        todo!()
    }
}

impl NodeInterface for ReturnStatement {
    fn token_literal(&self) -> String {
        self.token.literal()
    }
}

impl ReturnStatement {
    pub fn new(token: Token) -> Self {
        Self { token, return_value: Expression { node: Node }  }
    }
}

// -----------------------------------------------------------------------------
// Identifier 
// -----------------------------------------------------------------------------

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
