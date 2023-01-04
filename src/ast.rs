use crate::token::{TokenType, Token};

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

impl std::fmt::Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Statement:")
        //write!(f, "{}", self.token_literal()) // ?
    }
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
// Identifier
// -----------------------------------------------------------------------------

pub struct Identifier {
    pub token: Token,
}

impl std::fmt::Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        //write!(f, "Identifier:")?;
        write!(f, "{}", self.token.literal)
    }
}

impl NodeInterface for Identifier {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl Identifier {
    pub fn new(token: Token) -> Self {
        Identifier { token }
    }
}

// -----------------------------------------------------------------------------
//  Expression
// -----------------------------------------------------------------------------

pub enum ExpressionType {
    NoExpression,
    Identifier(Identifier),
    Prefix(PrefixExpression),
    Infix(InfixExpression),
    Int(usize),
    Return,
    Assign,
}

impl std::fmt::Display for ExpressionType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        //write!(f, "ExpressionType:")?;
        match self {
            ExpressionType::NoExpression => write!(f, "NoExpression"),
            ExpressionType::Prefix(prefix) => write!(f, "{}", prefix),
            ExpressionType::Identifier(ident) => write!(f, "{}", ident),
            ExpressionType::Int(integer) => write!(f, "{}", integer),
            ExpressionType::Infix(infix) => write!(f, "{}", infix), 
            ExpressionType::Return => write!(f, "return"),
            ExpressionType::Assign => write!(f, "="),
        }
    }
}

pub struct PrefixExpression {
    pub token: Token, // Bang or Minus at this point
    pub operator: String,
    pub right: Box<ExpressionType>,
}

impl PrefixExpression {
    pub fn new(token: Token, operator: &str) -> Self {
        Self {
            token,
            operator: operator.to_string(),
            right: Box::new(ExpressionType::NoExpression),
        }
    }
}

impl std::fmt::Display for PrefixExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        //write!(f, "PrefixExpression:")?;
        write!(f, "(")?;
        write!(f, "{}", self.operator)?;
        write!(f, "{}", *self.right)?;
        write!(f, ")")
    }
}

pub struct InfixExpression {
    pub token: Token,
    pub left: Box<ExpressionType>,
    pub operator: String,
    pub right: Box<ExpressionType>,
}

impl InfixExpression {
    pub fn new(token: Token, operator: &str, left: ExpressionType) -> Self {
        Self {
            token,
            left: Box::new(left),
            operator: operator.to_string(),
            right: Box::new(ExpressionType::NoExpression),
        }
    }
}

impl std::fmt::Display for InfixExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        //write!(f, "InfixExpression:")?;
        write!(f, "(")?;
        write!(f, "{}", *self.left)?;
        write!(f, " {}", self.operator)?;
        write!(f, " {}", *self.right)?;
        write!(f, ")")
        //write!(f, "")
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
    Expression(ExpressionStatement),
}

impl std::fmt::Display for StatementType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        //write!(f, "StatementType:")?;
        match self {
            StatementType::Let(s) => write!(f, "{}", s),
            StatementType::Return(s) => write!(f, "{}", s),
            StatementType::Expression(s) => write!(f, "{}", s),
        }
    }
}

pub struct Program {
    pub statements: Vec<StatementType>,
}

impl std::fmt::Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        //write!(f, "Program:")?;
        for (_i, s) in self.statements.iter().enumerate() {
            write!(f, "{}", s)?;
        }
        write!(f, "")
    }
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
        Self {
            statements: Vec::new(),
        }
    }
}

// -----------------------------------------------------------------------------
//  LetStatement
// -----------------------------------------------------------------------------

pub struct LetStatement {
    pub token: Token,
    pub name: Option<Identifier>,
    pub value: ExpressionType,
}

impl std::fmt::Display for LetStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        //write!(f, "LetStatement:")?;
        write!(f, "{}", self.token_literal() + " ")?;
        let name: String = match &self.name {
            Some(ident) => format!("{}", ident),
            None => "unnamed".to_string(),
        };
        write!(f, "{}", name)?;
        write!(f, " = {}", self.value)?;
        write!(f, ";")
    }
}

impl LetStatement {
    pub fn new(token: Token) -> Self {
        Self {
            token,
            name: None,
            value: ExpressionType::NoExpression,
        }
    }
}

impl StatementInterface for LetStatement {
    fn statement_node() {
        todo!()
    }
}

impl NodeInterface for LetStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

// -----------------------------------------------------------------------------
//  ReturnStatement
// -----------------------------------------------------------------------------

pub struct ReturnStatement {
    pub token: Token,
    pub return_value: ExpressionType,
}

impl std::fmt::Display for ReturnStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "ReturnStatement:")?;
        write!(f, "{} ", self.token_literal())?;
        write!(f, "{}", self.return_value)?;
        write!(f, ";")
    }
}

impl StatementInterface for ReturnStatement {
    fn statement_node() {
        todo!()
    }
}

impl NodeInterface for ReturnStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl ReturnStatement {
    pub fn new(token: Token) -> Self {
        Self {
            token,
            return_value: ExpressionType::Return,
        }
    }
}

// -----------------------------------------------------------------------------
//  ExpressionStatement
// -----------------------------------------------------------------------------
// basically a wrapper for an expression, like when you type `1 + 1` in the
// python REPL and you get 2, no let, no return

pub struct ExpressionStatement {
    pub token: Token,
    pub expression: ExpressionType,
}

impl std::fmt::Display for ExpressionStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        //write!(f, "ExpressionStatement:")?;
        write!(f, "{}", self.expression)//, self.token)
    }
}

impl StatementInterface for ExpressionStatement {
    fn statement_node() {
        todo!()
    }
}

impl NodeInterface for ExpressionStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl ExpressionStatement {
    pub fn new(token: Token) -> Self {
        Self {
            token,
            expression: ExpressionType::NoExpression,
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn test_display() { // TestString in book
        let program = Program::new();
        let mut let_statement = LetStatement::new(Token::new(TokenType::Let, "let"));
        let_statement.name = Some(Identifier::new(Token::new(TokenType::Ident, "myVar")));
        let_statement.value = ExpressionType::Identifier(Identifier::new(Token::new(TokenType::Ident, "anotherVar")));
        
        assert_eq!(&format!("{}", let_statement), "let myVar = anotherVar;");
    }
}
