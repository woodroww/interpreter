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

impl std::fmt::Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.token_literal()) // ?
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
        write!(f, "{}", self.token.literal())
    }
}

impl NodeInterface for Identifier {
    fn token_literal(&self) -> String {
        self.token.literal()
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
    Expression(Expression),

    Identifier(Identifier),
    Prefix(PrefixExpression),
    Infix(InfixExpression),
    Int(usize),
}

impl std::fmt::Display for ExpressionType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let out = match self {
            ExpressionType::NoExpression => "NoExpression",
            ExpressionType::Expression(_) => "Expression",
            ExpressionType::Prefix(_) => "Prefix",
            ExpressionType::Identifier(_) => "Identifier",
            ExpressionType::Int(_) => "Int",
            ExpressionType::Infix(_) => "Infix",
        };
        write!(f, "ExpressionType::{}", out)
    }
}

pub struct Expression {
    node: Node,
}

pub trait ExpressionInterface {
    fn expression_node();
}

impl std::fmt::Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Expression")
    }
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

pub struct PrefixExpression {
    pub token: Token, // Bang or Minus at this point
    //operator: String,
    pub right: Expression,
}

impl PrefixExpression {
    pub fn new(token: Token, /*operator: String,*/) -> Self {
        Self { token, /*operator,*/ right: Expression::new() }
    }

    pub fn operator(&self) -> String {
        self.token.literal()
    }
}

impl std::fmt::Display for PrefixExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}{})", /*self.operator*/ self.token.literal(), self.right)
    }
}

pub struct InfixExpression {
    pub token: Token,
    pub left: Box<ExpressionType>,
    //pub operator: String,
    pub right: Box<ExpressionType>,
}

impl InfixExpression {
    pub fn new(token: Token) -> Self {
        Self { token, left: Box::new(ExpressionType::NoExpression), right: Box::new(ExpressionType::NoExpression) }
    }

    pub fn operator(&self) -> String {
        self.token.literal()
    }
}

impl std::fmt::Display for InfixExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
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
        write!(f, "StatementType::")?;
        match self {
            StatementType::Let(s) => write!(f, "Let {}", s),
            StatementType::Return(s) => write!(f, "Return {}", s),
            StatementType::Expression(s) => write!(f, "Expression {}", s),
        }
    }
}

pub struct Program {
    pub statements: Vec<StatementType>,
}

impl std::fmt::Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Program statements:\n")?;
        for (_i, s) in self.statements.iter().enumerate() {
            write!(f, "{}\n", s)?;
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

impl std::fmt::Display for LetStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
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

impl std::fmt::Display for ReturnStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
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
        self.token.literal()
    }
}

impl ReturnStatement {
    pub fn new(token: Token) -> Self {
        Self { token, return_value: Expression { node: Node }  }
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
        write!(f, "{} token: {}", self.expression, self.token.token_type())
    }
}

impl StatementInterface for ExpressionStatement {
    fn statement_node() {
        todo!()
    }
}

impl NodeInterface for ExpressionStatement {
    fn token_literal(&self) -> String {
        self.token.literal()
    }
}

impl ExpressionStatement {
    pub fn new(token: Token) -> Self {
        Self { token, expression: ExpressionType::NoExpression }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    //use pretty_assertions::assert_eq;

    #[test]
    #[ignore]
    fn test_display() {
        let program = Program::new();
        let mut let_statement = LetStatement::new(Token::Let);
        let_statement.name = Some(Identifier::new(Token::Ident("myVar".to_string())));
        //let_statement.value = 
        assert!(false);
    }
}
