use crate::token::Token;

// -----------------------------------------------------------------------------
//  NodeInterface 
// -----------------------------------------------------------------------------

pub trait NodeInterface {
    fn token_literal(&self) -> String;
}

// -----------------------------------------------------------------------------
//  StatementInterface
// -----------------------------------------------------------------------------

pub trait StatementInterface {
    fn statement_node();
}

// -----------------------------------------------------------------------------
//  Identifier
// -----------------------------------------------------------------------------

#[derive(Debug)]
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
        Self { token }
    }
}

#[derive(Debug)]
pub struct BooleanExpression {
    pub token: Token,
    pub value: bool,
}

impl std::fmt::Display for BooleanExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.token.literal)
    }
}

impl NodeInterface for BooleanExpression {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl BooleanExpression {
    pub fn new(token: Token) -> Self {
        let value = match &token.literal as &str {
            "true" => true,
            "false" => false,
            _ => panic!("invalid value for a Boolean"),
        };
        Self { token, value }
    }
}

#[derive(Debug)]
pub struct BlockStatement {
    token: Token,
    statements: Vec<StatementType>,
}

impl std::fmt::Display for BlockStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        //write!(f, "BlockStatement:")
        for s in self.statements.iter() {
            write!(f, "{}", s)?;
        }
        Ok(())
    }
}

#[derive(Debug)]
pub struct IfExpression {
    pub token: Token,
    pub condition: Box<Expression>,
    pub consequence: Box<BlockStatement>,
    pub alternative: Option<Box<BlockStatement>>,
}

impl NodeInterface for IfExpression {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl std::fmt::Display for IfExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        //write!(f, "IfExpression:")?;
        write!(f, "if{} {}", self.condition, self.consequence)?;
        match &self.alternative {
            Some(alt) => {
                write!(f, "else {}", alt)
            }
            None => Ok(()),
        }
    }
}

// -----------------------------------------------------------------------------
//  Expression
// -----------------------------------------------------------------------------

#[derive(Debug)]
pub enum Expression {
    Identifier(Identifier),
    Prefix(PrefixExpression),
    Infix(InfixExpression),
    Int(usize),
    Return,
    Assign,
    Boolean(BooleanExpression),
    If(IfExpression),
}

impl std::fmt::Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        //write!(f, "ExpressionType:")?;
        match self {
            Expression::Prefix(prefix) => write!(f, "{}", prefix),
            Expression::Identifier(ident) => write!(f, "{}", ident),
            Expression::Int(integer) => write!(f, "{}", integer),
            Expression::Infix(infix) => write!(f, "{}", infix), 
            Expression::Return => write!(f, "return"),
            Expression::Assign => write!(f, "="),
            Expression::Boolean(b) => write!(f, "{}", b.value),
            Expression::If(_) => todo!(),
        }
    }
}

#[derive(Debug)]
pub struct PrefixExpression {
    pub token: Token, // Bang or Minus at this point
    pub operator: String,
    pub right: Option<Box<Expression>>,
}

impl PrefixExpression {
    pub fn new(token: Token, operator: &str) -> Self {
        Self {
            token,
            operator: operator.to_string(),
            right: None,
        }
    }
}

impl std::fmt::Display for PrefixExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        //write!(f, "PrefixExpression:")?;
        write!(f, "(")?;
        write!(f, "{}", self.operator)?;
        let right = match &self.right {
            Some(r) => format!("{}", r),
            None => format!(""),
        };
        write!(f, "{}", right)?;
        write!(f, ")")
    }
}

#[derive(Debug)]
pub struct InfixExpression {
    pub token: Token,
    pub left: Option<Box<Expression>>,
    pub operator: String,
    pub right: Option<Box<Expression>>,
}

impl InfixExpression {
    pub fn new(token: Token, operator: &str, left: Expression) -> Self {
        Self {
            token,
            left: Some(Box::new(left)),
            operator: operator.to_string(),
            right: None,
        }
    }
}

impl std::fmt::Display for InfixExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        //write!(f, "InfixExpression:")?;
        write!(f, "(")?;
        let left = match &self.left {
            Some(l) => format!("{}", l),
            None => format!(""),
        };
        let right = match &self.right {
            Some(r) => format!("{}", r),
            None => format!(""),
        };
        write!(f, "{}", left)?;
        write!(f, " {}", self.operator)?;
        write!(f, " {}", right)?;
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

#[derive(Debug)]
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

#[derive(Debug)]
pub struct LetStatement {
    pub token: Token,
    pub name: Option<Identifier>,
    pub value: Option<Expression>,
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
        let value = match &self.value {
            Some(v) => format!("{}", v),
            None => format!(""),
        };
        write!(f, " = {}", value)?;
        write!(f, ";")
    }
}

impl LetStatement {
    pub fn new(token: Token) -> Self {
        Self {
            token,
            name: None,
            value: None,
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

#[derive(Debug)]
pub struct ReturnStatement {
    pub token: Token,
    pub return_value: Expression,
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
            return_value: Expression::Return,
        }
    }
}

// -----------------------------------------------------------------------------
//  ExpressionStatement
// -----------------------------------------------------------------------------
// basically a wrapper for an expression, like when you type `1 + 1` in the
// python REPL and you get 2, no let, no return

#[derive(Debug)]
pub struct ExpressionStatement {
    pub token: Token,
    pub expression: Option<Expression>,
}

impl std::fmt::Display for ExpressionStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        //write!(f, "ExpressionStatement:")?;
        let expression = match &self.expression {
            Some(e) => format!("{}", e),
            None => format!(""),
        };
        write!(f, "{}", expression)
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
            expression: None,
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use pretty_assertions::assert_eq;
    use crate::token::TokenType;

    #[test]
    fn test_display() { // TestString in book
        let program = Program::new();
        let mut let_statement = LetStatement::new(Token::new(TokenType::Let, "let"));
        let_statement.name = Some(Identifier::new(Token::new(TokenType::Ident, "myVar")));
        let_statement.value = Some(Expression::Identifier(Identifier::new(Token::new(TokenType::Ident, "anotherVar"))));
        
        assert_eq!(&format!("{}", let_statement), "let myVar = anotherVar;");
    }
}
