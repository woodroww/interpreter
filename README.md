Following along with Thorsten Ball's book [Writing An Interpreter In Go](https://interpreterbook.com/), but in Rust.


/*
pub struct Lexer<'a> {
    chars: Peekable<Chars<'a>>,

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {

pub struct Program {
    pub statements: Vec<StatementType>,

Lexer iterates over chars to produce Tokens.
Parser iterates over Tokens to produce a Program of StatementType(s).
Compiler takes a program and generates bytecode.

pub enum StatementType 
    Let(LetStatement),
        pub struct LetStatement {
            pub token: Token,
            pub name: Option<Identifier>,
            pub value: Option<Expression>,
        }
    Return(ReturnStatement),
        pub struct ReturnStatement {
            pub token: Token,
            pub return_value: Option<Expression>,
        }
    Expression(ExpressionStatement),
        pub struct ExpressionStatement {
            pub token: Token,
            pub expression: Option<Expression>,
        }
            pub enum Expression {
                Identifier(Identifier),
                Prefix(PrefixExpression),
                Infix(InfixExpression),
                Int(isize),
                Return,
                Assign,
                Boolean(BooleanExpression),
                If(IfExpression),
                FunctionLiteral(FunctionLiteralExpression),
                Call(CallExpression),
                String(StringLiteral),
                ArrayLiteral(ArrayLiteral),
                IndexExpression(IndexExpression),
                Hash(HashLiteral),
            }
    Block(BlockStatement),
        pub struct BlockStatement {
            pub token: Token,
            pub statements: Vec<StatementType>,
        }

pub struct Instructions(pub Vec<u8>);
struct Compiler {
    instructions: Instructions,
    constants: Vec<Object>,
}
struct Bytecode {
    instructions: Instructions,
    constants: Vec<Object>,
}
Compiler
    pub fn compile(&self, program: Program) -> Result<(), ()> {
    pub fn bytecode(&self) -> Bytecode {



        */

