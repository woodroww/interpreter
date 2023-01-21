use std::{cell::RefCell, rc::Rc};

use crate::{environment::Environment, ast::{BlockStatement, Identifier}};

#[derive(Eq, PartialEq, Debug, Clone)]
pub enum Object {
    Integer(isize),
    Boolean(bool),
    Return(Box<Object>),
    Error(String),
    Function(FunctionObject),
    Null,
}

impl std::fmt::Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::Integer(n) => write!(f, "{}", n),
            Object::Boolean(b) => match b {
                true => write!(f, "true"),
                false => write!(f, "false"),
            }
            Object::Return(obj) => write!(f, "{}", obj),
            Object::Null => write!(f, "Null"),
            Object::Error(err) => write!(f, "error {}", err),
            Object::Function(_) => todo!(),
        }
    }
}

impl Object {
    pub fn new_error(error_string: &str) -> Object {
        Self::Error(error_string.to_string())
    }
}

#[derive(Eq, PartialEq, Debug, Clone)]
pub struct FunctionObject {
    pub parameters: Vec<Identifier>,
    pub body: Option<BlockStatement>,
    pub env: Rc<RefCell<Environment>>,
}

impl std::fmt::Display for FunctionObject {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "fn(")?;
        for (i, parameter) in self.parameters.iter().enumerate() {
            if i == self.parameters.len() - 1 {
                write!(f, "{}", parameter)?;
            } else {
                write!(f, "{}, ", parameter)?;
            }
        }
        write!(f, ") {{\n")?;
        if self.body.is_some() {
            write!(f, "{}", self.body.as_ref().unwrap())?;
        } else {
            write!(f, "")?;
        }
        write!(f, "\n}}")
    }
}




