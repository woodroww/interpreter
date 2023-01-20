#[derive(Eq, PartialEq, Debug, Clone)]
pub enum Object {
    Integer(isize),
    Boolean(bool),
    Return(Box<Object>),
    Error(String),
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
        }
    }
}

impl Object {
    pub fn new_error(error_string: &str) -> Object {
        Self::Error(error_string.to_string())
    }
}
