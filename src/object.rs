#[derive(Eq, PartialEq, Debug)]
pub enum Object {
    Integer(usize),
    Boolean(bool),
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
            Object::Null => write!(f, "Null"),
        }
    }
}
