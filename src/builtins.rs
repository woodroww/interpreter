use std::collections::HashMap;

use crate::object::Object;

pub type BuiltinFn = fn(Vec<Object>) -> Option<Object>;

pub struct Builtins {
    builtins: HashMap<String, BuiltinFn>,
}

impl Builtins {
    pub fn new() -> Self {
        let mut map: HashMap<String, BuiltinFn> = HashMap::new();
        map.insert("len".to_string(), len_builtin);
        Self {
            builtins: map
        }
    }
    pub fn get(&self, ident: &str) -> Option<&BuiltinFn> {
        self.builtins.get(ident)
    }
}

pub fn len_builtin(args: Vec<Object>) -> Option<Object> {
    if args.len() != 1 {
        Some(Object::Error(format!("wrong number of arguments. got {}, expected 1", args.len())))
    } else {
        match &args[0] {
            Object::String(s) => {
                Some(Object::Integer(s.len() as isize))
            }
            _ => {
                Some(Object::Error(format!("argument to `len` not supported, got {}", args[0].type_string())))
            }
        }
    }
}
