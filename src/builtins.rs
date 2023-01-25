use std::collections::HashMap;

use crate::object::{Object, ArrayObject};

pub type BuiltinFn = fn(Vec<Object>) -> Option<Object>;

pub struct Builtins {
    builtins: HashMap<String, BuiltinFn>,
}

impl Builtins {
    pub fn new() -> Self {
        let mut map: HashMap<String, BuiltinFn> = HashMap::new();
        map.insert("len".to_string(), len_builtin);
        map.insert("first".to_string(), first_builtin);
        map.insert("last".to_string(), last_builtin);
        map.insert("rest".to_string(), rest_builtin);
        map.insert("push".to_string(), push_builtin);
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
            Object::Array(a) => {
                Some(Object::Integer(a.elements.len() as isize))
            }
            _ => {
                Some(Object::Error(format!("argument to `len` not supported, got {}", args[0].type_string())))
            }
        }
    }
}

pub fn first_builtin(args: Vec<Object>) -> Option<Object> {
    if args.len() != 1 {
        Some(Object::Error(format!("wrong number of arguments. got {}, expected 1", args.len())))
    } else {
        match &args[0] {
            Object::Array(obj) => {
                if obj.elements.len() > 0 {
                    Some(obj.elements[0].clone())
                } else {
                    Some(Object::Null)
                }
            }
            _ => {
                Some(Object::Error(format!("argument to `first` must be ARRAY, got {}", args[0].type_string())))
            }
        }
    }
}

pub fn last_builtin(args: Vec<Object>) -> Option<Object> {
    if args.len() != 1 {
        Some(Object::Error(format!("wrong number of arguments. got {}, expected 1", args.len())))
    } else {
        match &args[0] {
            Object::Array(obj) => {
                let length = obj.elements.len();
                if length > 0 {
                    Some(obj.elements[length - 1].clone())
                } else {
                    Some(Object::Null)
                }
            }
            _ => {
                Some(Object::Error(format!("argument to `last` must be ARRAY, got {}", args[0].type_string())))
            }
        }
    }
}

pub fn rest_builtin(args: Vec<Object>) -> Option<Object> {
    if args.len() != 1 {
        Some(Object::Error(format!("wrong number of arguments. got {}, expected 1", args.len())))
    } else {
        match &args[0] {
            Object::Array(obj) => {
                let length = obj.elements.len();
                if length > 0 {
                    let new_elements = obj.elements.iter().cloned().skip(1).collect();
                    Some(Object::Array(ArrayObject::new(new_elements)))
                } else {
                    Some(Object::Null)
                }
            }
            _ => {
                Some(Object::Error(format!("argument to `rest` must be ARRAY, got {}", args[0].type_string())))
            }
        }
    }
}

pub fn push_builtin(args: Vec<Object>) -> Option<Object> {
    if args.len() != 2 {
        Some(Object::Error(format!("wrong number of arguments. got {}, expected 2", args.len())))
    } else {
        match &args[0] {
            Object::Array(obj) => {
                let mut array = obj.elements.clone();
                array.push(args[1].clone());
                Some(Object::Array(ArrayObject::new(array)))
            }
            _ => {
                Some(Object::Error(format!("argument to `push` must be ARRAY, got {}", args[0].type_string())))
            }
        }
    }
}
