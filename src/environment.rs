use std::{collections::HashMap, cell::RefCell, rc::Rc};

use crate::object::Object;

#[derive(Eq, PartialEq, Debug, Clone)]
pub struct Environment {
    store: HashMap<String, Object>,
    outer: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new() -> Self {
        Self { store: HashMap::new(), outer: None }
    }
    pub fn with_outer(mut self, outer: Rc<RefCell<Environment>>) -> Self {
        self.outer = Some(outer); // clone ???
        self
    }

    pub fn get(&self, name: String) -> Option<Object> {
        let obj = self.store.get(&name);
        if obj.is_none() && self.outer.is_some() {
            let outer = self.outer.as_ref().unwrap().borrow();
            outer.get(name)
        } else {
            obj.cloned()
        }
    }
    pub fn set(&mut self, name: String, value: Object) -> Object {
        self.store.insert(name, value.clone());
        value
    }
}
