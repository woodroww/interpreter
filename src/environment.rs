use std::collections::HashMap;

use crate::object::Object;

pub struct Environment {
    store: HashMap<String, Object>,
}

impl Environment {
    pub fn new() -> Self {
        Self { store: HashMap::new() }
    }
    pub fn get(&self, name: String) -> Option<&Object> {
        println!("get environment name: {}", name);
        let map_get = self.store.get(&name);
        if self.store.contains_key("a") {
            println!("self.store.contains_key(\"a\") == true");
        } else {
            println!("self.store.contains_key(\"a\") == false");
        }
        if map_get.is_some() {
            println!("get environment name: {} Some", name);
        } else {
            println!("get environment name: {} None", name);
        }
        map_get
    }
    pub fn set(&mut self, name: String, value: Object) -> Object {
        println!("set environment name: {}, value: {}", name, value);
        let return_value = value.clone();
        self.store.insert(name, value);
        return_value
    }
}
