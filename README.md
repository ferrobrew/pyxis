A DSL for describing types for existing structures and classes in memory.

Will fill out with more detail later.

Want to add:
  - in-built helpers that the backend can generate if possible
    - field array pairs
      - e.g. you can pair `models_ptr: *mut Model` and `models_size: u32` with `models: [models_ptr; models_size]` and a `models(&mut self) -> &[*mut Model]` will automatically be generated