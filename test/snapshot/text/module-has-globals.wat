(module
  (global (mut i32) (i32.const 0))
  (global i32 (i32.const 1))
  (global (mut i32) (global.get 1))
)
