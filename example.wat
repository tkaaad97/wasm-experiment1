(module
  (import "" "printf" (func $printf (param i32 i32 i32) (result)))
  (func $add (param $a i32) (param $b i32) (result i32)
    i32.const 100
    local.get $a
    local.get $b
    call $printf
    local.get $a
    local.get $b
    i32.add
  )
  (memory 1)
  (data (i32.const 100) "hello, world. first: %d, second: %d")
  (data (i32.const 200) "test")
  (export "mem" (memory 0))
  (export "add" (func $add))
)
