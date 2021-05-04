(module
  (type (;0;) (func (param i32 i32) (result i32)))
  (func (;0;) (type 0) (param i32 i32) (result i32)
    local.get 0
    local.get 1
    i32.eq
    if (result i32)
      i32.const 0
    else
      i32.const 100
    end
  )
  (export "iffunc" (func 0))
)
