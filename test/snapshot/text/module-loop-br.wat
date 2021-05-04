(module
  (type (;0;) (func (param i32) (result i32)))
  (func (;0;) (type 0) (param i32) (result i32)
    (local i32 i32)
    block
      loop
        local.get 0
        local.get 1
        i32.le_s
        br_if 0
        local.get 2
        local.get 1
        i32.add
        local.set 2
        local.get 1
        i32.const 1
        local.set 1
        br 1
      end
    end
    local.get 2
    return
  )
  (export "sum" (func 0))
)
