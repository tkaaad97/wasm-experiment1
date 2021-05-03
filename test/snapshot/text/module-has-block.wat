(module
  (type (;0;) (func (result i32)))
  (func (;0;) (type 0) (result i32)
    block (result i32)
      i32.const 0
      block (result i32)
        i32.const 0
        i32.const 1
        i32.add
      end
      i32.add
    end
  )
  (export "blockfunc" (func 0))
)
