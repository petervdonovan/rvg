  $ dune build

  $ dune exec -- ../../bin/main.exe 0.rvg
  
      add a0 a1 a2
      add t0 t1 t2
      add t0 t1 t2
  $ dune exec -- ../../bin/main.exe  1-multiple-files/a.rvg 1-multiple-files/b.rvg
  addi zero zero 0
  $ dune exec -- ../../bin/main.exe tokens 0.rvg
  {"kind": "function", "modifier": "", "range": { "file": "0.rvg", "range": [[3, 5], [3, 6]] } }
  {"kind": "function", "modifier": "", "range": { "file": "0.rvg", "range": [[4, 5], [4, 6]] } }
  {"kind": "function", "modifier": "defaultLibrary", "range": { "file": "0.rvg", "range": [[0, 2], [0, 7]] } }
  $ rbu -d -n 2.rvg
  a ok
  b ok
  $ rbu -d -n 3.rvg
  good 0
  good 1
  E(Lam(params=[E(Var(name=b), )], lbody=E(Name(b), ); E(Template(E(Asm(this should fail), )), )), ): [3.rvg] line 7, col 19 to line 7, col 29
  Expected lam of 2 parameters: [stdlib.rvg] line 12, col 13 to line 12, col 48
  [stdlib.rvg] line 7, col 4 to line 7, col 13: Assertion failed
  [stdlib.rvg] line 10, col 4 to line 12, col 50: Assertion failed
  [3.rvg] line 7, col 9 to line 7, col 17: Assertion failed
  [3.rvg] line 7, col 1 to line 7, col 52: Assertion failed
  [stdlib.rvg] line 49, col 2 to line 49, col 10: Assertion failed
  [3.rvg] line 1, col 2 to line 8, col 3: Assertion failed
  $ rbu -d -n 4.rvg
  testing lam!
  executing identity
  this is not a lam and should error: [4.rvg] line 8, col 6 to line 8, col 42
  Expected lam of 1 parameters: [stdlib.rvg] line 12, col 13 to line 12, col 48
  [stdlib.rvg] line 7, col 4 to line 7, col 13: Assertion failed
  [stdlib.rvg] line 10, col 4 to line 12, col 50: Assertion failed
  [4.rvg] line 8, col 6 to line 8, col 42: Assertion failed
  [4.rvg] line 8, col 4 to line 8, col 43: Assertion failed
  [stdlib.rvg] line 49, col 2 to line 49, col 10: Assertion failed
  [4.rvg] line 1, col 2 to line 9, col 3: Assertion failed
  $ rbu -d -n 5.rvg
  curried successfully
  first thing to print
  second thing to print
  $ rbu -d -n 6.rvg
  pass
  7
  Not a number: [6.rvg] line 7, col 4 to line 7, col 18
  Expected a number: [stdlib.rvg] line 21, col 19 to line 21, col 46
  [stdlib.rvg] line 7, col 4 to line 7, col 13: Assertion failed
  [stdlib.rvg] line 19, col 4 to line 21, col 48: Assertion failed
  [6.rvg] line 4, col 28 to line 4, col 32: Assertion failed
  [6.rvg] line 7, col 2 to line 7, col 19: Assertion failed
  [stdlib.rvg] line 49, col 2 to line 49, col 10: Assertion failed
  [6.rvg] line 1, col 2 to line 8, col 3: Assertion failed
  $ rbu -d -n 7.rvg
  ok nop
  ok addi
  ok nasty
  ok t0
  ok alphabet
  ok var-deref0
  ok var-deref1
  $ rbu -d -n 8.rvg
      addi t0, t0, 4
      add t0, t0, t1
      addi t0, t0, 67
  [8.rvg] line 14, col 3 to line 14, col 13: Expected 3 cycles but got 4
  [stdlib.rvg] line 49, col 2 to line 49, col 10: Expected 3 cycles but got 4
  [8.rvg] line 1, col 2 to line 15, col 3: Expected 3 cycles but got 4
  $ rbu -d -n 9.rvg
  15
  $ rbu -d -n 10.rvg
      bgeu zero, t0, DONE_tJ6slU7Skn
      lw t1, 0(t0)
  DONE_tJ6slU7Skn:
  $ rbu -d -n 11.rvg
      addi a0, zero, 0
      addi t0, zero, 0
      addi t1, zero, 10
  LOOP_N840jLsMYh:
      add a0, a0, t0
      addi t0, t0, 1
      blt t0, t1, LOOP_N840jLsMYh
  $ rbu -d -n 12.rvg
  3
  7
  11
  15
  19
  55
      addi t0, t1, 12
      addi t0, t1, 12
      addi t0, t1, 12
  $ rbu -d -n 13.rvg
  alphabet soup bishop
  $ rbu -d -n 14.rvg
  sterling 42
  y: [14.rvg] line 11, col 39 to line 11, col 42
  Expected a number: [stdlib.rvg] line 21, col 19 to line 21, col 46
  [stdlib.rvg] line 7, col 4 to line 7, col 13: Assertion failed
  [stdlib.rvg] line 19, col 4 to line 21, col 48: Assertion failed
  [14.rvg] line 11, col 39 to line 11, col 42: Assertion failed
  [14.rvg] line 11, col 2 to line 11, col 43: Assertion failed
  [stdlib.rvg] line 49, col 2 to line 49, col 10: Assertion failed
  [14.rvg] line 1, col 2 to line 12, col 3: Assertion failed
  $ rbu -d -n 15.rvg
  done computing prologue
      sw s4, 16(sp)
      sw s3, 12(sp)
      sw s2, 8(sp)
      sw s1, 4(sp)
      sw s0, 0(sp)
      addi sp, sp, 20
  now producing corresponding epilogue
      addi sp, sp, -20
      lw s0, 0(sp)
      lw s1, 4(sp)
      lw s2, 8(sp)
      lw s3, 12(sp)
      lw s4, 16(sp)
  $ rbu -d -n hover 3 10 15.rvg
  {"attrs": [], "typeUnionOf": ["block"], "cycles": 6, "cyclesMod": [], "range": { "file": "/home/peter/lf/rvg/_build/default/tests/integration.t/15.rvg", "range": [[3, 8], [3, 12]] }}
  $ rbu -d -n 17.rvg
  hello:
  HELLO_tJ6slU7Skn:
  Hello:
  $ dune exec -- ../../bin/main.exe rvg-stdlib/stdlib.rvg "18.0.rvg=[mu[] [[lam [(x)] [;; {21}] [[print {[id x] [id x]}]]] {hello}]]" "18.1.rvg=[stdlib 18.0]"
  hello hello
  $ rbu -d -n 19.rvg
  hello:
  start:
      lui t0, 764588
      addi t0, t0, -1366
      csrrw zero, 0x51e, t0
      csrrw zero, 0x51e, a0
      jalr zero, ra, 0
