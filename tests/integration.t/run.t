  $ dune exec -- ../../bin/main.exe 0.rvg
      add t0, t1, t2
      add a0, a1, a2
      add t0, t1, t2
  $ dune exec -- ../../bin/main.exe 1-multiple-files/a.rvg 1-multiple-files/b.rvg
      addi zero, zero, 0
  $ dune exec -- ../../bin/main.exe tokens 0.rvg
  {"kind": "string", "modifier": "", "range": { "file": "0.rvg", "range": [[4, 7], [4, 9]] } }
  {"kind": "function", "modifier": "", "range": { "file": "0.rvg", "range": [[4, 5], [4, 6]] } }
  {"kind": "string", "modifier": "", "range": { "file": "0.rvg", "range": [[6, 13], [6, 15]] } }
  {"kind": "string", "modifier": "", "range": { "file": "0.rvg", "range": [[2, 7], [2, 9]] } }
  {"kind": "function", "modifier": "", "range": { "file": "0.rvg", "range": [[2, 5], [2, 6]] } }
  {"kind": "string", "modifier": "", "range": { "file": "0.rvg", "range": [[6, 13], [6, 15]] } }
  {"kind": "function", "modifier": "defaultLibrary", "range": { "file": "0.rvg", "range": [[0, 2], [0, 7]] } }
  $ dune exec -- ../../bin/main.exe stdlib.rvg 2.rvg
  a ok
  b ok
  $ dune exec -- ../../bin/main.exe stdlib.rvg 3.rvg
  good 0
  good 1
  E(Lam(params=[E(Var(name=b), )], lbody=E(Name(b), ); E(Template(E(Asm(this should fail), )), )), ): [3.rvg] line 6, col 20 to line 6, col 30
  Expected lam of 2 parameters: [stdlib.rvg] line 11, col 12 to line 11, col 47
  [stdlib.rvg] line 6, col 3 to line 6, col 12: Assertion failed
  $ dune exec -- ../../bin/main.exe stdlib.rvg 4.rvg
  testing lam!
  executing identity
  this is not a lam and should error: [4.rvg] line 7, col 5 to line 7, col 41
  Expected lam of 1 parameters: [stdlib.rvg] line 11, col 12 to line 11, col 47
  [stdlib.rvg] line 6, col 3 to line 6, col 12: Assertion failed
  $ dune exec -- ../../bin/main.exe stdlib.rvg 5.rvg
  curried successfully
  first thing to print
  second thing to print
  $ dune exec -- ../../bin/main.exe stdlib.rvg 6.rvg
  pass
  7
  Not a number: [6.rvg] line 6, col 3 to line 6, col 17
  Expected a number: [stdlib.rvg] line 20, col 18 to line 20, col 45
  [stdlib.rvg] line 6, col 3 to line 6, col 12: Assertion failed
  $ dune exec -- ../../bin/main.exe stdlib.rvg 7.rvg
  ok 23
  ok nop
  ok addi
  ok nasty
  ok t0
  ok alphabet
  ok var-deref0
  ok var-deref1
  $ dune exec -- ../../bin/main.exe stdlib.rvg 8.rvg
      addi t0, t0, 4
      add t0, t0, t1
      addi t0, t0, 67
  [8.rvg] line 13, col 2 to line 13, col 14: Expected 3 cycles but got 4
  $ dune exec -- ../../bin/main.exe 9.rvg
  15
  $ dune exec -- ../../bin/main.exe stdlib.rvg ctrl.rvg 10.rvg
      bgeu zero, t0, DONE_tJ6slU7Skn
      lw t1, 0(t0)
  DONE_tJ6slU7Skn:
  $ dune exec -- ../../bin/main.exe stdlib.rvg ctrl.rvg 11.rvg
      addi a0, zero, 0
      addi t0, zero, 0
      addi t1, zero, 10
  LOOP_tJ6slU7Skn:
      add a0, a0, t0
      addi t0, t0, 1
      blt t0, t1, LOOP_tJ6slU7Skn
  $ dune exec -- ../../bin/main.exe stdlib.rvg ctrl.rvg 12.rvg
  3
  7
  11
  15
  19
  55
      addi t0, t1, 12
      addi t0, t1, 12
      addi t0, t1, 12
  $ dune exec -- ../../bin/main.exe 13.rvg
  alphabet soup bishop
  $ dune exec -- ../../bin/main.exe stdlib.rvg 14.rvg
  sterling 42
  y: [14.rvg] line 13, col 38 to line 13, col 41
  Expected a number: [stdlib.rvg] line 20, col 18 to line 20, col 45
  [stdlib.rvg] line 6, col 3 to line 6, col 12: Assertion failed
  $ dune exec -- ../../bin/main.exe stdlib.rvg ctrl.rvg 15.rvg
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
  $ dune exec -- ../../bin/main.exe hover 3 10 15.rvg stdlib.rvg ctrl.rvg 15.rvg
  {"attrs": [], "typeUnionOf": ["block"], "cycles": 6, "cyclesMod": [], "range": { "file": "15.rvg", "range": [[3, 8], [3, 12]] }}
  $ dune exec -- ../../bin/main.exe stdlib.rvg 17.rvg
  hello:
  HELLO_tJ6slU7Skn:
  Hello_vWRvZ2Pn4y:
  $ dune exec -- ../../bin/main.exe stdlib.rvg "18.0.rvg=[mu[] [[lam [(x)] [;; {21}] [[print {[id x] [id x]}]]] {hello}]]" "18.1.rvg=[stdlib 18.0]"
  hello hello
  $ dune exec -- ../../bin/main.exe stdlib.rvg ctrl.rvg 19.rvg
  hello:
  start:
      lui t0, 764588
      addi t0, t0, -1366
      csrrw zero, 0x51e, t0
      csrrw zero, 0x51e, a0
      jalr zero, ra, 0
