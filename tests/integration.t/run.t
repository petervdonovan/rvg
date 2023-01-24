  $ dune exec -- ../../bin/main.exe 0.rvg
      add t0, t1, t2
      add a0, a1, a2
      add t0, t1, t2
  $ dune exec -- ../../bin/main.exe 1-multiple-files/a.rvg 1-multiple-files/b.rvg
      addi zero, zero, 0
  $ dune exec -- ../../bin/main.exe tokens 0.rvg
  {"kind": "function", "modifier": "defaultLibrary", "startInclusive": [0, 2], "endExclusive": [0, 7] }
  $ dune exec -- ../../bin/main.exe stdlib.rvg 2.rvg
  a ok
  b ok
  $ dune exec -- ../../bin/main.exe stdlib.rvg 3.rvg
  good 0
  good 1
  Expected lam of 2 parameters: line 11, col 12 to line 11, col 47
  Assertion failed: line 6, col 3 to line 6, col 12
  $ dune exec -- ../../bin/main.exe stdlib.rvg 4.rvg
  testing lam!
  executing identity
  Expected lam of 1 parameters: line 11, col 12 to line 11, col 47
  Assertion failed: line 6, col 3 to line 6, col 12
  $ dune exec -- ../../bin/main.exe stdlib.rvg 5.rvg
  line 16, col 7 to line 16, col 19: Unbound name: result_check
  $ dune exec -- ../../bin/main.exe stdlib.rvg 6.rvg
  pass
  7
  Expected a number: line 20, col 18 to line 20, col 45
  Assertion failed: line 6, col 3 to line 6, col 12
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
  Expected exactly 3 but got 4: line 4, col 17 to line 8, col 1
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
  $ dune exec -- ../../bin/main.exe ctrl.rvg stdlib.rvg ../../bin/main.exe 12.rvg
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
