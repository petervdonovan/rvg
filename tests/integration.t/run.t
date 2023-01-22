  $ dune exec -- ../../bin/main.exe 0.rvg
      add t0, t1, t2
      add a0, a1, a2
      add t0, t1, t2
  $ dune exec -- ../../bin/main.exe 1-multiple-files/a.rvg 1-multiple-files/b.rvg
      addi zero, zero 0
  $ dune exec -- ../../bin/main.exe tokens 0.rvg
  {"kind": "function", "modifier": "defaultLibrary", "startInclusive": [0, 2], "endExclusive": [0, 7] }
  $ dune exec -- ../../bin/main.exe stdlib.rvg 2.rvg
  a ok
  b ok
  $ dune exec -- ../../bin/main.exe stdlib.rvg 3.rvg
  good 0
  good 1
  Expected lam of 2 parameters: line 11, col 12 to line 11, col 47
  Assertion failed: line 6, col 20 to line 6, col 30
  $ dune exec -- ../../bin/main.exe stdlib.rvg 4.rvg
  testing lam!
  executing identity
  Expected lam of 1 parameters: line 11, col 12 to line 11, col 47
  Assertion failed: line 7, col 5 to line 7, col 41
  $ dune exec -- ../../bin/main.exe stdlib.rvg 5.rvg
  curried successfully
  first thing to print
  second thing to print
