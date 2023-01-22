  $ dune exec -- ../../bin/main.exe 0.rvg
      add t0, t1, t2
      add a0, a1, a2
      add t0, t1, t2
  $ dune exec -- ../../bin/main.exe 1-multiple-files/a.rvg 1-multiple-files/b.rvg
      addi zero, zero 0
  $ dune exec -- ../../bin/main.exe tokens 0.rvg
  {"kind": "function", "modifier": "defaultLibrary", "startInclusive": [0, 2], "endExclusive": [0, 7] }
  $ dune exec -- ../../bin/main.exe stdlib.rvg 2.rvg
  $ dune exec -- ../../bin/main.exe stdlib.rvg 3.rvg
  Expected lam of width 2: line 11, col 12 to line 11, col 42
  Assertion failed: line 2, col 20 to line 2, col 30
