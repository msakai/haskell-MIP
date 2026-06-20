# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

A Haskell monorepo of two Stack/Cabal packages (spun off from [toysolver](https://github.com/msakai/toysolver)) for Mixed Integer Programming:

- **`MIP/`** — core library: data model for MIP problems, LP/MPS file readers/writers, solver-output parsers, and drivers that shell out to external command-line solvers. Published on Hackage (BSD3).
- **`MIP-glpk/`** — a GLPK backend that links the GLPK C library in-process via FFI (GPL, separate license because it links GLPK).

## Build & test commands

Stack is the primary tool. The default `stack.yaml` pins `lts-24.45` (GHC 9.10); per-GHC configs `stack-ghc-X.Y.yaml` exist for CI across GHC 8.6–9.12.

```bash
stack build                                  # build all packages
stack test                                   # build + run all test suites
stack test MIP                               # one package's suite
stack --stack-yaml stack-ghc-9.12.yaml build # build under a specific GHC
stack haddock --no-haddock-deps              # generate docs
stack test --coverage                        # HPC coverage (CI uploads to Coveralls)
```

Run a single test case (suites use `tasty`, so filter with a pattern):

```bash
stack test MIP --ta '-p "cbc"'
```

### Solver-dependent tests are opt-in

By default the test suite skips every case that needs an external solver. Those cases are CPP-gated (`#ifdef TEST_CBC`, etc. in `MIP/test/Test/MIPSolver.hs`) and enabled by manual Cabal flags — and the corresponding executable must be on `PATH`:

```bash
stack test --flag MIP:TestCBC --flag MIP:TestGlpsol --flag MIP:TestLPSolve \
           --flag MIP:TestHiGHS --flag MIP:TestSCIP
```

Flags: `TestCBC`, `TestCPLEX`, `TestGlpsol`, `TestGurobiCl`, `TestHiGHS`, `TestLPSolve`, `TestPrintemps`, `TestSCIP`. The `WithZlib` flag (on by default) enables transparent `.gz` I/O; build with `--flag MIP:-WithZlib` to drop the `zlib` dependency.

### Building MIP-glpk

`MIP-glpk` needs the GLPK C library and headers present. On macOS (Homebrew) pass:

```bash
stack build --extra-include-dirs /opt/homebrew/include --extra-lib-dirs /opt/homebrew/lib
```

Note: `package.yaml` is the source of truth (hpack); the `*.cabal` files are generated. Edit `package.yaml`, not the `.cabal`.

## Architecture

### Core data model — `Numeric.Optimization.MIP.Base`

Everything centers on `Problem c` (parameterized over the numeric coefficient type, usually `Scientific`). Key types: `Expr` (a `Seq` of `Term c`, where `Term c = Term c [Var]` allows products → quadratic/polynomial), `ObjectiveFunction`, `Constraint`, `SOSConstraint`, `Domain = (VarType, Bounds)`, and `Solution`. A `Var` is a `newtype` over `InternedText` (the `intern` package) so equality/hashing are cheap; build them with `toVar`/`varExpr`.

`Status` (solver result status) has a defined partial order with a lattice `meet`; treat status comparisons through those helpers rather than ad-hoc equality. The `Eval` type class evaluates problem elements against an assignment under a `Tol` (tolerance); `Variables` collects the variable set of any element.

`Numeric.Optimization.MIP` is the umbrella module re-exporting `Base` plus file I/O. `readFile`/`writeFile` dispatch on extension (`.lp` → `LPFile`, `.mps` → `MPSFile`); `FileUtils.hs` layers optional gzip + encoding on top via `FileOptions`.

### File formats

`LPFile.hs` and `MPSFile.hs` are `megaparsec`-based parsers plus pretty-printers, configured by `FileOptions`. Solver-output formats live under `Solution/` (one module per solver: `CBC`, `CPLEX`, `GLPK`, `Gurobi`, `HiGHS`, `MIPLIB`, `Printemps`, `SCIP`) and produce a `Solution`.

### Solver abstraction

`Solver/Base.hs` defines the `IsSolver s m` class with `SolveOptions` (time limit, tolerance, stdout/stderr loggers, condensed-solution flag). Implementors only define `solve'`, which **may omit zero-valued variables**; the default `solve` adds those zeroes back unless `solveCondensedSolution` is set.

Two distinct solver styles:

1. **CLI drivers** (`Solver/CBC.hs`, `Glpsol`, `GurobiCl`, `HiGHS`, `LPSolve`, `Printemps`, `SCIP`, `CPLEX`) — the common pattern: write the problem to a temp LP/MPS file, run the external command via `Internal/ProcessUtil.runProcessWithOutputCallback` (streaming output to the loggers), then parse the solver's solution file with the matching `Solution/*` module. Each solver is a record (e.g. `CBC{cbcPath, cbcArgs}`) with a `def`/lowercase smart constructor (`cbc`) you tweak via record update. Maximization is typically handled by negating the objective before writing, then negating the objective value back. `Solver.hs` re-exports all CLI solvers.

2. **FFI backend** (`MIP-glpk`'s `Solver/GLPK.hs`) — solves in-process through `glpk-headers` FFI bindings, no temp files. Note there are *two* GLPK paths: `Glpsol` (CLI, in `MIP`) vs `GLPK` (linked library, in `MIP-glpk`).

When adding a CLI solver: add `Solver/<Name>.hs` (instance of `IsSolver`, following the temp-file pattern), a `Solution/<Name>.hs` parser if the output format is new, re-export from `Solver.hs`, and add a `Test<Name>` flag + CPP-gated cases in `MIP/test/Test/MIPSolver.hs`.
