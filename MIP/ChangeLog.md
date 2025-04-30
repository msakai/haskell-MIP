# Changelog for MIP

## Unreleased changes

* Add `Numeric.Optimization.MIP.Solution.MIPLIB` module for reading/writing MIPLIB solution format
* Add `Domain` type
* Add utility functions: `isInDomain`, `isIntegral`, `isInBounds`, and `constrBounds`

## 0.2.0.0 (2025-02-03)

* `Problem` type
  * Merge `varType` and `varBounds` fields into `varDomains` field 
  * Rename `varType` into `varTypes` and deprecate `varType`
  * Represent `Expr c` as `Seq (Term c)` instead of `[Term c]`
  * Change `Var` into `newtype`
* Add `Tol` data type and `Eval` type class
* Add `continuousVariables :: Problem c -> Set Var`
* Add `binaryVariables :: Problem c -> Set Var`
* File I/O
  * Allow CRLF in parsers
  * Fix to print the right-hand side of indicators as integers when possible in LP files
  * Accept numbers other than 0 or 1 as the right-hand side of indicators in LP files
  * Add `optMPSWriteObjSense` to `FileOptions`
  * Add `optMPSWriteObjName` to `FileOptions`
* Solver supports
  * Add omitted zeroes to solutions (thanks to @dpvanbalen)
  * Allow passing arguments solver executables
  * Support printemps solver
  * Support HiGHS solver
  * Add `solveTol` field to `SolveOptions`
* Dependencies
  * Require `base >=4.12` (i.e. GHC `>=8.6`)
  * Require `megaparsec >=7`
  * Support `mtl-2.3`

## 0.1.1.0

* re-export the `Default` class from `Numeric.Optimization.MIP`.
* fix to work with recent versions of CBC
* generalizethe  type of `parseLPString` and `parseMPSString`

## 0.1.0.0

* Separated from toysolver package
  https://github.com/msakai/toysolver/tree/7096038ece0a5f860a951567689ef9a03ac0355d
