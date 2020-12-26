# MIP-glpk

[![Hackage](https://img.shields.io/hackage/v/MIP.svg)](https://hackage.haskell.org/package/MIP-glpk)
[![Hackage Deps](https://img.shields.io/hackage-deps/v/MIP.svg)](https://packdeps.haskellers.com/feed?needle=MIP-glpk)
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

This provides [GLPK](https://www.gnu.org/software/glpk/) backend for `MIP` package.

`MIP` package already have the ability to invoke GLPK's command-line solver `glpsol`,
but this package allows using GLPK as a library with smaller overhead.
