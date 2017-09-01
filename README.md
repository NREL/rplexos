# rplexos

[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/NREL/rplexos?branch=master&svg=true)](https://ci.appveyor.com/project/NREL/rplexos)
[![Coverage status](https://codecov.io/gh/NREL/rplexos/branch/master/graph/badge.svg)](https://codecov.io/github/NREL/rplexos?branch=master)

`rplexos` is an R package developed to read and analyze PLEXOS solutions. It currently supports the
conversion of PLEXOS solution files into SQLite databases and functions to query those databases.

It can be installed from [CRAN](https://cran.r-project.org/package=rplexos):

```
install.packages("rplexos")
```

The "Getting started" vignette presents the preferred workflow to process PLEXOS solutions with this package.

```
library(rplexos)
vignette("rplexos")
```
