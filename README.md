# rplexos

[![Build status](https://ci.appveyor.com/api/projects/status/gg78nxem0d1vy606/branch/master?svg=true)](https://ci.appveyor.com/project/eibanez/rplexos/branch/master)

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
