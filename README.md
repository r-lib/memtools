
# memtools

<!-- badges: start -->
[![R-CMD-check](https://github.com/lionel-/memtools/workflows/R-CMD-check/badge.svg)](https://github.com/lionel-/memtools/actions)
<!-- badges: end -->


## Overview

memtools provides debugging tools to detect and solve memory leaks in R:

- Record __memory snapshots__ of the R heap.
- Compare snapshots to detect leaked objects with a large retained size.
- Inspect the __shortest paths__ between nodes (e.g. between a leaked node and its __dominator__).

See [`vignette("memtools")`](https://memtools.r-lib.org/articles/memtools.html) for a tutorial.


## Installation

Install memtools from github with:

```r
remotes::install_github(
  "r-lib/memtools",
  build_manual = TRUE,
  build_vignettes = TRUE
)
```
