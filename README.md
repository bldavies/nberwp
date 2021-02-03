# nberwp

This package contains information about [NBER](https://www.nber.org) working papers published between June 1973 and December 2020.
The package provides the following two data frames.

* `papers`: working paper attributes.
* `authors`: paper-author correspondences.

I disambiguate author names by cross-referencing against RePEc IDs and common co-authorships, and via fuzzy matching.
Please notify me of any errors by [adding an issue](https://github.com/bldavies/nberwp/issues) or [submitting a pull request](https://github.com/bldavies/nberwp/pulls).

See [here](https://www.nber.org/policies.html) for information about the NBER working paper catalogue.

## Installation

nberwp can be installed via [remotes](https://github.com/r-lib/remotes):

```r
library(remotes)
install_github('bldavies/nberwp')
```

## License

CC0
