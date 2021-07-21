# nberwp

[![CRAN](https://www.r-pkg.org/badges/version/nberwp)](https://cran.r-project.org/package=nberwp)

This package contains information about [NBER](https://www.nber.org) working papers published between June 1973 and June 2021.

## Installation

nberwp is [available on CRAN](https://cran.r-project.org/package=nberwp):

```r
install.packages('nberwp')
```

The development version can be installed from GitHub via [remotes](https://github.com/r-lib/remotes):

```r
remotes::install_github('bldavies/nberwp')
```

## Description

nberwp provides the following five tables.

* `papers`: working paper attributes.
* `authors`: author attributes.
* `programs`: program attibutes.
* `paper_authors`: paper-author correspondences.
* `paper_programs`: paper-program correspondences.

I construct these tables from raw metadata published [here](https://data.nber.org/nber-wp-logs/) and [here](https://www2.nber.org/RePEc/nbr/).
I clean these metadata by fixing typos, removing non-ASCII characters, and disambiguating author names.
My disambiguation approach involves cross-referencing against NBER user names, RePEc IDs, common co-authorships, common [program](https://www.nber.org/programs-projects/programs-working-groups) associations, name edit distances, and information gleaned (manually) from authors' personal and academic webpages.

See [here](https://www.nber.org/policies.html) for information about the NBER working paper catalogue.
The catalogue comprises three series: historical, technical, and general.
Working papers in these series have numbers prefixed by "h", "t", and "w".

## Contributing

Please notify me of any errors by [adding an issue](https://github.com/bldavies/nberwp/issues) or [submitting a pull request](https://github.com/bldavies/nberwp/pulls).

## License

CC0
