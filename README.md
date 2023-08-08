AnthropMMD
==========
[![CRAN Version](http://www.r-pkg.org/badges/version/AnthropMMD)](https://cran.r-project.org/package=AnthropMMD)
[![pipeline status](https://gitlab.com/f-santos/anthropmmd/badges/master/pipeline.svg)](https://gitlab.com/f-santos/anthropmmd/-/commits/master)
[![coverage report](https://gitlab.com/f-santos/anthropmmd/badges/master/coverage.svg)](https://gitlab.com/f-santos/anthropmmd/-/commits/master)
[![CRAN Downloads](http://cranlogs.r-pkg.org/badges/AnthropMMD)](https://cran.r-project.org/package=AnthropMMD)

`AnthropMMD` is an R package for an easy calculation of the mean measure of divergence (MMD). It offers both a graphical user interface (for scientists who are not R programmers), and a set of functions which can be used through the command line interface (for reproducible research or scientists who are already familiar with R).

For the technical and scientific aspects, the package is better described in its [vignette](https://cran.r-project.org/package=AnthropMMD/vignettes/intro_AnthropMMD.html).

Feature requests or bug reports are welcome.

## Installation of AnthropMMD from CRAN (recommended)

The latest stable version of AnthropMMD is available on CRAN, and can be installed by typing the following command line into the R console:

```r
install.packages("AnthropMMD", dep = TRUE)
```

## Installation of the R package AnthropMMD from GitLab

Alternatively, the latest development (possibly unstable) version can be installed from GitLab.

### Install prerequisites

1. Make sure that [Git](https://git-scm.com/) and a [recent version of R](https://cran.r-project.org/) (newer than 4.1.0) are installed.

2. Install the R package `remotes` by typing the following command line into the R console:

   ```r
   install.packages("remotes")
   ```

3. Install build environment:
    * **Linux**: no additional operation required.
    * **OSX**: install [XCODE](https://developer.apple.com/xcode/).
    * **Windows**: install the latest version of [Rtools](https://cran.r-project.org/bin/windows/Rtools/).

### Install AnthropMMD

Run the following command in R:

```r
remotes::install_git('https://gitlab.com/f-santos/anthropmmd.git',
                     build_vignettes = TRUE)
```

## Package vignette

The package vignette can also be consulted locally by running the following command into the R console:

```r
vignette(package = "AnthropMMD", topic = "intro_AnthropMMD")
```
	
## Running AnthropMMD

To start the graphical interface, run the following commands into the R console:

```r
library(AnthropMMD)
start_mmd()
```

## Citing AnthropMMD

To cite the package in a scientific article, citation information can be found by typing:

```r
citation("AnthropMMD")
```

into the R console.
