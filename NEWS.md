# AnthropMMD 4.1.0 (Release date: 2025-10-21)

* In the user-interface, the "significance" of MMDs are now decided using Sjovold's rule-of-thumb (the MMD value must be greater than twice its standard deviation). The p-values are not used for that purpose anymore, since their validity is still highly discussed in the literature, and relies on often unrealistic hypotheses.
* In the same spirit, p-values have now been totally removed from the package. `{AnthropMMD}` now only provides ratios MMD/sd(MMD), as in Bertsatos' Octave script (see Bertsatos & Chovalopoulou, 2016).

## Other minor changes
* More unit tests have been added, and some unit tests have been rewritten.
* A `codemeta.json` file has been added to provide a better structure for the package's metadata.

# AnthropMMD 4.0.3 (Release date: 2023-11-29)

## Other changes
* Fixed man page of package (version mismatch)
* New reference added in man pages

# AnthropMMD 4.0.2 (Release date: 2023-07-28)

## Other changes
* Fixed encoding of LICENSE file.

# AnthropMMD 4.0.1 (Release date: 2023-06-15)

## Bug fix
* The internal function `mmd_resample()` now handles edge cases where resampling problems could arise.
* Fixed formula for the computation of sample sizes in `mmd_resample()`.

# AnthropMMD 4.0.0 (Release date: 2023-03-06)

## Requirements
* `AnthropMMD` now depends on R >= 4.1.0.
* New dependencies on third-party R packages (`{dplyr}`, `{MASS}`, `{rlang}`) have been added.

## New features
* The function `mmd()` now has another argument, `correct` (which defaults to `TRUE`), which allows the user to specify whether the correction for small sample sizes should be applied.
* A bootstrap method described by [Fidalgo et al. (2022)](https://doi.org/10.1016/j.jas.2022.105545) is now available through the function `mmd_boot()`. It can be used from the command line interface only; its implemention in the graphical user interface is not planned (at least in a near future).

## Other changes
* The code of various functions (in particular `binary_to_table()`) has been refactored, and should now run faster.
* Updated package vignette.

# AnthropMMD 3.1.0 (Release date: 2020-07-17)

## Incompatible changes in the API
* A new generic function, `plot()`, is added. It replaces `plot_mmd()`. The old `plot_mmd()` is not exported anymore.

## New features
* p-values are now implemented in `mmd()`.

## Other changes
* Improved code readability.

# AnthropMMD 3.0.1 (Release date: 2019-07-16)

## Minor changes
* Fixed warning when the angular transformation is not specified in `mmd()` function.
* Fixed typos in package vignette and documentation files.

# AnthropMMD 3.0.0 (Release date: 2019-07-05)

## Change in dependencies
* Due to some new features implemented in `AnthropMMD`, the package now depends on R >= 3.5.0.
* `AnthropMMD` now imports `{plotrix}`.
* `AnthropMMD` now suggests `{covr}`, `{knitr}`, `{rmarkdown}` and `{testthat}`.

## Change of License
* `AnthropMMD` is now distributed under CeCILL 2.1 license (instead of GPL 3).

## User visible changes
* `AnthropMMD` can now be used by command lines, and not only as an R-shiny application. The main goal is to make `AnthropMMD` suitable for reproducible research.
* An example data file, `toyMMD`, is now available.
* A vignette is now available.
* Improved display of group labels on the MDS plots.
* Some small improvements in documentation files.

## Other changes
* A large majority of comments in the R source files have been translated from French into English.
* The reliability of future updates has been improved by implementing unit tests; and a CI pipeline has been set on the GitLab repo.
* The indentation style in `.R` and `.Rd` files now follows the R standards.

## Bug fixes
* The Shiny app does not crash anymore if the user specifies a wrong field separator.
* Fix for the computation of Spearman's measure of agreement for MDS, where distances were erroneously counted twice. Although the impact was generally negligible, this resulted in a small overestimation of the quality of agreement.

# AnthropMMD 2.5.3 (Release date: 2019-03-18)

* First release on GitLab
* Added LICENSE, NEWS and README files
* Change of GPL license (GPL3 instead of GPL2)
* Updated DESCRIPTION file
