\name{plot.anthropmmd_boot}
\alias{plot.anthropmmd_boot}

\title{
  Display a multidimensional scaling (MDS) plot using Fidalgo et al's
  (2022) bootstrap method for MMD
}

\description{
  This function plots a 2D MDS to represent the MMD dissimilarities
  among the groups compared, after a bootstrap resampling performed with
  \code{\link{mmd_boot}}.
}

\usage{
\method{plot}{anthropmmd_boot}(x, method = c("classical", "interval", "ratio", "ordinal"),
    level = 0.95, pch = 16, gof = FALSE, xlab = NA, ylab = NA,
    main = "MDS plot of original and bootstrapped samples", ...)
}

\arguments{
  \item{x}{An object of class \code{anthropmmd_boot}, produced by the
    function \code{\link{mmd_boot}}.}.
  \item{method}{Algorithm used for MDS computation; see \code{\link{plot.anthropmmd_result}}.}
  \item{level}{Numeric value between 0 and 1, confidence level for the
    contour lines displayed after the kernel density estimate.}
  \item{pch}{Passed to \code{plot}.}
  \item{gof}{Boolean; whether to display goodness of fit statistic on
    the plot.}
  \item{xlab}{Passed to \code{plot}.}
  \item{ylab}{Passed to \code{plot}.}
  \item{main}{Passed to \code{plot}.}
  \item{...}{Other arguments possibly passed to \code{plot}.}
}

\value{
  This function returns no value by itself, and only plots a MDS in a
  new device.
}

\details{
  In the current implementation, to stick to Fidalgo et al.'s (2022)
  protocol, this function does not provide as much freedom as
  \code{\link{plot.anthropmmd_result}} as concenrs MDS parameters and
  other analysis options.
}

\references{
  D. Fidalgo, V. Vesolowski and M. Hubbe (2022). Biological affinities
  of Brazilian pre-colonial coastal communities explored through
  boostrapped biodistances of dental non-metric traits. \emph{Journal of
  Archaeological Science} \bold{138}, 105545.
}

\seealso{
  \code{\link{start_mmd}}, \code{stats::cmdscale}
}

\examples{
\dontrun{
## Load and visualize a raw binary dataset:
data(toyMMD)
head(toyMMD)
## Compute MMD among bootstrapped samples:
resboot <- mmd_boot(
    data = toyMMD,
    B = 50, # number of bootstrap samples
    angular = "Anscombe",
    strategy = "excludeQNPT", # strategy for trait selection
    k = 10 # minimal number of observations required per trait
)
## MDS plot for bootstrapped samples:
plot(
    x = resboot,
    method = "interval", # algorithm used for MDS computation
    level = 0.95 # confidence level for the contour lines
)
}
}

\author{Frédéric Santos, \email{frederic.santos@u-bordeaux.fr}}
