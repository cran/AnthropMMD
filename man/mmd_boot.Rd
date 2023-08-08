\name{mmd_boot}
\alias{mmd_boot}
\title{
  Implementation of Fidalgo et al.'s (2022) method of bootstrap for the
  Mean Measure of Divergence
}

\description{
  Compute a matrix of MMD dissimilarities among bootstrapped samples of
  the original groups. The input data must be a \dQuote{raw binary
  dataset}.
}

\usage{
mmd_boot(data, angular = c("Anscombe", "Freeman"), B = 100, ...)
}

\arguments{
  \item{data}{A \dQuote{raw binary dataset}, as defined in the man page
    of \code{\link{start_mmd}}.}
  \item{angular}{Choice of a formula for angular transformation: either
    Anscombe or Freeman-Tukey transformation.}
  \item{B}{Numeric value: number of bootstrap samples.}
  \item{...}{Arguments for traits selection, passed to
  \code{\link{select_traits}}.}
}

\value{
  A symmetrical dissimilarity matrix of MMD values among original groups
  and bootstrapped samples. This matrix is an R object of class
  \code{anthropmmd_boot}.
}

\details{
  This function sticks very close to Fidalgo et al's (2022)
  implementation. In particular, no correction for small sample sizes is
  applied in the MMD formula; see Fidalgo et al's (2021) for the
  rationale.

  Note that \bold{only} a \dQuote{raw binary dataset} is allowed as
  input, since the resampling cannot be performed properly from a table
  of counts and frequencies.

  To get a MDS plot of the dissimilarity matrix obtained with this
  function, see \code{\link{plot.anthropmmd_boot}}.
}

\references{
  D. Fidalgo, M. Hubbe and V. Vesolowski (2021). Population history of
  Brazilian south and southeast shellmound builders inferred through
  dental morphology. \emph{American Journal of Physical Anthropology}
  \bold{176}(2), 192-207.

  D. Fidalgo, V. Vesolowski and M. Hubbe (2022). Biological affinities
  of Brazilian pre-colonial coastal communities explored through
  boostrapped biodistances of dental non-metric traits. \emph{Journal of
    Archaeological Science} \bold{138}, 105545.
}

\seealso{
  \code{\link{plot.anthropmmd_boot}}
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
## View part of MMD matrix among bootstrapped samples:
dim(resboot)
print(resboot[1:15, 1:15])
}
}
\author{Frédéric Santos, \email{frederic.santos@u-bordeaux.fr}}
\concept{MMD}
\concept{mean measure of divergence}
