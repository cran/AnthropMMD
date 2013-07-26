\name{StartMMD}
\alias{StartMMD}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
The only function to use !
}
\description{
Contains or calls the whole GUI for the package.
}
\usage{
StartMMD()
}
\details{
For more information about this package and its possibility, see the full (separated) documentation written by the author (contact by email).
}
\value{
A MMD distance matrix : elements above the diagonal are MMD values (distances), elements below the diagonal are MMD standard deviations. 
}
\references{
Joel D. Irish. The mean measure of divergence : its utility in model-free and model-bound analyses relative to the mahalanobis d2 distance for nonmetric traits. American Journal of Human Biology, 2010.
}
\author{
Frederic Santos
}
\note{
This function also generates two files "effectifs_MMD.csv" and "prop_MMD.csv" in the folder chosen for the current R-session. This files contain the number of known individuals and proportion of "1" for each group and each variable.
}
\examples{
\dontrun{StartMMD() # the only command to know for all the package !}
}
