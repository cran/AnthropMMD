\name{Mat_MMD}
\alias{Mat_MMD}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Internal function : effective calculus of the MMD Matrix.
}
\description{
Internal function : effective calculus of the MMD Matrix.
}
\usage{
Mat_MMD(Mat_eff, Mat_prop, formule, corrFT)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Mat_eff}{
A (nxp) matrix , where n was the number of groups in the original data file, and p the number of (binary) variables. This matrix contains the number of known individuals in each group and for each variable.
}
  \item{Mat_prop}{
A (nxp) matrix , where n was the number of groups in the original data file, and p the number of (binary) variables. This matrix contains the proportion of presence (i.e. of "1" values) in each group and for each variable.
}
  \item{formule}{
"Freeman-Tukey" or "Anscombe" : the choice of MMD formula.
}
  \item{corrFT}{
Boolean indicating whether the Freeman-Tukey correction for small samples has to be applied.
}
}
\author{
Frederic Santos
}

