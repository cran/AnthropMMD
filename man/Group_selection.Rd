\name{Group_selection}
\alias{Group_selection}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Internal function : tcltk code to display a window allowing to select the groups involved in the analysis.
}
\description{
Your dataset contains at least two populations, and probably (much) more. But you may want to exclude one or some groups before beginning the analysis. The window displayed by this code allows you to select which groups you want to retain for the effective MMD calculus.
}
\usage{
Group_selection(tab)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tab}{
A data matrix, whose the first column is a group indicator.
}
}
\value{
A list of the retained groups.
}
\author{
Frederic Santos
}

