\name{Prepa_MMD}
\alias{Prepa_MMD}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Internal function : prepares the data set for MMD analysis according to thechoices previously specified by the user.
}
\description{
Internal function : prepares the data set for MMD analysis according to thechoices previously specified by the user (groups and variables which have to be excluded, etc.).
}
\usage{
Prepa_MMD(tab, type = "raw_data", k = 10, all_vars = FALSE, idiosync = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tab}{
The dataset.
}
  \item{type}{
"raw_data" or "summarized_data", depending on the choice previously specified by the user.
}
  \item{k}{
Minimal required number of individuals per group.
}
  \item{all_vars}{
Boolean : if FALSE, the variables which are always 0 or always 1 are excluded.
}
  \item{idiosync}{
Boolean : if FALSE, the variables which are always 0 or always 1 *excepted on one individual* are excluded.
}
}
\author{
Frederic Santos
}
