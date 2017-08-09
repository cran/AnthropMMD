\name{StartMMD}
\alias{StartMMD}
\title{
The only function to use !
}
\description{
Launches a Shiny Graphical User Interface to work out Mean Measures of Divergence.
}
\usage{
StartMMD()
}
\details{
The GUI of AnthropMMD is completely independent and autonomous. Reading the data file and specifying the parameters of the analysis are done through the interface. Once the dataset is loaded, the output reacts dynamically to any change in the analysis settings. 

Note that the R console is not available when the GUI is active. To exit to GUI, type Echap (on MS Windows systems) or Ctrl+C (on Linux systems) in the R console.

\enumerate{
\item AnthropMMD allows to read .CSV or .TXT data sets, but does not support .ODS or .XLS(X) files. Two types of files are accepted:
\itemize{
\item A 'Raw binary dataset' (one row per individual, one column per variable). The first column must be a factor indicating the group of each individual, and the other columns are binary data for the traits studied, where 1 indicates the presence of a trait in an individual, and 0 its absence. Row names are optional for this type of file. An example of valid datafile can be found here: \url{http://tinyurl.com/RawBinaryMMD}
\item A 'Table of n's and absolute frequencies for each group', i.e. a dataset of sample sizes and absolute frequencies. This type of dataset has \eqn{2 \times K}{2*K} rows (\eqn{K}{K} being the number of groups compared) and \eqn{p}{p} columns (\eqn{p}{p} being the number of traits studied). The first \eqn{K}{K} lines must be the group n's for each trait, and the last \eqn{K}{K} lines are absolute frequencies for each trait (i.e. the number of times the trait is present). Row names are mandatory for this type of file. The first \eqn{K}{K} rows must be labelled with names beginning with "N_", such as: N_GroupA, N_GroupB, ..., N_GroupK. The last \eqn{K}{K} rows should be labelled with names beginning with "Freq_", such as: Freq_GroupA, ..., Freq_GroupK. An example of valid datafile can be found here: \url{http://tinyurl.com/TableFreqMMD}
}
For both data types, column names are strongly recommended for better interpretability of the results. 

\item The analysis could be done with all groups/populations in the datafile (default), or some of them can be excluded (for example if their very weak sample sizes makes tend to eliminate too much variables from the analysis; more details are given below).

\item Various parameters can be set:
\itemize{
\item One can choose between Anscombe or Freeman-Tukey formula for angular transformation (cf. Harris and Sjovold 2004; Irish 2010).
\item 'Only retain the traits with this minimal number of individuals per group': the traits with fewer indivuals in at least one active group will not be considered in the analysis.
\item 'Exclusion strategy': a good selection of traits is crucial when using MMD (cf. Harris and Sjovold 2004 for a complete explanation), and the user should probably choose to exclude some traits with very fewx variability accross groups. 'Exclude nonpolymorphic traits' allows to remove all the traits showing no variability at all, i.e. with the same value (always 0, or always 1) for all the individuals. 'Exclude quasinonpolymorphic traits' includes the previous option, and also removes the traits whose variability is only due to a single individual: for example, a trait with only one positive observation in the whole dataset. Finally, 'Exclude traits with negative overall MD' is a simple way of removing the traits with quite similar frequencies accross groups (the 'overall MD' is defined as the sum of the variable's measures of divergence over all pairs of groups). These three options are designed to avoid negative MMD values, which would prevent the MDS plot to be displayed.
\item Some groups/populations can be manually excluded from the analysis. This may be useful if very few individuals belonging to a given population could be recorded for the variables retained by the criteria described above.
}

\item A MDS plot, done using MMD dissimilarities as an input, is given in the third tab. Please note that various reasons can prevent the MDS plot from being displayed: if there are only two active groups/populations; if there is only one postive eigenvalue; or if there are negative values in MMD matrix.
}
}
\value{
The function returns no value by itself, but all results can be indivudally downloaded through the graphical interface.
}
\references{
Sjovold, T. (1977). Non-metrical Divergence Between Skeletal Populations. Ossa, \bold{4}, Supp. 1.

Harris, E. F. and Sjovold, T. (2004). Calculation of Smith's Mean Measure of Divergence for Intergroup Comparisons Using Nonmetric Data. Dental Anthropology, \bold{17}(3), 83--93.

Irish, J. (2010). The Mean Measure of Divergence: Its Utility in Model-Free and Model-Bound Analyses Relative to the Mahalanobis D2 Distance for Nonmetric Traits. American Journal of Human Biology, \bold{22}, 378--395.
}
\author{
Frederic Santos

Maintainer: Frederic Santos <frederic.santos@u-bordeaux.fr>
}
\examples{
\dontrun{ StartMMD()}
}
