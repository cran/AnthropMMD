\name{StartMMD}
\alias{StartMMD}
\title{
The only function to use !
}
\description{
Launches a Graphical User Interface to work out Mean Measures of Divergence.
}
\usage{
StartMMD()
}
\details{
The GUI of AnthropMMD is completely independent and autonomous. Reading the data file, specifying the parameters of the analysis, and selecting the desired outputs, are done only through the interface. 

AnthropMMD allows to read .CSV or .TXT datasets, but does not support .ODS or .XLS(X) files. They can be :
- a binary dataset (one row per individual, p+1 columns). The first column must be a factor indicating the group of each individual, and the following p columns are binary data for the p traits studied, where 1 indicates the presence of a trait in an individual, and 0 its absence. To see an example, download the following dataset : https://tinyurl.com/DataMMD2
- a 'summarized dataset', in fact a dataset of group frequencies and sample sizes. This type of dataset has 2*K rows (K being the number of groups compared) and p columns (p being the number of traits studied). The first K lines must be the group n's for each trait, and the last K lines are group frequencies for each trait. To see an example, download the following dataset : https://tinyurl.com/DataMMD1

For more information about this package, please contact the author by email to get a full PDF documentation.
}
\value{
The user can specify the desired outputs using the graphical user interface, but some files are always written in the working directory of the R process :
- in accordance with a widespread use in publications of physical anthropology, a matrix containing in its upper triangular part MMD values, and in its lower triangular part the corresponding standard deviations, is recorded in the file Resultats_AnthropMMD_MmdValuesUp_SdDown.csv.
- the file Results_AnthropMMD_MmdValuesSym.csv is a symmetrical matrix containing the MMD values between each pair of groups.
- the file Results_AnthropMMD_SignifMmd.csv is a matrix where significant MMD values are indicating by stars (*), significance being determined at the 0.05 level. 

}
\references{
Sjovold, T. (1977). Non-metrical Divergence Between Skeletal Populations. Ossa, 4, Supp. 1.

Harris, E. F., Sjovold, T. (2004). Calculation of Smith's Mean Measure of Divergence for Intergroup Comparisons Using Nonmetric Data. Dental Anthropology, 17(3), 83-93.
}
\author{
Frederic Santos

Maintainer: Frederic Santos <frederic.santos@u-bordeaux.fr>
}
\examples{
# StartMMD() # the only command to know for all the package !
}
