## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
options(digits = 3)

## ----include=FALSE------------------------------------------------------------
library(AnthropMMD)

## ----eval=FALSE, include=TRUE-------------------------------------------------
# start_mmd()

## ----rows.print=8-------------------------------------------------------------
data(toyMMD)
head(toyMMD)

## -----------------------------------------------------------------------------
str(toyMMD)

## -----------------------------------------------------------------------------
tab <- binary_to_table(toyMMD, relative = TRUE)
print(tab)

## -----------------------------------------------------------------------------
data(absolute_freqs)
print(absolute_freqs)

## -----------------------------------------------------------------------------
tab <- table_relfreq(absolute_freqs)
print(tab)

## -----------------------------------------------------------------------------
tab_selected <- select_traits(tab, k = 10, strategy = "keepFisher")
print(tab_selected$filtered)

## -----------------------------------------------------------------------------
mmd.result <- mmd(tab_selected$filtered, angular = "Anscombe")
mmd.result

## -----------------------------------------------------------------------------
par(cex = 0.8)
plot(x = mmd.result, method = "interval", 
     gof = TRUE, axes = TRUE, xlim = c(-1.2, 0.75))

## -----------------------------------------------------------------------------
library(cluster)
par(cex = 0.8)
plot(agnes(mmd.result$MMDSym), which.plots = 2, main = "Dendrogram of MMD dissimilarities")

## -----------------------------------------------------------------------------
## Load the example data once again:
data(toyMMD)
## Compute MMD among bootstrapped samples:
set.seed(2023) # set seed for reproducibility
resboot <- mmd_boot(
    data = toyMMD,
    B = 50, # number of bootstrap samples
    angular = "Anscombe",
    strategy = "keepFisher", # strategy for trait selection
    k = 10 # minimal number of observations required per trait
)

## -----------------------------------------------------------------------------
## MDS plot for bootstrapped samples:
plot(
    x = resboot,
    method = "interval", # algorithm used for MDS computation
    level = 0.95, # confidence level for the contour lines
    gof = TRUE # display goodness of fit statistic
)

