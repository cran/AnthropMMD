## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
options(digits = 3)

## ----include=FALSE------------------------------------------------------------
library(AnthropMMD)

## ----eval=FALSE, include=TRUE-------------------------------------------------
#  start_mmd()

## ----rows.print=8-------------------------------------------------------------
data(toyMMD)
head(toyMMD)

## -----------------------------------------------------------------------------
str(toyMMD)

## -----------------------------------------------------------------------------
tab <- binary_to_table(toyMMD, relative = TRUE)
tab

## -----------------------------------------------------------------------------
data(absolute_freqs)
print(absolute_freqs)

## -----------------------------------------------------------------------------
tab <- table_relfreq(absolute_freqs)
print(tab)

## -----------------------------------------------------------------------------
tab_selected <- select_traits(tab, k = 10, strategy = "keepFisher")
tab_selected$filtered

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

