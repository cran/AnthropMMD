binary_to_table <- function(data, relative = FALSE) {
### data: matrix of binary data, + one column for the group indicator
### relative: boolean, indicates whether the last rows of the table contain
###           absolute frequencies (i.e., number of individuals with each trait)
###           or relative frequencies (i.e., proportions).
### Converts this data frame into a table of group sample sizes and frequencies.

    ## 1. Set some constants:
    colnames(data)[1] <- "Group"
    groupnames <- levels(data$Group)

    ## 2. Compute the observed sample size per group and trait
    mateff <- aggregate(
        x = data[, -1],
        by = list(data$Group),
        FUN = function(x) sum(!is.na(x))
    )

    ## 3. Compute the observed frequency per group and trait
    matfreq <- aggregate(
        x = data[, -1],
        by = list(data$Group),
        FUN = sum,
        na.rm = TRUE
    )

    ## 4. Combine results in a matrix:
    mat <- as.matrix(rbind(mateff[, -1], matfreq[, -1]))
    rownames(mat) <- c(paste("N", groupnames, sep = "_"),
                       paste("Freq", groupnames, sep = "_"))

    ###########################
    ## 4. Return the results ##
    ###########################
    if (relative == FALSE) {
        return(mat)
    } else {
        return(table_relfreq(mat))
    }
}
