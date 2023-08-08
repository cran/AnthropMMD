mmd <- function(data, angular = c("Anscombe", "Freeman"), correct = TRUE,
                all.results = TRUE) {
### data: table of group sample sizes and frequencies,
###       such as returned by the function table_relfreq
### angular: choice of a formula for angular transformation
### correct: boolean; whether to apply the correction for small samples
### all.results: boolean; if FALSE, only the symmetrical MMD matrix is computed

    angular <- match.arg(angular) # avoid a warning if no arg is given

    ##################################################
    ## 1. Define some useful constants and matrices ##
    ##################################################
    ngroups <- nrow(data) / 2
    ntraits <- ncol(data)
    ## portion of the data corresponding to the sample sizes:
    matsize <- data[1:ngroups, ]
    groupnames <- rownames(matsize)
    ## portion of the data corresponding to the relative frequencies:
    matfreq <- data[-c(1:ngroups), ]
    ## angular transformation of relative frequencies:
    for (j in 1:ntraits) {
        matfreq[, j] <- mapply(
            n = matsize[, j],
            p = matfreq[, j],
            FUN = theta,
            MoreArgs = list(choice = angular)
        )
    }

    #######################################
    ## 2. Initialize some empty matrices ##
    #######################################
    ## MMD matrix (symmetrical):
    mmd_sym <- matrix(NA, nrow = ngroups, ncol = ngroups)
    ## the rows and columns of mmd_sym are labeled according to group names:
    colnames(mmd_sym) <- substr(groupnames, 3, nchar(groupnames))
    rownames(mmd_sym) <- colnames(mmd_sym)
    ## Other matrices:
    pval_matrix <- sd_matrix <- signif_matrix <- mmd_sym

    #############################
    ## 3. Fill in the matrices ##
    #############################
    for (i in 1:ngroups) {
        for (j in 1:ngroups) { # For each pair of groups (i, j)...
            mmd_vect <- sd_vect <- rep(NA, ntraits)
            sum_pval <- 0
            for (k in 1:ntraits) { # and for each trait k,
                ## Compute the measure of divergence on trait k:
                mmd_vect[k] <- compute_md(nA = matsize[i, k],
                                          pA = matfreq[i, k],
                                          nB = matsize[j, k],
                                          pB = matfreq[j, k],
                                          correct = correct)
                if (all.results) {
                    ## Compute the SD for this trait:
                    sd_vect[k] <- sd_mmd(nA = matsize[i, k], nB = matsize[j, k])
                    ## Intermediate result for computing the p-value:
                    sum_pval <- sum_pval + ((matfreq[i, k] - matfreq[j, k])^2 / (1 / (matsize[i, k] + 0.5) + 1 / (matsize[j, k] + 0.5)))
                }
            }
            ## The MMD is the mean of those MD values:
            mmd_sym[i, j] <- max(mean(mmd_vect), 0) # replace by 0 if negative
            if (all.results & (i != j)) { # avoid NaN when comparing a group to itself
                ## The associated SD is as follows:
                sd_matrix[i, j] <- sqrt(2 * sum(sd_vect)) / ntraits
                ## The associated p-value:
                pval_matrix[i, j] <- pchisq(sum_pval, df = ntraits,
                                            lower.tail = FALSE)
                ## And finally the significance ('*' or 'NS'):
                signif_matrix[i, j] <- ifelse(pval_matrix[i, j] < 0.05, "*", "NS")
            }
        }
    }
    diag(mmd_sym) <- 0 # distance between a group and itself must be null

    #################################################
    ## 4. Prepare the matrices for elegant display ##
    #################################################
    mmd_matrix <- mix_matrices(m = mmd_sym, n = sd_matrix, diag_value = 0)
    if (all.results) {
        pval_matrix <- mix_matrices(m = mmd_sym, n = pval_matrix, diag_value = NA)
        signif_matrix <- mix_matrices(m = round(mmd_sym, 3), n = signif_matrix,
                                      diag_value = NA)
    }

    ###########################
    ## 5. Return the results ##
    ###########################
    list_results <- list(MMDMatrix = round(mmd_matrix, 6),
                         MMDSym = round(mmd_sym, 6),
                         MMDSignif = signif_matrix,
                         MMDpval = round(pval_matrix, 4))
    class(list_results) <- "anthropmmd_result"
    return(list_results)
}
