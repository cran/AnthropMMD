mmd_resample <- function(dat, B = 100) {
### dat: dataset *in raw binary data* format
### B: numeric value; number of bootstrap replicates

    ## Define some useful constants:
    ntraits <- ncol(dat) - 1 # (the 1st column is the group indicator)
    colnames(dat)[1] <- "Group"
    ngroups <- nlevels(dat$Group)
    groupnames <- levels(dat$Group)
    sampsizes <- as.numeric(table(dat$Group)) # ns by group

    ###################################
    ## 1. Initialize empty matrices ###
    ###################################
    matfreq <- matrix(NA,
                      nrow = ngroups * (B + 1),
                      ncol = ntraits)
    colnames(matfreq) <- colnames(dat[, -1])
    rownames(matfreq) <- c(
        groupnames,
        paste0(rep(groupnames, times = B),
               "_boot",
               rep(1:B, each = ngroups))
    )
    matsizes <- matfreq

    ##############################
    ## 2. Fill in the matrices ###
    ##############################
    ## One row per bootstrap replicate,
    ## one column per trait.
    ## 2.1. For the original data:
    origtable <- binary_to_table(dat, relative = TRUE)
    matfreq[1:ngroups, ] <- tail(origtable, ngroups)
    matsizes[1:ngroups, ] <- head(origtable, ngroups)

    ## 2.2. For the bootstrap samples:
    for (b in 1:B) {
        starsample <-  dat |>
            group_by(.data$Group) |> ## .data prefix avoids a NOTE in R CMD check
            sample_n(n(), replace = TRUE) |>
            as.data.frame()
        startable <- binary_to_table(starsample, relative = TRUE)
        matfreq[(b*ngroups+1):((b+1)*ngroups), ] <- tail(startable, ngroups)
        matsizes[(b*ngroups+1):((b+1)*ngroups), ] <- head(startable, ngroups)
    }

    ###################
    ## Return result ##
    ###################
    rownames(matsizes) <- paste0("N_", rownames(matfreq))
    rownames(matfreq) <- paste0("Freq_", rownames(matfreq))
    return(list(matfreq = matfreq, matsizes = matsizes))
}

mmd_boot <- function(data, angular = c("Anscombe", "Freeman"), B = 100, ...) {
### data: dataset *in raw binary data* format
### B: numeric value; number of bootstrap replicates
### ...: arguments for traits selection, passed to select_traits()

    angular <- match.arg(angular)

    ########################
    ## 1. Trait selection ##
    ########################
    selected_traits <- data |>
        binary_to_table(relative = TRUE) |>
        select_traits(...) |>
        getElement(name = "filtered") |>
        colnames()
    if (is.null(selected_traits) | length(selected_traits) < 2) {
        stop("Not enough traits available -- change strategy of traits selection.")
    }
    data <- data.frame(
        Group = data[, 1],
        data[, selected_traits]
    )

    ###############################
    ## 2. Resample in each group ##
    ###############################
    resamp <- mmd_resample(data, B = B)
    if (any(resamp$matsizes == 0)) {
        warning("The resampling process resulted in empty groups for some traits. Try to choose a higher value of k in mmd_boot().")
    }

    ###################
    ## 3. MMD matrix ##
    ###################
    mmdmat <- mmd(
        data = rbind(resamp$matsizes, resamp$matfreq),
        angular = angular,
        correct = FALSE,
        all.results = FALSE
    )$MMDSym

    ######################
    ## 4. Return result ##
    ######################
    class(mmdmat) <- "anthropmmd_boot"
    return(mmdmat)
}
