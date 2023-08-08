compute_mds <- function(data, dim = 2, gof = FALSE,
                        method = c("classical", "interval", "ratio", "ordinal")) {
### Helper function that only *computes* the mds solution.

    ## Will be redefined below iff gof=TRUE:
    legend_gof <- gof_value <- rho_value <- NULL

    ## Compute MDS solution:
    if (method == "classical") {
        ## Option A. Classical metric MDS
        res_mds <- cmdscale(data, k = dim, eig = TRUE) # *list* of MDS results
        coor <- res_mds$points # MDS coordinates
        var_by_dim <- apply(coor, MARGIN = 2, FUN = sd) # sd along each axis
        legend_plot <- "Classical multidimensional scaling of MMD values"
        if (gof == TRUE) {
            legend_gof <- "Eigenvalue-based GoF="
            gof_value <- round(res_mds$GOF[1], 3)
            rho_value <- mds_rho(mmd = data, coor = coor)
        }
    } else {
        ## Option B: MDS via SMACOF package
        res_mds <- smacof::smacofSym(as.dist(data),
                                     type = method) # *list* of MDS results
        coor <- res_mds$conf # MDS coordinates
        var_by_dim <- apply(coor, MARGIN = 2, FUN = sd) # sd along each axis
        legend_plot <- paste0("Multidimensional scaling of MMD values (",
                             method, " type)")
        if (gof == TRUE) {
            legend_gof <- "Stress="
            gof_value <- round(res_mds$stress, 3)
            rho_value <- mds_rho(mmd = data, coor = coor)
        }
    }

    ## Return result:
    return(list(res_mds = res_mds, coor = coor, var_by_dim = var_by_dim,
                legend_plot = legend_plot, legend_gof = legend_gof,
                gof_value = gof_value, rho_value = rho_value))
}

plot_mds2d <- function(data, coor, axes, legend_plot, asp_value, xlim,
                       gof, legend_gof, gof_value, rho_value) {
### data: original data submitted to plot.anthropmmd_result()
### coor: mds coordinates

    if (ncol(coor) >= 2 && any(data > 0)) { # OK, the plot can be displayed
            plot(x = coor[, 1], y = coor[, 2], pch = 16, xlab = "", ylab = "",
                 axes = axes, main = legend_plot, asp = asp_value, xlim = xlim,
                 ylim = c(1.1 * min(coor[, 2]), 1.15 * max(coor[, 2])))
            plotrix::thigmophobe.labels(x = coor[, 1], y = coor[, 2],
                                        labels = rownames(coor))

            if (gof == TRUE) { # if the user wants the GOF to be displayed
                legend("topleft", legend = c(paste0("Spearman's rho=", rho_value),
                                             paste0(legend_gof, gof_value)))
            }
        } else if (ncol(coor) >= 2 && all(data == 0)) {
            ## if the input matrix is filled only with zeroes -> error:
            plot(x = 0, y = 0, xlab = "", ylab = "", axes = FALSE,
                 xlim = c(-2, 2), ylim = c(-2, 2), pch = "")
            text(x = 0, y = 0.5,
                 labels = "The MMD matrix contains only zeroes.",
                 col = "black")
            text(x = 0, y = -0.5, labels = "Impossible to get a MDS plot.",
                 col = "black")
        } else { # ncol(coor)<2, so that MDS cannot be computed
            plot(x = 0, y = 0, xlab = "", ylab = "", axes = FALSE,
                 xlim = c(-2, 2), ylim = c(-2, 2), pch = "")
            text(x = 0, y = 0, col = "black",
                 labels = "The representation could not be computed since there is only one positive eigenvalue.")
        }
}

plot_mmd <- function(data,
                     method = c("classical", "interval", "ratio", "ordinal"),
                     axes = FALSE, gof = FALSE, dim = 2, asp = TRUE,
                     xlim = NULL) {
### data: symmetrical matrix of MMD values
### method: type of MDS. "classical" for cmdscale, or one of SMACOF methods
### axes: boolean, display axes on plot or not
### gof: boolean, display goodness of fit value on plot or not
### dim: *maximal* dimension for the calculation of MDS coordinates.
###      /!\ In fine, the solution may be computed on 2 axes only even if dim=3.
### asp: boolean, TRUE passes "asp=1" to plot function.

########################################
### 1. Verify and set some arguments ###
########################################
    ## 'translate' asp parameter as required by plot function:
    asp_value <- ifelse(asp == TRUE, 1, NA)
    method <- match.arg(method) # avoid a warning if no arg is given
    if (! dim %in% c(2, 3)) {
        stop("Incorrect value for dim parameter: please choose 2 or 3.")
    }

#############################################################
### 2. Compute the MDS according to user-defined criteria ###
#############################################################
    ## MDS solution:
    sol <- compute_mds(data = data, method = method, gof = gof, dim = dim)
    ## Set the limits for the x-axis on 2D plot, if no arg is given:
    if (is.null(xlim)) {
        border_value <- max(abs(min(sol$coor[, 1])), abs(max(sol$coor[, 1])))
        xlim <- c(-1.17 * border_value, 1.17 * border_value)
    }

###########################
### 3. Display MDS plot ###
###########################
    ## 3.1. MDS 2D
    ## (if the user wanted a 2D plot, or if a 3D plot could not be computed)
    if ((dim == 2) | (dim == 3 & ncol(sol$coor) < 3) | (dim == 3 & ncol(sol$coor) >= 3 & min(sol$var_by_dim) < 2e-16)) {
        plot_mds2d(data = data, coor = sol$coor, axes = axes,
                   legend_plot = sol$legend_plot, asp_value = asp_value,
                   xlim = xlim, gof = gof, legend_gof = sol$legend_gof,
                   gof_value = sol$gof_value, rho_value = sol$rho_value)

    } else { # dim=3 & ncol(coor)>=3 & sufficient variability on third axis: a 3D graph is possible
        ## 3.2. MDS 3D:
        graphe <- scatterplot3d::scatterplot3d(
                                     x = sol$coor[, 1],
                                     y = sol$coor[, 2],
                                     z = sol$coor[, 3],
                                     axis = axes,
                                     pch = 16,
                                     highlight.3d = TRUE,
                                     type = "h",
                                     main = sol$legend_plot,
                                     asp = asp_value,
                                     xlab = "",
                                     ylab = "",
                                     zlab = "")
        coord_labels <- graphe$xyz.convert(x = sol$coor[, 1],
                                           y = sol$coor[, 2],
                                           z = sol$coor[, 3])
        text(x = coord_labels$x, y = coord_labels$y,
             pos = 3, labels = rownames(sol$coor))
        if (gof == TRUE) { # if the user wants the GOF to be displayed
            legend("topleft",
                   legend = c(paste0("Spearman's rho=", sol$rho_value),
                              paste0(sol$legend_gof, sol$gof_value)))
        }
    }
}
