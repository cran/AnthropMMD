plot.anthropmmd_boot <- function(
    x,
    method = c("classical", "interval", "ratio", "ordinal"),
    level = 0.95,
    pch = 16,
    gof = FALSE,
    xlab = NA,
    ylab = NA,
    main = "MDS plot of original and bootstrapped samples",
    ...
) {
### x: an object of class "anthropmmd_boot"

    ## Check input:
    stopifnot(class(x) == "anthropmmd_boot")
    method <- match.arg(method)

    ############
    ## 1. MDS ##
    ############
    ## Compute MDS coordinates:
    mdsboot <- compute_mds(data = x, dim = 2, gof = gof, method = method)
    coor <- as.data.frame(mdsboot$coor) # MDS coordinates
    colnames(coor) <- c("x", "y")

    ## Dataframe of MDS coordinates:
    coor$Group <- gsub(
        x = rownames(coor),
        pattern = "([[:graph:] ]*)(_boot[0-9]*)$",
        replacement = "\\1"
    ) |> factor()
    coor$Type <- ifelse(
        grepl(pattern = "_boot[0-9]*", x = rownames(coor)),
        yes = "boot",
        no = "original"
    ) |> factor()

    #########################
    ## 2. Display MDS plot ##
    #########################
    if (gof) {
        goflegend <- paste0(mdsboot$legend_gof, mdsboot$gof_value)
    } else {
        goflegend <- NULL
    }
    plot(x = coor$x, y = coor$y,
         pch = pch, xlab = xlab, ylab = ylab,
         col = coor$Group,
         xlim = c(1.2 * min(coor$x), max(coor$x)),
         ylim = c(min(coor$y), 1.2 * max(coor$y)),
         cex = ifelse(coor$Type == "original", 2.2, 0.8),
         main = main,
         sub = goflegend,
         ...)
    legend("topleft", col = 1:nlevels(coor$Group),
           legend = levels(coor$Group), pch = pch)

    ## Compute KDE2d for each group (using only bootstrapped data):
    coor <- coor[coor$Type == "boot", ]
    for (g in 1:nlevels(coor$Group)) {
        temp <- subset(coor, coor$Group == levels(coor$Group)[g])
        kd <- MASS::kde2d(
            x = temp$x,
            y = temp$y,
            n = 100,
            lims = 1.1 * c(min(coor$x), max(coor$x), min(coor$y), max(coor$y))
        )
        contour(kd, levels  =  c(0.95), col = g, add = TRUE,
                xlim = c(min(coor$x), max(coor$x)),
                ylim = c(min(coor$y), max(coor$y)))
    }
}
