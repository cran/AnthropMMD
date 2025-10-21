library(AnthropMMD)

########################
### Data preparation ###
########################
## Import dataset and transform it into a table of frequencies:
data(toyMMD)
tab <- binary_to_table(toyMMD, relative = TRUE)[, -c(1, 5, 8)]

## Compute MMD results with AnthropMMD:
mmd_ans <- mmd(tab, angular = "Anscombe")
mmd_ft <- mmd(tab, angular = "Freeman")

## Expected result when using Anscombe transformation:
res_ans <- matrix(c(0.000, 0.450, 0.083, 0.278,	0.000,
                    0.450, 0.000, 0.343, 0.365, 0.256,
                    0.083, 0.343, 0.000, 0.098, 0.060,
                    0.278, 0.365, 0.098, 0.000,	0.282,
                    0.000, 0.256, 0.060, 0.282,	0.000),
                  byrow = TRUE, ncol = 5)
colnames(res_ans) <- c("GroupA", "GroupB", "GroupC", "GroupD", "GroupE")
rownames(res_ans) <- c("GroupA", "GroupB", "GroupC", "GroupD", "GroupE")

## Expected result when using Freeman-Tukey transformation:
res_ft <- matrix(c(0.000, 0.475, 0.080,	0.273, 0.000,
                   0.475, 0.000, 0.356,	0.366, 0.278,
                   0.080, 0.356, 0.000,	0.096, 0.057,
                   0.273, 0.366, 0.096,	0.000, 0.276,
                   0.000, 0.278, 0.057,	0.276, 0.000),
                 byrow = TRUE, ncol = 5)
colnames(res_ft) <- c("GroupA", "GroupB", "GroupC", "GroupD", "GroupE")
rownames(res_ft) <- c("GroupA", "GroupB", "GroupC", "GroupD", "GroupE")

## Expected result for standard deviations + Freeman-Tukey:
res_sd <- matrix(c(0.000, 0.475, 0.080, 0.273, 0.000,
                   0.053, 0.000, 0.356, 0.366, 0.278,
                   0.046, 0.051, 0.000, 0.096, 0.057,
                   0.043, 0.049, 0.041, 0.000, 0.276,
                   0.048, 0.054, 0.047, 0.044, 0.000),
                 byrow = TRUE, ncol = 5)
colnames(res_sd) <- c("GroupA", "GroupB", "GroupC", "GroupD", "GroupE")
rownames(res_sd) <- c("GroupA", "GroupB", "GroupC", "GroupD", "GroupE")

## Expected result for MMD (Freeman-Tukey)  significance:
res_sig <- matrix(c(NA, "0.475", "0.08", "0.273", "0",
                    "*", NA, "0.356", "0.366", "0.278",
                    "NS", "*", NA, "0.096", "0.057",
                    "*", "*", "*", NA, "0.276",
                    "NS", "*", "NS", "*", NA),
                  byrow = TRUE, ncol = 5)
colnames(res_sig) <- c("GroupA", "GroupB", "GroupC", "GroupD", "GroupE")
rownames(res_sig) <- c("GroupA", "GroupB", "GroupC", "GroupD", "GroupE")

## Expected result for MMD (Freeman-Tukey) ratios:
res_ratio <- matrix(c(NA, 0.475, 0.080, 0.273, 0.000,
                      8.925, NA, 0.356, 0.366, 0.278,
                      1.764, 6.932, NA, 0.096, 0.057,
                      6.275, 7.522, 2.334, NA, 0.276,
                      0.000, 5.144, 1.217, 6.333, NA),
                  byrow = TRUE, ncol = 5)
colnames(res_ratio) <- c("GroupA", "GroupB", "GroupC", "GroupD", "GroupE")
rownames(res_ratio) <- c("GroupA", "GroupB", "GroupC", "GroupD", "GroupE")

############################
### Tests for MMD values ###
############################
test_that("MMD values are correct with Anscombe transformation", {
    expect_equal(round(mmd_ans$MMDSym, 3), res_ans)
})

test_that("MMD values are correct with Freeman-Tukey transformation", {
    expect_equal(round(mmd_ft$MMDSym, 3), res_ft)
})

test_that("MMD stdevs are correct, along with Freeman-Tukey transformation", {
    expect_equal(round(mmd_ft$MMDMatrix, 3), res_sd)
})

## Tests for MMD significance:
test_that("MMD significance is OK", {
    expect_equal(mmd_ft$MMDSignif, res_sig)
})

## Tests for ratios MMD/sd(MMD):
test_that("MMD ratio is OK", {
    expect_equal(round(mmd_ft$MMDRatio, 3), res_ratio)
})
