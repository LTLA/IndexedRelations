library(IRanges)

random_ranges <- function(n) {
    starts <- sample(1000, n, replace=TRUE)
    widths <- sample(100, n, replace=TRUE) 
    out <- IRanges(starts, width=widths)
    unique(out)
}

expect_as_if <- function(x, y) {
    n <- ncol(partners(x))
    expect_identical(n, ncol(partners(y)))
    for (i in seq_len(n)) {
        expect_identical(partnerFeatures(x, i), partnerFeatures(y, i))
    }
    invisible(NULL)
}
