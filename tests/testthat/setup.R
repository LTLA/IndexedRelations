library(IRanges)

random_ranges <- function(n) {
    starts <- sample(1000, n, replace=TRUE)
    widths <- sample(100, n, replace=TRUE) 
    out <- IRanges(starts, width=widths)
    unique(out)
}

