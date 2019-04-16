# This tests the show methods.
# library(testthat); library(IndexedRelations); source("setup.R"); source("test-show.R")

set.seed(19999)
r1 <- random_ranges(20)
r2 <- random_ranges(10)
r3 <- random_ranges(40)

N <- 100
i1 <- sample(length(r1), N, replace=TRUE)
i2 <- sample(length(r2), N, replace=TRUE)
i3 <- sample(length(r3), N, replace=TRUE)

test_that("show method works correctly with any number of features", {
    rel <- IndexedRelations(list(i1, i2), list(r1, r2))
    show(rel)

    rel <- IndexedRelations(list(i1, i2, i3), list(r1, r2, r3))
    expect_error(show(rel), NA) # need some expect statements to avoid skipping these.
})

test_that("show method works correctly with named features", {
    rel <- IndexedRelations(list(A=i1, B=i2, C=i3), list(r1, r2, r3))
    show(rel)

    featureSetNames(rel) <- c("X", "Y", "Z") # should have no effect.
    show(rel)

    names(rel) <- sprintf("RELATION_%i", seq_along(rel))
    expect_error(show(rel), NA)
})

test_that("show method works correctly with metadata", {
    rel <- IndexedRelations(list(A=i1, B=i2, C=i3), list(r1, r2, r3))
    mcols(rel)$xxx <- runif(length(rel))
    show(rel)

    mcols(rel)$yyy <- sample(LETTERS, length(rel), replace=TRUE)
    show(rel)

    metadata(rel)$blah <- "BLAH" # should have no effect.
    expect_error(show(rel), NA)
})

test_that("show method works correctly with empty objects", {
    rel <- IndexedRelations(list(A=i1, B=i2, C=i3), list(r1, r2, r3))
    expect_error(show(rel[0]), NA)
})
