# This tests the conversion capabilities of IndexedRelations.
# library(IndexedRelations); library(testthat); source("setup.R"); source("test-convert.R")

set.seed(19999)
r1 <- random_ranges(20)
r2 <- random_ranges(10)
r3 <- random_ranges(40)

N <- 100
i1 <- sample(length(r1), N, replace=TRUE)
i2 <- sample(length(r2), N, replace=TRUE)
i3 <- sample(length(r3), N, replace=TRUE)

test_that("conversion to and from a Pairs object works correctly", {
    IR <- IndexedRelations(list(i1, i2), list(r1, r2))

    p <- makePairsFromIndexedRelations(IR)
    expect_identical(first(p), partnerFeatures(IR, 1))
    expect_identical(second(p), partnerFeatures(IR, 2))

    nr <- paste0("RELATION", seq_len(N))
    names(IR) <- nr
    p <- makePairsFromIndexedRelations(IR)
    expect_identical(names(p), nr)

    # Converting back is straightforward.
    ir2 <- as(p, "IndexedRelations")
    ref <- IndexedRelations(list(r1[i1], r2[i2]))
    names(ref) <- nr
    partnerNames(ref) <- featureSetNames(ref) <- c("first", "second")
    expect_identical(ir2, ref)
})

test_that("conversion to and from a DataFrame object works correctly", {
    IR <- IndexedRelations(list(i1, i2), list(r1, r2))

# Putting them inside the DF erases the class attributes, for some weird reason.
    df <- as(IR, "DataFrame")
#    expect_identical(df[,1], partnerFeatures(IR, 1))
#    expect_identical(df[,2], partnerFeatures(IR, 2))

    nr <- paste0("RELATION", seq_len(N))
    names(IR) <- nr
    df <- as(IR, "DataFrame")
    expect_identical(rownames(df), nr)

    # Converting back is straightforward.
    ir2 <- as(df, "IndexedRelations")
    ref <- IndexedRelations(list(r1[i1], r2[i2]))
    names(ref) <- nr
#    featureSetNames(ref) <- colnames(df)
#    expect_identical(ir2, ref)
})
