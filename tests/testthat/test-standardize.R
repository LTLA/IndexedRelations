# This tests the standardization utilities.
# library(testthat); library(IndexedRelations); source("setup.R"); source("test-standardize.R")

set.seed(39999)
r1 <- random_ranges(20)
r2 <- random_ranges(10)
r3 <- random_ranges(40)

N <- 100
i1 <- sample(length(r1), N, replace=TRUE)
i2 <- sample(length(r2), N, replace=TRUE)
i3 <- sample(length(r3), N, replace=TRUE)

################################
################################

test_that("standardizeFeatureSets works correctly with trivial mapping", {
    # Same feature set.
    ir1 <- IndexedRelations(list(i1, i2, i3), list(r1, r2, r3))
    ir2 <- IndexedRelations(list(i1, i2, i3), list(rev(r1), rev(r2), rev(r3)))

    out <- standardizeFeatureSets(ir1, list(ir2))
    expect_identical(out$x, ir1)
    expect_as_if(out$objects[[1]], ir2)

    expect_identical(featureSets(out$objects[[1]]), featureSets(ir1))
    expect_identical(mapping(out$x), 1:3)
    expect_identical(mapping(out$objects[[1]]), 1:3)

    # Completely different feature set.
    a1 <- random_ranges(10)
    a2 <- random_ranges(20)
    a3 <- random_ranges(30)

    N2 <- 50
    j1 <- sample(length(a1), N2, replace=TRUE)
    j2 <- sample(length(a2), N2, replace=TRUE)
    j3 <- sample(length(a3), N2, replace=TRUE)

    ir3 <- IndexedRelations(list(j1, j2, j3), list(a1, a2, a3))
    out <- standardizeFeatureSets(ir1, list(ir3))

    expect_as_if(out$x, ir1)
    expect_as_if(out$objects[[1]], ir3)

    expect_identical(featureSets(out$x), featureSets(out$objects[[1]]))
    expect_identical(mapping(out$x), 1:3)
    expect_identical(mapping(out$objects[[1]]), 1:3)

    # Behaves with multiple entries.
    b1 <- random_ranges(10)
    b2 <- random_ranges(20)
    b3 <- random_ranges(30)

    N3 <- 10
    k1 <- sample(length(a1), N3, replace=TRUE)
    k2 <- sample(length(a2), N3, replace=TRUE)
    k3 <- sample(length(a3), N3, replace=TRUE)

    ir4 <- IndexedRelations(list(k1, k2, k3), list(b1, b2, b3))
    out <- standardizeFeatureSets(ir1, list(ir3, ir4))

    expect_as_if(out$x, ir1)
    expect_as_if(out$objects[[1]], ir3)
    expect_as_if(out$objects[[2]], ir4)

    expect_identical(featureSets(out$x), featureSets(out$objects[[1]]))
    expect_identical(featureSets(out$x), featureSets(out$objects[[2]]))

    expect_identical(mapping(out$x), 1:3)
    expect_identical(mapping(out$objects[[1]]), 1:3)
})

test_that("standardizeFeatureSets works correctly with single mapping", {
    # Same feature set.
    R <- r3
    ir1 <- IndexedRelations(list(i1, i2, i3), list(R), mapping=rep(1L, 3))
    ir2 <- IndexedRelations(list(i1, i2, i3), list(rev(R)), mapping=rep(1L, 3))

    out <- standardizeFeatureSets(ir1, list(ir2))
    expect_identical(out$x, ir1)
    expect_as_if(out$objects[[1]], ir2)

    expect_identical(featureSets(out$objects[[1]]), featureSets(ir1))
    expect_identical(featureSets(out$x)[[1]], R)

    expect_identical(mapping(out$x), rep(1L, 3))
    expect_identical(mapping(out$objects[[1]]), rep(1L, 3))

    # Completely different feature set.
    A <- random_ranges(30)
    N2 <- 50
    j1 <- sample(length(A), N2, replace=TRUE)
    j2 <- sample(length(A), N2, replace=TRUE)
    j3 <- sample(length(A), N2, replace=TRUE)

    ir3 <- IndexedRelations(list(j1, j2, j3), list(A), mapping=rep(1L, 3))
    out <- standardizeFeatureSets(ir1, list(ir3))

    expect_as_if(out$x, ir1)
    expect_as_if(out$objects[[1]], ir3)

    expect_identical(featureSets(out$x), featureSets(out$objects[[1]]))
    expect_identical(length(featureSets(out$x)), 1L)

    expect_identical(mapping(out$x), rep(1L, 3))
    expect_identical(mapping(out$objects[[1]]), rep(1L, 3))

    # Behaves with multiple entries.
    B <- random_ranges(10)
    N3 <- 10
    k1 <- sample(length(B), N3, replace=TRUE)
    k2 <- sample(length(B), N3, replace=TRUE)
    k3 <- sample(length(B), N3, replace=TRUE)

    ir4 <- IndexedRelations(list(k1, k2, k3), list(B), mapping=rep(1L, 3))
    out <- standardizeFeatureSets(ir1, list(ir3, ir4))

    expect_as_if(out$x, ir1)
    expect_as_if(out$objects[[1]], ir3)
    expect_as_if(out$objects[[2]], ir4)

    expect_identical(featureSets(out$x), featureSets(out$objects[[1]]))
    expect_identical(featureSets(out$x), featureSets(out$objects[[2]]))
    expect_identical(length(featureSets(out$x)), 1L)

    expect_identical(mapping(out$x), rep(1L, 3))
    expect_identical(mapping(out$objects[[1]]), rep(1L, 3))
})

test_that("standardizeFeatureSets works correctly with cross-mapping", {
    # Same feature sets. 
    ir1 <- IndexedRelations(list(i1, i2, i3), list(r3, r3, r3))
    ir2 <- IndexedRelations(list(i1, i2, i3), list(r3), mapping=rep(1L, 3))

    out <- standardizeFeatureSets(ir1, list(ir2))
    expect_identical(ir1, out$x)
    expect_as_if(ir2, out$objects[[1]])
    expect_identical(featureSets(out$x), featureSets(out$objects[[1]]))
    expect_identical(length(featureSets(out$x)), 3L)

    expect_identical(mapping(out$x), 1:3)
    expect_identical(mapping(out$objects[[1]]), 1:3)

    # Different feature sets.
    A <- random_ranges(30)
    N2 <- 50
    j1 <- sample(length(A), N2, replace=TRUE)
    j2 <- sample(length(A), N2, replace=TRUE)
    j3 <- sample(length(A), N2, replace=TRUE)

    irx <- IndexedRelations(list(i1, i2, i3), list(r1, r2, r3))
    iry <- IndexedRelations(list(j1, j2, j3), list(A), mapping=rep(1L, 3))
    out <- standardizeFeatureSets(irx, list(iry))

    expect_as_if(irx, out$x)
    expect_as_if(iry, out$objects[[1]])
    expect_identical(featureSets(out$x), featureSets(out$objects[[1]]))
    expect_identical(length(featureSets(out$x)), 3L)

    expect_identical(mapping(out$x), 1:3)
    expect_identical(mapping(out$objects[[1]]), 1:3)
})

test_that("standardizeFeatureSets responds to changes ONLY in mapping", {
    ir1 <- IndexedRelations(list(i1, i2, i3), list(r1, r2, r3))
    ir2 <- IndexedRelations(list(i3, i2, i1), list(r1, r2, r3), mapping=3:1)

    out <- standardizeFeatureSets(ir1, list(ir2))
    expect_as_if(ir1, out$x)
    expect_as_if(ir2, out$objects[[1]])

    expect_identical(featureSets(out$x), featureSets(out$objects[[1]]))
    expect_identical(length(featureSets(out$x)), 3L)

    expect_identical(mapping(out$x), 1:3)
    expect_identical(mapping(out$objects[[1]]), 1:3)
})

################################
################################

INFLATE <- function(x) { # Add ties.
    x[rep(seq_along(x), sample(5, length(x), replace=TRUE))]
}

test_that("standardizeFeatureSets works correctly with cleaning", {
    # Same feature set.
    ir1 <- IndexedRelations(list(i1, i2, i3), list(INFLATE(r1), INFLATE(r2), INFLATE(r3)))
    ir2 <- IndexedRelations(list(i1, i2, i3), list(INFLATE(r1), INFLATE(r2), INFLATE(r3)))

    out <- standardizeFeatureSets(ir1, list(ir2), clean=TRUE)
    expect_as_if(out$x, ir1)
    expect_as_if(out$objects[[1]], ir2)
    expect_identical(featureSets(out$x), featureSets(out$objects[[1]]))
    expect_false(any(unlist(lapply(featureSets(out$x), duplicated))))
    expect_false(any(unlist(lapply(featureSets(out$x), is.unsorted))))

    # Completely different feature set.
    a1 <- INFLATE(random_ranges(10))
    a2 <- INFLATE(random_ranges(20))
    a3 <- INFLATE(random_ranges(30))

    N2 <- 50
    j1 <- sample(length(a1), N2, replace=TRUE)
    j2 <- sample(length(a2), N2, replace=TRUE)
    j3 <- sample(length(a3), N2, replace=TRUE)

    ir3 <- IndexedRelations(list(j1, j2, j3), list(a1, a2, a3))
    out <- standardizeFeatureSets(ir1, list(ir3), clean=TRUE)

    expect_as_if(out$x, ir1)
    expect_as_if(out$objects[[1]], ir3)
    expect_identical(featureSets(out$x), featureSets(out$objects[[1]]))
    expect_false(any(unlist(lapply(featureSets(out$x), duplicated))))
    expect_false(any(unlist(lapply(featureSets(out$x), is.unsorted))))

    # With singles.
    A <- INFLATE(random_ranges(30))
    N2 <- 50
    j1 <- sample(length(A), N2, replace=TRUE)
    j2 <- sample(length(A), N2, replace=TRUE)
    j3 <- sample(length(A), N2, replace=TRUE)

    irx <- IndexedRelations(list(j1, j2, j3), list(A), mapping=rep(1L, 3))
    iry <- IndexedRelations(list(i1, i2, i3), list(r1, r2, r3))
    out <- standardizeFeatureSets(irx, list(iry), clean=TRUE)

    expect_as_if(irx, out$x)
    expect_as_if(iry, out$objects[[1]])
    expect_identical(featureSets(out$x), featureSets(out$objects[[1]]))
    expect_false(any(unlist(lapply(featureSets(out$x), duplicated))))
    expect_false(any(unlist(lapply(featureSets(out$x), is.unsorted))))
})

test_that("cleanFeatureSets works as expected", {
    # Same feature set.
    ir1 <- IndexedRelations(list(i1, i2, i3), list(INFLATE(r1), INFLATE(r2), INFLATE(r3)))
    out <- cleanFeatureSets(ir1)
    expect_as_if(ir1, out)
    expect_false(any(unlist(lapply(featureSets(out), duplicated))))
    expect_false(any(unlist(lapply(featureSets(out), is.unsorted))))

    # Different feature set.
    A <- INFLATE(random_ranges(30))
    N2 <- 50
    j1 <- sample(length(A), N2, replace=TRUE)
    j2 <- sample(length(A), N2, replace=TRUE)
    j3 <- sample(length(A), N2, replace=TRUE)

    ir3 <- IndexedRelations(list(j1, j2, j3), list(A), mapping=rep(1L, 3))
    out <- cleanFeatureSets(ir3)
    expect_as_if(ir3, out)
    expect_false(any(unlist(lapply(featureSets(out), duplicated))))
    expect_false(any(unlist(lapply(featureSets(out), is.unsorted))))
}) 

################################
################################

test_that("standardizeFeatureSets handles odd inputs gracefully", {
    ir1 <- IndexedRelations(list(i1, i2, i3), list(r1, r2, r3))
    library(GenomicRanges)
    ir2 <- IndexedRelations(list(i1, i2, i3), list(GRanges("chrA", r1), r2, r3))
    expect_error(standardizeFeatureSets(ir1, list(ir2)), "not comparable")

    # Handles zero-length inputs (in terms of empty feature sets);
    empty <- IndexedRelations(list(integer(0), integer(0), integer(0)), list(r1[0], r2[0], r3[0]))
    out <- standardizeFeatureSets(empty, list(ir1))
    expect_as_if(empty, out$x)
    expect_identical(ir1, out$objects[[1]])
 
    expect_identical(empty, cleanFeatureSets(empty))
})
