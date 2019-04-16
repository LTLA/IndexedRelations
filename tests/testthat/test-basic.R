# Basic tests for the IndexedRelations class.
# library(testthat); library(IndexedRelations); source("setup.R"); source("test-basic.R")

set.seed(19999)
r1 <- random_ranges(20)
r2 <- random_ranges(10)
r3 <- random_ranges(40)

N <- 100
i1 <- sample(length(r1), N, replace=TRUE)
i2 <- sample(length(r2), N, replace=TRUE)
i3 <- sample(length(r3), N, replace=TRUE)

################
# Constructors #
################

test_that("basic construction works correctly", {
    IR <- IndexedRelations(list(i1, i2, i3), list(r1, r2, r3))
    expect_identical(length(IR), as.integer(N))

    expected <- DataFrame(i1, i2, i3)
    colnames(expected) <- c("X.1", "X.2", "X.3")
    expect_identical(partners(IR), expected)
    expect_identical(featureSets(IR), List(r1, r2, r3))
    expect_identical(mapping(IR), seq_len(3))

    # Preserves names.
    IRn <- IndexedRelations(list(X=i1, Y=i2, Z=i3), list(A=r1, B=r2, C=r3))
    expect_identical(partnerNames(IRn), c("X", "Y", "Z"))
    expect_identical(featureSetNames(IRn), c("A", "B", "C"))

    # Handles zero-length inputs.
    ir0 <- IndexedRelations(list(integer(0), integer(0), integer(0)), list(r1, r2, r3))
    expect_identical(length(ir0), 0L)
    expect_identical(featureSets(ir0), featureSets(IR))

    ir0 <- IndexedRelations(list(integer(0), integer(0), integer(0)), list(r1[0], r2[0], r3[0]))
    expect_identical(length(ir0), 0L)
})

test_that("complex construction works correctly", {
    ir <- IndexedRelations(list(i1, i2, i3), list(r1, r2, r3))
    ir2 <- IndexedRelations(list(r1[i1], r2[i2], r3[i3]))

    expect_as_if(ir, ir2)
    expect_identical(featureSets(ir2)[[1]], r1[unique(i1)])
    expect_identical(featureSets(ir2)[[2]], r2[unique(i2)])
    expect_identical(featureSets(ir2)[[3]], r3[unique(i3)])

    # Preserves names.
    ir3 <- IndexedRelations(list(A=r1[i1], B=r2[i2], C=r3[i3]))
    expect_identical(partnerNames(ir3), c("A", "B", "C"))
    expect_identical(featureSetNames(ir3), c("A", "B", "C"))

    # Handles zero-length inputs.
    ir0 <- IndexedRelations(list(r1[0], r2[0], r3[0]))
    expect_identical(length(ir0), 0L)
})

test_that("constructors fail with invalid inputs", {
    expect_error(IndexedRelations(list(-1L, i2, i3), list(r1, r2, r3)), "out-of-bounds")
    expect_error(IndexedRelations(list(10000L, i2, i3), list(r1, r2, r3)), "out-of-bounds")
    expect_error(IndexedRelations(list(NA, i2, i3), list(r1, r2, r3)), "out-of-bounds")

    expect_error(IndexedRelations(list(i1, i2, i3), list(r1)), "out-of-bounds")
    expect_error(IndexedRelations(list(i1, i2, i3), list(r1, r2, r3), mapping=1:4), "should be the same")

    expect_error(IndexedRelations(list(), list()), NA)
})

########################
# Name-related methods #
########################

test_that("names getting and setting works", {
    IR <- IndexedRelations(list(i1, i2, i3), list(r1, r2, r3))
    expect_identical(names(IR), NULL)

    all.inters <- sprintf("LINK.%i", seq_along(IR))
    names(IR) <- all.inters
    expect_identical(names(IR), all.inters)

    names(IR) <- NULL
    expect_identical(names(IR), NULL)
})

test_that("other names getting and setting works", {
    IR <- IndexedRelations(list(A=i1, B=i2, C=i3), list(r1, r2, r3))
    expect_identical(partnerNames(IR), c("A", "B", "C"))

    partnerNames(IR) <- c("C", "B", "A")
    expect_identical(partnerNames(IR), c("C", "B", "A"))

    featureSetNames(IR) <- c("C", "B", "A")
    expect_identical(featureSetNames(IR), c("C", "B", "A"))
})

###################
# Getters/setters #
###################

test_that("partner getter works correctly", {
    ir <- IndexedRelations(list(i1, i2, i3), list(r1, r2, r3))

    expect_identical(partnerFeatures(ir, 1), r1[i1])
    expect_identical(partnerFeatures(ir, 2), r2[i2])
    expect_identical(partnerFeatures(ir, 3), r3[i3])

    expect_identical(partners(ir)[,1], i1)
    expect_identical(partners(ir)[,2], i2)
    expect_identical(partners(ir)[,3], i3)

    # Works by name.
    ir <- IndexedRelations(list(A=i1, B=i2, C=i3), list(r1, r2, r3))
    expect_identical(partners(ir)[,"A"], i1)
    expect_identical(partners(ir)[,"B"], i2)
    expect_identical(partners(ir)[,"C"], i3)

    expect_identical(partnerFeatures(ir, "A"), r1[i1])
    expect_identical(partnerFeatures(ir, "B"), r2[i2])
    expect_identical(partnerFeatures(ir, "C"), r3[i3])
})

test_that("partner setter works correctly (same feature set)", {
    ir.0 <- ir <- IndexedRelations(list(i1, i2, i3), list(r1, r2, r3))

    partnerFeatures(ir, 1) <- rev(r1[i1])
    expect_identical(partnerFeatures(ir, 1), r1[rev(i1)])
    expect_identical(partners(ir)[,1], rev(i1))
    expect_identical(featureSets(ir), featureSets(ir.0))

    partners(ir)[,2] <- rev(i2)
    expect_identical(partnerFeatures(ir, 2), r2[rev(i2)])
    expect_identical(partners(ir)[,2], rev(i2))
    expect_identical(featureSets(ir), featureSets(ir.0))

    # Works by name.
    ir <- IndexedRelations(list(A=i1, B=i2, C=i3), list(r1, r2, r3))

    partnerFeatures(ir, "A") <- rev(r1[i1])
    expect_identical(partnerFeatures(ir, "A"), r1[rev(i1)])
    expect_identical(partners(ir)[,"A"], rev(i1))
    expect_identical(featureSets(ir), featureSets(ir.0)) 

    partners(ir)[,"B"] <- rev(i2)
    expect_identical(partnerFeatures(ir, "B"), r2[rev(i2)])
    expect_identical(partners(ir)[,"B"], rev(i2))
    expect_identical(featureSets(ir), featureSets(ir.0))
})

test_that("partner setter works correctly (different feature set)", {
    ir <- IndexedRelations(list(i1, i2, i3), list(r1, r2, r3))

    # Guarantee uniqueness from 'r1', for easier testing.
    new.ranges <- random_ranges(length(ir)) 
    width(new.ranges) <- max(width(r1))+1 

    partnerFeatures(ir, 1) <- new.ranges
    expect_identical(partnerFeatures(ir, 1), new.ranges)
    expect_identical(partners(ir)[,1], match(new.ranges, unique(new.ranges)) + length(r1))
    expect_identical(featureSets(ir)[[1]], c(r1, unique(new.ranges)))

    # Partially unique from 'r2'.
    chosen <- sample(length(ir), 10)
    new.ranges <- random_ranges(10)
    width(new.ranges) <- max(width(r2))+1 

    partnerFeatures(ir, 2)[chosen] <- new.ranges
    expect_identical(partnerFeatures(ir, 2)[-chosen], r2[i2[-chosen]])
    expect_identical(partnerFeatures(ir, 2)[chosen], new.ranges)
    expect_identical(featureSets(ir)[[2]], c(r2, unique(new.ranges[order(chosen)])))
})

test_that("feature getter/setter works correctly", {
    ir <- IndexedRelations(list(i1, i2, i3), list(A=r1, B=r2, C=r3))

    featureSets(ir)[[1]] <- resize(featureSets(ir)[[1]], width=100)
    expect_identical(partnerFeatures(ir, 1), resize(r1[i1], width=100))

    X <- runif(length(r2))
    mcols(featureSets(ir)[["B"]])$blah <- X
    expect_identical(mcols(partnerFeatures(ir, 2))$blah, X[i2])
})

############################
# Subsetting and combining #
############################

test_that("subsetting works correctly", {
    original <- list(i1, i2, i3)
    ir <- IndexedRelations(original, list(A=r1, B=r2, C=r3))
    expect_identical(ir[1:10], IndexedRelations(lapply(original, "[", 1:10), list(A=r1, B=r2, C=r3)))
    expect_identical(ir[10:1], IndexedRelations(lapply(original, "[", 10:1), list(A=r1, B=r2, C=r3)))
})

test_that("combining works correctly", {
    # Same features.
    original <- list(i1, i2, i3)
    ir <- IndexedRelations(original, list(A=r1, B=r2, C=r3))
    ir2 <- c(ir, ir[100:80])

    modified <- mapply(c, original, lapply(original, "[", 100:80), SIMPLIFY=FALSE)
    expect_identical(ir2, IndexedRelations(modified, list(A=r1, B=r2, C=r3)))

    # Different features.
    irx <- IndexedRelations(original[c(3,1,2)], list(A=r3, B=r1, C=r2))
    ir3 <- c(ir, irx)

    for (i in 1:3) {
        expect_identical(partnerFeatures(ir3, i), c(partnerFeatures(ir, i), partnerFeatures(irx, i)))
    }

    expect_identical(featureSets(ir3)[[1]], unique(c(r1, r3)))
    expect_identical(featureSets(ir3)[[2]], unique(c(r2, r1)))
    expect_identical(featureSets(ir3)[[3]], unique(c(r3, r2)))

    # Crashes with incompatible features.
    expect_error(c(ir, IndexedRelations(original[1:2], list(A=r1, B=r2))), "feature sets")
    expect_error(c(ir, IndexedRelations(original[1:2], list(A=r1, B=r2, C=r3))), "same number of columns")
})

test_that("subset assignment works correctly", {
    # Same features.
    original <- list(i1, i2, i3)
    ir <- ir2 <- IndexedRelations(original, list(A=r1, B=r2, C=r3))
    ir2[21:30] <- ir[1:10]

    modified <- lapply(original, "[", c(1:20, 1:10, 31:100))
    expect_identical(ir2, IndexedRelations(modified, list(A=r1, B=r2, C=r3)))

    # Different features.
    irx <- IndexedRelations(original[c(3,1,2)], list(A=r3, B=r1, C=r2))
    ir3 <- ir
    ir3[21:30] <- irx[1:10]

    for (i in 1:3) {
        ref <- partnerFeatures(ir, i)
        ref[21:30] <- partnerFeatures(irx, i)[1:10]
        expect_identical(partnerFeatures(ir3, i), ref)
    }

    expect_identical(featureSets(ir3)[[1]], unique(c(r1, r3)))
    expect_identical(featureSets(ir3)[[2]], unique(c(r2, r1)))
    expect_identical(featureSets(ir3)[[3]], unique(c(r3, r2)))

    # Crashes with incompatible features.
    expect_error(ir[1:100] <- IndexedRelations(original[1:2], list(A=r1, B=r2)), "feature sets")
    expect_error(ir[1:100] <- IndexedRelations(original[1:2], list(A=r1, B=r2, C=r3)), "same number of columns")
})
