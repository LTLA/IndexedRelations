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

    expect_identical(partner(ir, 1), partner(ir2, 1))
    expect_identical(partner(ir, 2), partner(ir2, 2))
    expect_identical(partner(ir, 3), partner(ir2, 3))

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

#########################
# Partner getter/setter #
#########################

test_that("partner getter works correctly", {
    ir <- IndexedRelations(list(i1, i2, i3), list(r1, r2, r3))

    expect_identical(partner(ir, 1), r1[i1])
    expect_identical(partner(ir, 2), r2[i2])
    expect_identical(partner(ir, 3), r3[i3])

    expect_identical(partner(ir, 1, id=TRUE), i1)
    expect_identical(partner(ir, 2, id=TRUE), i2)
    expect_identical(partner(ir, 3, id=TRUE), i3)

    # Works by name.
    ir <- IndexedRelations(list(A=i1, B=i2, C=i3), list(r1, r2, r3))
    expect_identical(partner(ir, "A", id=TRUE), i1)
    expect_identical(partner(ir, "B", id=TRUE), i2)
    expect_identical(partner(ir, "C", id=TRUE), i3)

    expect_identical(partner(ir, "A"), r1[i1])
    expect_identical(partner(ir, "B"), r2[i2])
    expect_identical(partner(ir, "C"), r3[i3])
})

test_that("partner setter works correctly (same feature set)", {
    ir.0 <- ir <- IndexedRelations(list(i1, i2, i3), list(r1, r2, r3))

    partner(ir, 1) <- rev(r1[i1])
    expect_identical(partner(ir, 1), r1[rev(i1)])
    expect_identical(partner(ir, 1, id=TRUE), rev(i1))
    expect_identical(featureSets(ir), featureSets(ir.0))

    partner(ir, 2, id=TRUE) <- rev(i2)
    expect_identical(partner(ir, 2), r2[rev(i2)])
    expect_identical(partner(ir, 2, id=TRUE), rev(i2))
    expect_identical(featureSets(ir), featureSets(ir.0))

    # Works by name.
    ir <- IndexedRelations(list(A=i1, B=i2, C=i3), list(r1, r2, r3))

    partner(ir, "A") <- rev(r1[i1])
    expect_identical(partner(ir, "A"), r1[rev(i1)])
    expect_identical(partner(ir, "A", id=TRUE), rev(i1))
    expect_identical(featureSets(ir), featureSets(ir.0)) 

    partner(ir, "B", id=TRUE) <- rev(i2)
    expect_identical(partner(ir, "B"), r2[rev(i2)])
    expect_identical(partner(ir, "B", id=TRUE), rev(i2))
    expect_identical(featureSets(ir), featureSets(ir.0))
})

test_that("partner setter works correctly (different feature set)", {
    ir <- IndexedRelations(list(i1, i2, i3), list(r1, r2, r3))

    # Guarantee uniqueness from 'r1', for easier testing.
    new.ranges <- random_ranges(length(ir)) 
    width(new.ranges) <- max(width(r1))+1 

    partner(ir, 1) <- new.ranges
    expect_identical(partner(ir, 1), new.ranges)
    expect_identical(partner(ir, 1, id=TRUE), match(new.ranges, unique(new.ranges)) + length(r1))
    expect_identical(featureSets(ir)[[1]], c(r1, unique(new.ranges)))

    # Partially unique from 'r2'.
    chosen <- sample(length(ir), 10)
    new.ranges <- random_ranges(10)
    width(new.ranges) <- max(width(r2))+1 

    partner(ir, 2)[chosen] <- new.ranges
    expect_identical(partner(ir, 2)[-chosen], r2[i2[-chosen]])
    expect_identical(partner(ir, 2)[chosen], new.ranges)
    expect_identical(featureSets(ir)[[2]], c(r2, unique(new.ranges[order(chosen)])))
})

#########################
# Feature getter/setter #
#########################

test_that("feature getter/setter works correctly", {
    ir <- IndexedRelations(list(i1, i2, i3), list(A=r1, B=r2, C=r3))

    featureSets(ir)[[1]] <- resize(featureSets(ir)[[1]], width=100)
    expect_identical(partner(ir, 1), resize(r1[i1], width=100))

    X <- runif(length(r2))
    mcols(featureSets(ir)[["B"]])$blah <- X
    expect_identical(mcols(partner(ir, 2))$blah, X[i2])
})
