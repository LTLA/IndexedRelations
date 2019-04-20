# This tests the dropping of unused feature (sets).
# library(testthat); library(IndexedRelations); source("setup.R"); source("test-drop.R")

r1 <- random_ranges(20)
r2 <- random_ranges(10)
r3 <- random_ranges(5)

test_that("dropUnusedFeatures works as expected with multiple features", {
    N <- 3
    i1 <- sample(length(r1), N)
    i2 <- sample(length(r2), N)
    i3 <- sample(length(r3), N)

    IR <- IndexedRelations(list(i1, i2, i3), featureSets=list(r1, r2, r3))
    IR2 <- dropUnusedFeatures(IR)
    expect_as_if(IR, IR2)

    expect_identical(featureSets(IR)[[1]][i1], featureSets(IR2)[[1]])
    expect_identical(featureSets(IR)[[2]][i2], featureSets(IR2)[[2]])
    expect_identical(featureSets(IR)[[3]][i3], featureSets(IR2)[[3]])

    # Works with duplicated features.
    IR3 <- dropUnusedFeatures(c(IR, IR))
    expect_identical(featureSets(IR2), featureSets(IR3))
})

test_that("dropUnusedFeatures works with unused feature sets", {
    N <- 4
    i1 <- sample(length(r1), N)
    i2 <- sample(length(r2), N)
    i3 <- sample(length(r3), N)

    IR <- IndexedRelations(list(i1, i2, i3), featureSets=list(r1, r2, r3, r1, r2, r3), mapping=1:3)
    IR2 <- dropUnusedFeatures(IR)
    expect_as_if(IR, IR2)

    expect_identical(featureSets(IR)[[1]][i1], featureSets(IR2)[[1]])
    expect_identical(featureSets(IR)[[2]][i2], featureSets(IR2)[[2]])
    expect_identical(featureSets(IR)[[3]][i3], featureSets(IR2)[[3]])

    expect_identical(lengths(featureSets(IR2))[4:6], integer(3))
})

test_that("dropUnusedFeatures works with singletons", {
    N <- 10
    i1 <- sample(length(r1), N)
    i2 <- sample(length(r1), N)
    i3 <- sample(length(r1), N)

    IR <- IndexedRelations(list(i1, i2, i3), featureSets=list(r1), mapping=rep(1L, 3))
    IR2 <- dropUnusedFeatures(IR)
    expect_as_if(IR, IR2)

    expect_identical(featureSets(IR)[[1]][unique(c(i1, i2, i3))], featureSets(IR2)[[1]])

    # Works with duplicated features.
    IR3 <- dropUnusedFeatures(c(IR, IR))
    expect_identical(featureSets(IR2), featureSets(IR3))
})

test_that("dropUnusedFeatures works with empty objects", {
    IR <- IndexedRelations(list(integer(0), integer(0), integer(0)), 
        featureSets=list(r1, r2, r3))
    IR2 <- dropUnusedFeatures(IR)
    expect_as_if(IR, IR2)

    expect_identical(lengths(featureSets(IR2)), integer(3))
})

test_that("dropUnusedFeatureSets works as expected", {
    N <- 4
    i1 <- sample(length(r1), N)
    i2 <- sample(length(r2), N)
    i3 <- sample(length(r3), N)

    IR <- IndexedRelations(list(i1, i2, i3), featureSets=list(r1, r2, r3, r1, r2, r3), mapping=1:3)
    IR2 <- dropUnusedFeatureSets(IR)
    expect_as_if(IR, IR2)
    expect_identical(featureSets(IR2)[[1]], r1)
    expect_identical(featureSets(IR2)[[2]], r2)
    expect_identical(featureSets(IR2)[[3]], r3)

    # Mixing it up with non-trivial mapping.
    IR <- IndexedRelations(list(i3, i1), featureSets=list(r1, r2, r3), mapping=c(3L, 1L))
    IR3 <- dropUnusedFeatureSets(IR)
    expect_as_if(IR, IR3)
    expect_identical(featureSets(IR3)[[1]], r3)
    expect_identical(featureSets(IR3)[[2]], r1)
    expect_identical(featureSets(IR2)[[3]], r3)
})

