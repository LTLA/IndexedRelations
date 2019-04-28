# This tests the dropping of unused features.
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

test_that("dropUnusedFeatures works with empty objects", {
    IR <- IndexedRelations(list(integer(0), integer(0), integer(0)), 
        featureSets=list(r1, r2, r3))
    IR2 <- dropUnusedFeatures(IR)
    expect_as_if(IR, IR2)

    expect_identical(unname(lengths(featureSets(IR2))), integer(3))
})
