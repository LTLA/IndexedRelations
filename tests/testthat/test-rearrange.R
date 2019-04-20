# This tests the rearrangePartners() function.
# library(testthat); library(IndexedRelations); source("setup.R"); source("test-rearrange.R")

r1 <- random_ranges(20)
r2 <- random_ranges(10)
r3 <- random_ranges(40)

i1 <- sample(length(r1), 100, replace=TRUE)
i2 <- sample(length(r2), 100, replace=TRUE)
i3 <- sample(length(r3), 100, replace=TRUE)

test_that("rearrangePartners works correctly", {
    IR <- IndexedRelations(list(A=i1, B=i2, C=i3), featureSets=list(r1, r2, r3))

    expect_identical(rearrangePartners(IR, c(3,2,1)),
        IndexedRelations(list(C=i3, B=i2, A=i1), featureSets=list(r1, r2, r3), mapping=3:1))
        
    expect_identical(rearrangePartners(IR, c(1,1,2)),
        IndexedRelations(list(A=i1, A=i1, B=i2), featureSets=list(r1, r2, r3), mapping=c(1L,1L,2L)))

    expect_identical(rearrangePartners(IR, 1),
        IndexedRelations(list(A=i1), featureSets=list(r1, r2, r3), mapping=c(1L)))

    expect_error(rearrangePartners(IR, 5), "out-of-bounds")
    expect_error(rearrangePartners(IR, 0), "out-of-bounds")
    expect_error(rearrangePartners(IR, NA), "out-of-bounds")
})
