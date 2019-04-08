# This tests all functionality with a non-trivial mapping.
# library(testthat); library(IndexedRelations); source("setup.R"); source("test-mapped.R")

regions <- random_ranges(20)
i1 <- sample(length(regions), 100, replace=TRUE)
i2 <- sample(length(regions), 100, replace=TRUE)
i3 <- sample(length(regions), 100, replace=TRUE)

#################
# Basic methods #
#################

test_that("Getters work as expected on a singleton", {
    IR <- IndexedRelations(list(i1, i2, i3), featureSets=list(regions), mapping=c(1L,1L,1L))
    expect_identical(partner(IR, 1), regions[i1])
    expect_identical(partner(IR, 2), regions[i2])
    expect_identical(partner(IR, 3), regions[i3])

    expect_identical(length(featureSets(IR)), 1L)
    expect_identical(featureSets(IR)[[1]], regions)

    expect_identical(mapping(IR), rep(1L, 3))
})

test_that("Setters work as expected on a singleton", {
    IR <- IndexedRelations(list(i1, i2, i3), featureSets=list(regions), mapping=c(1L,1L,1L))

    # Replacing IDs.
    partner(IR, 1, id=TRUE) <- rev(i1)
    expect_identical(partner(IR, 1), regions[rev(i1)])
    expect_identical(partner(IR, 2), regions[i2])
    expect_identical(partner(IR, 3), regions[i3])

    # Replacing regions.
    new.regions <- random_ranges(100)
    partner(IR, 1) <- new.regions
    expect_identical(partner(IR, 1), new.regions)
    expect_identical(partner(IR, 2), regions[i2])
    expect_identical(partner(IR, 3), regions[i3])

    expect_identical(featureSets(IR)[[1]], unique(c(regions, new.regions)))

    # Replacing features.
    IR <- IndexedRelations(list(i1, i2, i3), featureSets=list(regions), mapping=c(1L,1L,1L))
    featureSets(IR)[[1]] <- new.regions
    expect_identical(partner(IR, 1), new.regions[i1])
    expect_identical(partner(IR, 2), new.regions[i2])
    expect_identical(partner(IR, 3), new.regions[i3])
})

test_that("Subsetting and combining work correctly", {
    # Checking out the subsetting.          
    IR <- IndexedRelations(list(i1, i2, i3), featureSets=list(regions), mapping=c(1L,1L,1L))
    chosen <- sample(length(IR), 10)
    IRs <- IR[chosen]

    expect_identical(partners(IRs), partners(IR)[chosen,])
    expect_identical(featureSets(IRs), featureSets(IR))

    # Checking out the combining.
    IR2 <- c(IR, IRs) 
    expect_identical(partners(IR2), rbind(partners(IR), partners(IRs)))
    expect_identical(featureSets(IR2), featureSets(IR))

    IRx <- IRs
    featureSets(IRx)[[1]] <- resize(regions, max(width(regions))+1) # force creations of new regions.
    IR3 <- c(IR, IRx) 
    expect_identical(length(featureSets(IR3)), 1L)
    expect_identical(featureSets(IR3)[[1]], c(regions, featureSets(IRx)[[1]]))

    for (i in 1:3) {
        expect_identical(partner(IR3, i),  c(partner(IR, i), partner(IRx, i)))
    }

    # Checking out the subset assignment.
    IRa <- IR
    IRa[chosen] <- rev(IRs)
    expect_identical(featureSets(IRa), featureSets(IR))
    pset <- partners(IR)
    pset[rev(chosen),] <- partners(IRs)
    expect_identical(pset, partners(IRa))

    IRa <- IR
    IRa[chosen] <- IRx
    expect_identical(length(featureSets(IR3)), 1L)
    expect_identical(featureSets(IR3)[[1]], c(regions, featureSets(IRx)[[1]]))

    for (i in 1:3) {
        P <- partner(IR, i)
        P[chosen] <- partner(IRx, i)
        expect_identical(partner(IRa, i), P)
    }
})

######################
# Comparison methods #
######################

test_that("pcompare works correctly", {
    # Same region.
    IR <- IndexedRelations(list(i1, i2, i3), featureSets=list(regions), mapping=c(1L,1L,1L))
    expect_true(all(IR==IR))

    ref <- IndexedRelations(list(i1, i2, i3), featureSets=list(regions, regions, regions))
    s <- sample(length(IR))
    expect_identical(pcompare(ref, ref[s]), pcompare(IR, IR[s]))

    # Different sets of regions.
    r2 <- random_ranges(50)
    IR2 <- IndexedRelations(list(i1, i2, i3), featureSets=list(r2), mapping=c(1L,1L,1L))
    ref2 <- IndexedRelations(list(i1, i2, i3), featureSets=list(r2, r2, r2))

    expect_identical(pcompare(ref, ref2), pcompare(IR, IR2))
})

test_that("match works correctly", {
    # Same region.
    IR <- IndexedRelations(list(i1, i2, i3), featureSets=list(regions), mapping=c(1L,1L,1L))
    ref <- IndexedRelations(list(i1, i2, i3), featureSets=list(regions, regions, regions))

    s <- sample(length(IR))
    expect_identical(match(ref, ref[s]), match(IR, IR[s]))
    expect_identical(match(ref, ref[1:10]), match(IR, IR[1:10]))

    # Different sets of regions.
    r2 <- random_ranges(50)
    IR2 <- IndexedRelations(list(i1, i2, i3), featureSets=list(r2), mapping=c(1L,1L,1L))
    ref2 <- IndexedRelations(list(i1, i2, i3), featureSets=list(r2, r2, r2))

    s2 <- sample(length(IR) + length(IR2))
    expect_identical(match(ref, c(ref, ref2)[s2]), match(IR, c(IR, IR2)[s2]))

    # Self matching.
    IRs <- IndexedRelations(list(1:10, 1:10, 1:10), featureSets=list(unique(regions)), mapping=c(1L,1L,1L))
    expect_identical(selfmatch(IRs), 1:10)
    expect_identical(selfmatch(c(IRs, IRs)), c(1:10, 1:10))
})

test_that("ordering works correctly", {
    IR <- IndexedRelations(list(i1, i2, i3), featureSets=list(regions), mapping=c(1L,1L,1L))
    expect_identical(order(IR), order(regions[i1], regions[i2], regions[i3]))
})
