# This tests the comparison capabilities of IndexedRelations.
# library(testthat); library(IndexedRelations); source("setup.R"); source("test-compare.R")

set.seed(29999)
r1 <- random_ranges(50)
r2 <- random_ranges(100)
r3 <- random_ranges(20)

N <- 100
i1 <- sample(length(r1), N, replace=TRUE)
i2 <- sample(length(r2), N, replace=TRUE)
i3 <- sample(length(r3), N, replace=TRUE)

############
# pcompare #
############

REF_pcompare <- function(x, y) {
    output <- integer(max(length(x), length(y)))
    for (i in seq_len(ncol(partners(x)))) {
        current <- pcompare(partner(x, i), partner(y, i))
        undecided <- output==0L
        output[undecided] <- current[undecided]
    }
    output
}

test_that("pcompare works correctly", {
    # Same feature sets. 
    IR <- IndexedRelations(list(i1, i2, i3), list(r1, r2, r3))
    expect_identical(pcompare(IR, IR), integer(N))

    IR2 <- IR[sample(length(IR))]
    expect_identical(sign(pcompare(IR, IR2)), sign(REF_pcompare(IR, IR2)))

    # Different feature sets.
    IR3 <- IndexedRelations(list(i3, i2, i1), list(r3, r2, r1))
    expect_identical(sign(pcompare(IR, IR3)), sign(REF_pcompare(IR, IR3)))

    block <- random_ranges(10)
    IRx <- IndexedRelations(list(i1+10, i2+10, i3+10), 
        list(c(block, r1), c(block, r2), c(block, r3)))
    expect_identical(pcompare(IRx, IR), integer(N))

    # Recycles properly.
    X <- pcompare(IR[1], IR3)
    expect_identical(length(X), as.integer(N))
    expect_identical(sign(X), sign(REF_pcompare(IR[1], IR3)))

    expect_identical(pcompare(IR[0], IR3), integer(0))
})

#########
# match #
#########

test_that("match works correctly in basic scenarios", {
    # Same feature sets. 
    IR <- IndexedRelations(list(i1, i2, i3), list(r1, r2, r3))
    s <- sample(N)
    IR2 <- IR[s]

    m <- match(IR2, IR)
    expect_false(any(is.na(m)))
    expect_identical(IR2==IR[m], !logical(N))

    # Different feature sets.
    block <- random_ranges(10)
    s <- sample(N)
    IR3 <- IndexedRelations(list(i1[s]+10, i2[s]+10, i3[s]+10), 
        list(c(block, r1), c(block, r2), c(block, r3)))

    m <- match(IR, IR3)
    expect_false(any(is.na(m)))
    expect_identical(IR==IR3[m], !logical(N))

    # Handles empty inputs.
    expect_identical(match(IR[0], IR), integer(0))
    expect_identical(match(IR[0], IR[0]), integer(0))
})

test_that("match yields NAs when necessary", {
    IR <- IndexedRelations(list(i1, i2, i3), list(r1, r2, r3))

    # Simple case with unique elements.
    IRu <- unique(IR)
    m <- match(IRu, IRu[1:10])
    expect_identical(m, c(seq_len(10), rep(NA_integer_, N-10)))

    # More complex case with modified features to guarantee feature set differences.
    modfun <- function(x) resize(x, width(x) + 1) 
    IR2 <- IndexedRelations(list(i1[1:10], i2[1:10], i3[1:10]), list(modfun(r1), modfun(r2), modfun(r3)))

    combined <- c(IR2, IR)
    m <- match(combined, IR)
    expect_identical(m, c(rep(NA_integer_, 10), match(IR, IR)))

    # Checking resistance to permutation.
    s <- sample(length(combined))
    m2 <- match(combined[s], IR)
    expect_identical(m2, m[s])

    # Handles empty inputs.
    expect_identical(match(IR, IR[0]), rep(NA_integer_, N))
})

test_that("match works correctly with lots of ties", {
    IR <- IndexedRelations(list(i1, i2, i3), list(r1, r2, r3))
    
    # Ties in 'x'.
    mult.x <- sample(length(IR), length(IR)*2, replace=TRUE)
    m <- match(IR[mult.x], IR)
    expect_identical(m, match(IR, IR)[mult.x])

    # Ties in 'table'.
    mult.t <- sample(length(IR), length(IR)*2, replace=TRUE)
    mult.IR <- IR[mult.t]
    m <- match(IR, mult.IR)

    not.dup <- !duplicated(mult.IR)
    IR2 <- mult.IR[not.dup]
    m2 <- match(IR, IR2)
    expect_identical(m, which(not.dup)[m2])
})

test_that("selfmatch works correctly", {
    IR <- IndexedRelations(list(1:10, 1:10, 1:10), list(unique(r1), unique(r2), unique(r3)))
    expect_identical(selfmatch(IR), seq_len(10))

    IR2 <- c(IR, IR)
    expect_identical(selfmatch(IR2), rep(seq_len(10), 2))

    s <- sample(length(IR))
    IR3 <- c(IR[s], IR[2:5])
    expect_identical(selfmatch(IR3), c(seq_len(10), match(2:5, s)))
})

############
# ordering #
############

REF_order <- function(...) {
    objects <- list(...)
    for (i in seq_along(objects)) {
        x <- objects[[i]]
        objects[[i]] <- lapply(seq_len(ncol(partners(x))), function(i) partner(x, i))
    }
    objects <- unlist(objects, recursive=FALSE)
    do.call(order, objects)
}

test_that("ordering works correctly", {
    IR <- IndexedRelations(list(i1, i2, i3), list(r1, r2, r3))
    expect_identical(order(IR), REF_order(IR))

    IR2 <- IR[sample(5, N, replace=TRUE)]
    expect_identical(order(IR2, IR), REF_order(IR2, IR))

    # Behaves with zero-length inputs.
    expect_identical(order(IR[0]), integer(0))
})
