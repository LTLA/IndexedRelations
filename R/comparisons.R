#' Compare IndexedRelations objects.
#'
#' Perform comparisons within or between IndexedRelations objects.
#' This behaves \dQuote{as if} the partnering features were directly represented in the object,
#' i.e., comparisons are based on the features, not on the values of the integer indices.
#' This is an important distinction if different objects have different feature sets,
#' and/or if the feature sets contain duplicated entries.
#'
#' @section Element-wise comparisons:
#' \code{pcompare(x, y)} for two IndexedRelations \code{x} and \code{y} will return an integer vector of length equal to the longer of \code{x} and \code{y}.
#' This contains negative, zero or positive values if the entry in \code{x} is \dQuote{less than}, equal to or \dQuote{greater than} the corresponding entry of \code{y}, respectively.
#' Entries from the shorter object are recycled to reach the length of the longer object; unless one is of zero length, in which case an empty integer vector is returned.
#' 
#' The definition of \dQuote{less than}, etc., is based on comparisons between partners between \code{x} and \code{y}.
#' The first partners (i.e., \code{partner(x, 1)}) are compared; of those that are equal, the second partners are compared; and so on.
#' The definition of equality for partner comparisons are based on the equivalent definition for the partner's feature class. 
#' For example, if the first feature set was a \linkS4class{IRanges} object, intervals with smaller start positions would be considered to be less than other intervals.
#'
#' @section Matching:
#' \code{match(x, table, nomatch = NA_integer_, incomparables = NULL, ...)} takes two IndexedRelations objects \code{x} and \code{table}.
#' It returns an integer vector of length equal to \code{x}, specifying the first entry of \code{table} that is equal to each element of \code{x}.
#' Elements of \code{x} without a match are assigned values of \code{nomatch}.
#' All other arguments are ignored.
#'
#' \code{selfmatch(x, ...)} takes an IndexedRelations object \code{x} and returns an integer vector specifying the first entry of \code{x} that is equal to each entry of \code{x}.
#' This represents a more efficient specialization of \code{match} when \code{table} is directly equal to \code{x}.
#' All other arguments are ignored.
#'
#' In both cases, equality is based on the features rather than the indices of \code{x} and \code{table}.
#'
#' @section Ordering:
#' \code{order(..., na.last=TRUE, decreasing=FALSE, method=c("auto", "shell", "radix"))} will take any number of IndexedRelations objects of the same length in \code{...},
#' and return an integer vector specifying the permutation required to order the first object.
#' (For entries of the first object that are equal, the permutation will order them based on the second object, and so on.)
#' 
#' Ordering within each IndexedRelations object is performed based on the first partner, then on the second partner for entries that have the same first partner, and so on.
#' Ordering of partners is based on the definition of sorting for that partner's feature class.
#' 
#' @author Aaron Lun
#' @seealso \code{\link{pcompare}}, to see the specification for \linkS4class{Vector} classes.
#'
#' @examples
#' library(IRanges)
#' r1 <- IRanges(1:10, width=1)
#' r2 <- IRanges(1:5, width=2)
#' i1 <- sample(length(r1), 20, replace=TRUE) 
#' i2 <- sample(length(r2), 20, replace=TRUE) 
#' 
#' IR <- IndexedRelations(list(i1, i2), list(r1, r2))
#' IR2 <- IndexedRelations(list(rev(i1), rev(i2)), list(r1, r2))
#'
#' IR < IR2
#' IR > IR2
#' IR == IR2
#'
#' match(IR, IR2)
#' IR[1:10] %in% IR # based on 'match'
#'
#' selfmatch(IR)
#' duplicated(IR) # based on 'selfmatch'
#' unique(IR) # based on 'duplicated'
#' 
#' order(IR)
#' sort(IR) # based on 'order'
#'
#' @docType methods
#' @rdname comparisons
#' @name comparisons
#' @aliases pcompare,IndexedRelations,IndexedRelations-method
#' @aliases match,IndexedRelations,IndexedRelations-method selfmatch,IndexedRelations-method
#' @aliases order,IndexedRelations-method
NULL

#' @export
#' @importFrom S4Vectors pcompare
setMethod("pcompare", c("IndexedRelations", "IndexedRelations"), function(x, y) {
    if (length(x)==0L || length(y)==0L) {
        return(integer(0))
    }

    std.feat <- standardizeFeatureSets(x, list(y), clean=TRUE)
    x <- std.feat$x
    y <- std.feat$objects[[1]]

    output <- integer(max(length(x), length(y)))
    px <- partners(x)
    py <- partners(y)

    for (i in seq_len(ncol(px))) {
        current <- px[,i] - py[,i] # recycles.
        undecided <- output==0L
        output[undecided] <- current[undecided]
    }

    output
})

#' @export
#' @importFrom BiocGenerics match
setMethod("match", c("IndexedRelations", "IndexedRelations"), function(x, table, nomatch = NA_integer_, incomparables = NULL, ...) {
    std.feat <- standardizeFeatureSets(x, list(table), clean=TRUE)
    x <- std.feat$x
    table <- std.feat$objects[[1]]
    combined <- rbind(partners(x), partners(table))

    # 'all.origins' ensures that the 'table' entry is first if any entries of 'x' are equal,
    # and that the sort is stable with respect to elements within 'table'.
    all.origins <- c(rep(NA_integer_, length(x)), seq_along(table))
    o <- do.call(order, c(as.list(combined), list(all.origins)))
    combined <- combined[o,]
    all.origins <- all.origins[o]

    is.unique <- Reduce("|", lapply(combined, FUN=function(x) c(TRUE, diff(x)>0L)))
    has.origin <- cumsum(is.unique)
    has.origin <- all.origins[is.unique][has.origin]
    has.origin[o] <- has.origin

    output <- has.origin[seq_along(x)]
    output[is.na(output)] <- nomatch
    output
})

#' @export
#' @importFrom S4Vectors selfmatch
setMethod("selfmatch", "IndexedRelations", function(x, ...) {
    x <- cleanFeatureSets(x)
    pset <- partners(x)

    # Additional all.origins to ensure that ordering is stable.
    all.origins <- seq_along(x)
    o <- do.call(order, c(as.list(pset), list(all.origins)))
    all.origins <- all.origins[o]
    pset <- pset[o,]

    is.unique <- Reduce("|", lapply(pset, FUN=function(x) c(TRUE, diff(x)>0L)))
    has.origin <- cumsum(is.unique)
    has.origin <- all.origins[is.unique][has.origin]
    has.origin[o] <- has.origin

    has.origin
})

#' @export
#' @importFrom BiocGenerics order
setMethod("order", "IndexedRelations", function(..., na.last=TRUE, decreasing=FALSE, method=c("auto", "shell", "radix")) {
    objects <- list(...)
    objects <- lapply(objects, cleanFeatureSets)
    all.partners <- lapply(objects, partners)
    all.partners <- unlist(lapply(all.partners, as.list), recursive=FALSE)
    do.call(order, c(all.partners, list(na.last=na.last, decreasing=decreasing, method=method)))
})
