#' IndexedRelation conversions
#'
#' Convert an IndexedRelations object to and from different Bioconductor classes.
#'
#' @details
#' A number of \code{coerce} methods are available to convert to/from an \linkS4class{IndexedRelations} object:
#' \itemize{
#' \item \code{as(from, "IndexedRelations")} for a \linkS4class{Pairs} \code{x} will return an IndexedRelations object where the first vector in \code{from} becomes the first partner in the output.
#' \item \code{as(from, "IndexedRelations")} for a \linkS4class{DataFrame} \code{x} will return an IndexedRelations object where the first column in \code{from} becomes the first partner in the output.
#' \item \code{as(from, "DataFrame")} for an IndexedRelations \code{x} will return a DataFrame object where the first column is the first partner in \code{From}.
#' }
#'
#' In addition, \code{makePairsFromIndexedRelations(x)} will create a Pairs instance from an IndexedRelations \code{x}.
#' This requires that \code{x} only has two partners in each relationship, otherwise an error is thrown.
#'
#' @author Aaron Lun
#' @examples
#' p <- Pairs(1:10, Rle(1L, 10), score=rnorm(10), names=letters[1:10])
#'
#' ir <- as(p, "IndexedRelations")
#' ir
#'
#' back <- makePairsFromIndexedRelations(ir)
#' back
#'
#' df <- as(ir, "DataFrame")
#' df
#' 
#' as(df, "IndexedRelations")
#' 
#'
#' @docType methods 
#' @name IR conversions
#' @rdname conversions
#' @aliases makePairsFromIndexedRelations
#' @aliases coerce,Pairs,IndexedRelations-method coerce,DataFrame,IndexedRelations-method coerce,IndexedRelations,DataFrame-method
NULL

#' @export
#' @importFrom S4Vectors Pairs mcols
makePairsFromIndexedRelations <- function(x) {
    if (ncol(partners(x))!=2) {
        stop("'x' must have two partners to create Pairs")
    }

    left <- partner(x, 1)
    right <- partner(x, 2)

    meta <- mcols(x)
    if (is.null(meta)) meta <- new("DataFrame", nrows=length(x))

    Pairs(left, right, names=names(x), meta)
}

#' @importClassesFrom S4Vectors Pairs
#' @importFrom S4Vectors first second
setAs("Pairs", "IndexedRelations", function(from) {
    IndexedRelations(list(first=first(p), second=second(p)))
})

#' @importClassesFrom S4Vectors DataFrame
setAs("DataFrame", "IndexedRelations", function(from) {
    IndexedRelations(from)
})

#' @importClassesFrom S4Vectors DataFrame
#' @importFrom S4Vectors DataFrame
setAs("IndexedRelations", "DataFrame", function(from) {
    output <- vector("list", ncol(partners(from)))
    for (i in seq_along(output)) {
        output[[i]] <- I(partner(from, i))
    }
    names(output) <- partnerNames(from)
    do.call(DataFrame, output)
})