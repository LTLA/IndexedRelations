#' Conversion to Pairs
#'
#' Convert an IndexedRelations object from a Pairs object, and vice versa.
#'
#' @param x A \linkS4class{IndexedRelations} object with two partners.
#' @param from A \linkS4class{Pairs} object.
#' 
#' @return
#' \code{makePairsFromIndexedRelations} will return a Pairs object where the first partner in \code{x} is the first vector in the output.
#' 
#' The \code{as} method will return an IndexedRelations object where the first vector in \code{from} becomes the first partner in the output.
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
