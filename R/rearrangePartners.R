#' Rearrange partners
#' 
#' Rearrange partners in an \linkS4class{IndexedRelations} object.
#'
#' @param x An IndexedRelations object.
#' @param perm An integer vector specifying the permutation of the partners.
#'
#' @return An IndexedRelations object with rearranged partners.
#'
#' @details
#' This function facilitates the task of switching the order of partners across all relationships in \code{x}.
#' Note that the length of \code{perm} need not be the as \code{ncol(partners(x))};
#' partners can be dropped, duplicated, etc.
#'
#' @author Aaron Lun 
#' @examples
#' library(GenomicRanges)
#' genomic <- GRanges("chrA", IRanges(1:10*20, 1:10*20+10))
#' intervals <- IRanges(1:20*10, 1:20*10+10)
#'
#' rel <- IndexedRelations(list(1:2, 1:2), list(genomic, intervals))
#' 
#' rearrangePartners(rel, c(2, 1))
#'
#' # Or even drop a partner completely:
#' rearrangePartners(rel, 2)
#' 
#' @export
rearrangePartners <- function(x, perm) {
    perm <- as.integer(perm)
    N <- ncol(partners(x))
    if (!all(!is.na(perm) & perm > 0L & perm <= N)) {
        stop("'perm' contains out-of-bounds values")
    }
    initialize(x, partners=partners(x)[,perm,drop=FALSE], mapping=mapping(x)[perm])
}
