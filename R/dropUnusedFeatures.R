#' Drop unused features
#'
#' Drop unused features in an \linkS4class{IndexedRelations} object.
#' 
#' @param x An IndexedRelations object.
#'
#' @return An IndexedRelations object where any unused features have been removed.
#'
#' @details
#' \code{dropUnusedFeatures} will remove features in each element of \code{featureSets(x)} that are not referred to by any partner in any relationship.
#' All partner indices are remapped to the appropriate locations in the subsetted feature sets.
#' The only features that remain will have at least one index pointing to it in \code{partners(x)}.
#' 
#' This function can reduce the size of an IndexedRelations object, e.g., during serialization to file.
#' However, it is usually unnecessary to run these during the course of an analysis.
#' R operates on a copy-on-modify principle, so the same feature set(s) across multiple IndexedRelations objects do not occupy additional memory.
#' Dropping unused features requires construction of instance-specific feature sets that may increase memory usage.
#' 
#' @author Aaron Lun
#' @examples
#' library(GenomicRanges)
#' promoters <- GRanges("chrA", IRanges(1:10*20, 1:10*20+10))
#' enhancers <- GRanges("chrA", IRanges(1:20*10, 1:20*10+10))
#'
#' rel <- IndexedRelations(list(1:2, 1:2), list(promoters, enhancers))
#' lengths(featureSets(rel))
#'
#' # Only the first two entries of each feature set are retained.
#' dropped <- dropUnusedFeatures(rel)
#' lengths(featureSets(dropped))
#' 
#' @export
dropUnusedFeatures <- function(x) {
    all.features <- featureSets(x)
    all.partners <- partners(x)

    # Remapping the indices in the partners.
    for (i in seq_len(ncol(all.partners))) {
        keep <- unique(all.partners[,i])
        all.partners[,i] <- match(all.partners[,i], keep)
        all.features[[i]] <- all.features[[i]][keep]
    }

    initialize(x, featureSets=all.features, partners=all.partners)
}
