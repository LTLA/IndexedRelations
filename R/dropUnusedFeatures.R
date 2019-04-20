#' Drop unused features
#'
#' Drop unused features or feature sets in an \linkS4class{IndexedRelations} object.
#' 
#' @param x An IndexedRelations object.
#'
#' @return An IndexedRelations object where any unused features (or unused feature sets) have been removed.
#'
#' @details
#' \code{dropUnusedFeatures} will remove features in each element of \code{featureSets(x)} that are not referred to by any partner in any relationship.
#' All partner indices are remapped to the appropriate locations in the subsetted feature sets.
#' The only features that remain will have at least one index pointing to it in \code{partners(x)}.
#' 
#' \code{dropUnusedFeatureSets} will remove feature sets that are not referred to by any partner.
#' This differs from \code{dropUnusedFeatures}, which will generate empty feature sets (but not remove them).
#' Complete dropping of unused features and their sets requires both of these functions to be called.
#' 
#' These functions can reduce the size of an IndexedRelations object, e.g., during serialization to file.
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
#' # Unused feature sets are removed completely.
#' rel2 <- IndexedRelations(list(1:2, 1:2), list(promoters, enhancers), 
#'     mapping=c(1L, 1L))
#' featureSets(rel2)
#' 
#' dropped2 <- dropUnusedFeatureSets(rel2)
#' featureSets(dropped2)
#' 
#' @export
#' @importFrom S4Vectors mendoapply
dropUnusedFeatures <- function(x) {
    all.features <- featureSets(x)
    f <- factor(mapping(x), levels=seq_along(all.features))

    all.partners <- partners(x)
    by.set <- split(as.list(all.partners), f, drop=FALSE)

    for (i in seq_along(by.set)) {
        keep <- as.integer(unique(unlist(by.set[[i]])))
        all.features[[i]] <- all.features[[i]][keep]
        by.set[[i]] <- keep
    }

    # Remapping the indices in the partners.
    for (i in seq_len(ncol(all.partners))) {
        m <- mapping(x)[i]
        all.partners[,i] <- match(all.partners[,i], by.set[[m]])
    }

    initialize(x, featureSets=all.features, partners=all.partners)
}

#' @export
dropUnusedFeatureSets <- function(x) {
    map <- mapping(x)
    keep <- unique(map)
    initialize(x, featureSets=featureSets(x)[keep], mapping=match(map, keep))
}
