#' Standardize feature sets
#' 
#' Standardize feature sets across comparable \linkS4class{IndexedRelations} objects.
#'
#' @param x An IndexedRelations object.
#' @param objects A list of IndexedRelations object with the same number and classes of partners.
#' @param clean Logical scalar indicating whether the each of the feature sets should contain sorted and unique entries.
#' 
#' @details
#' This function is intended for use by developers of downstream packages.
#' The idea is to standardize the feature sets of different IndexedRelations objects,
#' thereby allowing direct comparison of the integer indices in downstream algorithms.
#'
#' Setting \code{clean=TRUE} will sort and remove duplicates within each feature set.
#' This provides some structure that can be useful in some algorithms, 
#' e.g., ordering of partner indices reflects the ordering of the partner features.
#'
#' In all cases, the \dQuote{as if} behaviour of the IndexedRelations class is unaffected,
#' i.e., the partner features are the same before and after running this function, 
#' even if the underlying representation is altered.
#' 
#' @return A list containing \code{x} an IndexedRelations object;
#' and \code{objects}, a list of IndexedRelations objects with the same feature sets as \code{x}.
#'
#' @author Aaron Lun
#' \code{\link{cleanFeatureSets}}, to clean the feature sets for a single IndexedRelations object.
#' 
#' @examples
#' library(GenomicRanges)
#' promoters <- GRanges("chrA", IRanges(1:10*20, 1:10*20+10))
#' enhancers <- GRanges("chrA", IRanges(1:20*10, 1:20*10+10))
#' partner1 <- sample(length(promoters), 100, replace=TRUE)
#' partner2 <- sample(length(enhancers), 100, replace=TRUE)
#'
#' rel1 <- IndexedRelations(
#'     list(promoter=partner1, enhancer=partner2),
#'     featureSets=list(promoters=promoters, enhancers=enhancers)
#' )
#' rel2 <- IndexedRelations(
#'     list(promoter=partner1, enhancer=partner2),
#'     featureSets=list(promoters=rev(promoters), enhancers=rev(enhancers))
#' )
#'
#' std.feat <- standardizeFeatureSets(rel1, list(rel2))
#' new.rel1 <- std.feat$x
#' new.rel2 <- std.feat$objects[[1]]
#' stopifnot(identical(featureSets(new.rel1), featureSets(new.rel2)))
#' 
#' @export
standardizeFeatureSets <- function(x, objects, clean=FALSE) 
# Utility to standardize the featureSets of different IndexedRelations objects.
# This allows them to be compared, merged, etc.
{
    # Checking that objects are even mergeable.
    FUN <- function(z) unname(lapply(featureSets(z), class))
    ref.fcls <- FUN(x)
    obj.fcls <- lapply(objects, FUN)
    if (!all(vapply(obj.fcls, identical, y=ref.fcls, FUN.VALUE=TRUE))) {
        stop("feature sets are not comparable across objects")
    }

    # Reindexing the features to the same set.
    ref.features <- .featureSets(x)
    for (i in seq_along(objects)) {
        cur.features <- .featureSets(objects[[i]])
        if (identical(cur.features, ref.features)) {
            next
        }

        remap <- vector("list", length(cur.features))
        for (j in seq_along(remap)) {
            out <- .combine_features(cur.features[[j]], ref.features[[j]])
            ref.features[[j]] <- out$ref
            remap[[j]] <- out$id
        }

        objects[[i]] <- .reindex_partners(objects[[i]], remap)
    }

    # Forcing features to be unique and ordered,
    # which simplifies downstream calculations.
    if (clean) {
        remap <- vector("list", length(ref.features))
        for (j in seq_along(remap)) {
            so <- .sort_and_order(ref.features[[j]])
            ref.features[[j]] <- so$features
            remap[[j]] <- so$remap
        }

        x <- .reindex_partners(x, remap)
        for (i in seq_along(objects)) {
            objects[[i]] <- .reindex_partners(objects[[i]], remap)
        } 
    }

    .featureSets(x) <- ref.features
    for (i in seq_along(objects)) {
        .featureSets(objects[[i]]) <- ref.features
    }

    list(x=x, objects=objects)
}

.reindex_partners <- function(x, remap) {
    cur.partners <- partners(x)
    for (k in seq_len(ncol(cur.partners))) {
        cur.partners[,k] <- remap[[k]][cur.partners[,k]]
    }
    .partners(x) <- cur.partners
    x
}

#' @importFrom S4Vectors order extractROWS
#' @importFrom BiocGenerics duplicated
.sort_and_order <- function(features) {
    o <- order(features)
    features <- extractROWS(features, o)
    is.unique <- !duplicated(features)
    features <- extractROWS(features, is.unique)
    remap <- cumsum(is.unique)
    remap[o] <- remap
    list(remap=remap, features=features)
}

#' Clean feature sets
#' 
#' Clean feature sets within a \linkS4class{IndexedRelations} object.
#'
#' @param x An IndexedRelations object.
#' 
#' @details
#' This function will sort and remove duplicates within each feature set.
#' This provides some structure that can be useful in some algorithms, 
#' e.g., ordering of partner indices reflects the ordering of the partner features.
#' 
#' @return An IndexedRelations object with unique and sorted feature sets.
#'
#' @author Aaron Lun
#' @seealso \code{\link{standardizeFeatureSets}}, to do the same thing with \code{clean=TRUE}.
#'
#' @examples
#' library(GenomicRanges)
#' promoters <- GRanges("chrA", IRanges(1:10*20, 1:10*20+10))
#' enhancers <- GRanges("chrA", IRanges(1:20*10, 1:20*10+10))
#' partner1 <- sample(length(promoters), 100, replace=TRUE)
#' partner2 <- sample(length(enhancers), 100, replace=TRUE)
#'
#' rel <- IndexedRelations(
#'     list(promoter=partner1, enhancer=partner2),
#'     featureSets=list(promoters=promoters, enhancers=enhancers)
#' )
#' rel <- cleanFeatureSets(rel)
#'
#' featureSets(rel)[[1]]
#' featureSets(rel)[[2]]
#'
#' @export
cleanFeatureSets <- function(x) {
    fsets <- .featureSets(x)
    remap <- vector("list", length(fsets))

    for (i in seq_along(fsets)) {
        so <- .sort_and_order(fsets[[i]])
        fsets[[i]] <- so$features
        remap[[i]] <- so$remap
    }

    x <- .reindex_partners(x, remap)
    .featureSets(x) <- fsets
    x
}

