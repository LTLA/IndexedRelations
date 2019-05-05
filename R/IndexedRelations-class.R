#' @title The IndexedRelations class 
#' 
#' @description
#' The IndexedRelations class is designed to represent relationships between feature sets (e.g., genes, genomic regions, proteins).
#' Each entry in a IndexedRelations object corresponds to a relationship between any number of \dQuote{partners} from different feature sets.
#' All entries in a single IndexedRelations object should have the same number, ordering and type of partners involved in the relationship.
#' 
#' This class aims to provide an efficiency boost in the case where one feature is a partner in multiple relationships.
#' Rather than storing the same feature multiple times, this class stores it once and simply refers to that feature in each relationship.
#' This reduces the memory usage and enables more efficient processing by algorithms that are aware of this redundancy.
#'
#' @section Constructor:
#' \code{IndexedRelations(x, featureSets=NULL)} will return a IndexedRelations object given a list or data.frame of partners in \code{x}.
#' 
#' Partners can be represented by any \linkS4class{Vector}-like data type in each element of \code{x}.
#' Parallel Vectors in \code{x} represent partners involved in a single relationship, i.e.,
#' the first element of \code{x[[1]]} is in a relationship with the first element of \code{x[[2]]} and so on.
#' Thus, all partner vectors in \code{x} should be of the same length.
#'
#' If \code{featureSets} is specified, this should contain a list of feature sets.
#' Again, any Vector-like data type is supported, but in this case, \code{x} should only contain integer indices.
#' This specifies the partners involved in each relationship by indexing the relevant feature set.
#' Each partner vector in \code{x} is assumed to point to the corresponding feature set in \code{featureSets},
#' i.e., indices in \code{x[[1]]} refer to the elements in \code{featuresSets[[1]]} and so on.
#' 
#' If \code{x} is a IndexedRelations object, it is returned without further modification.
#' 
#' @section Getter methods for partners:
#' In the following code snippets, \code{x} is a IndexedRelations object.
#' \describe{
#' \item{\code{partners(x)}:}{Returns a \linkS4class{DataFrame} of partner indices.
#' Each row represents a relationship and each column represents a partner.
#' The \code{i}th column contains an integer index that points to the corresponding feature set in \code{featureSets}.
#' }
#' \item{\code{partnerNames(x)}:}{Returns a character vector of names used for each parter in a relationship.
#' This may be \code{NULL} if the partners are not named.}
#' \item{\code{partnerFeatures(x, type)}:}{Returns the partners as features, by using the indices to subset the feature set.
#' \code{type} is an integer scalar specifying which partners to return.
#' Alternatively, it can be a string if the partners are named.}
#' \item{\code{npartners(x)}:}{Returns an integer scalar specifying the number of partners per relationship in \code{x}.}
#' }
#'
#' @section Other getters:
#' \code{featureSets(x)} returns a \linkS4class{List} of all feature sets,
#' given an IndexedRelations object \code{x}.
#' Each feature set corresponds to and is pointed at by the column of \code{partners(x)} with the same name.
#'
#' Getter methods applicable to \linkS4class{Vector} subclasses can be used here, 
#' e.g., \code{length}, \code{names}, \code{\link{mcols}}, \code{\link{metadata}}.
#' 
#' Element-wise metadata can be conveniently retrieved for an IndexedRelations object \code{x} with the field \code{name}
#' by using \code{x$name}.
#'
#' @section Setter methods for partners:
#' In the following code snippets, \code{x} is a IndexedRelations object.
#' \describe{
#' \item{\code{partners(x) <- value}:}{Replaces the partner indices with a \linkS4class{DataFrame} \code{value}.
#' \code{value} should have the same number of columns as \code{partners(x)},
#' and should contain integer indices that point to valid entries of the corresponding feature set.}
#' \item{\code{partnerNames(x) <- value}:}{Replaces the names used for each partner with \code{value}, a character vector or \code{NULL}.}
#' \item{\code{partnerFeatures(x, type, ...) <- value}:}{Replaces the partners specified by \code{type} with new features in \code{value}.
#' \code{type} can be an integer scalar or a string if the partners are named.
#' Values in \code{...} are ignored.}
#' }
#'
#' @section Other setter methods:
#' \code{featureSets(x) <- value} replaces the feature sets in an IndexedRelations instance \code{x} with the \linkS4class{List} \code{value}.
#' \code{value} should have the same length as the original \code{featureSets(x)}.
#'
#' Setter methods applicable to \linkS4class{Vector} subclasses are applicable to IndexedRelations instances,
#' e.g., \code{names<-}, \code{\link{mcols<-}}, \code{\link{metadata<-}}.
#'
#' Element-wise metadata can be conveniently set for an IndexedRelations object \code{x} with the field \code{name}
#' by using \code{x$name <- value}, where \code{value} is a vector-like object recycled to the length of \code{x}.
#'
#' @section Subsetting and combining:
#' An IndexedRelations instance behaves like a one-dimensional \linkS4class{Vector} during subsetting.
#' This will operate on the relations as vector elements.
#'
#' It is always possible to combine IndexedRelations objects with the same feature set classes,
#' even if the entries in the feature sets differ between objects.
#' In such cases, an IndexedRelations object will be produced where each feature set is a union of the corresponding sets in the input.
#' The same principle applies for subset assignment.
#' 
#' @section Miscellaneous:
#' \code{show(x)} will show information about an IndexedRelations \code{x}, including a preview of the first and last relationships.
#' Note that the feature classes must have methods implemented for \code{\link{showAsCell}} in order for correct display of the partner features.
#' 
#' @section Names and metadata in feature sets:
#' Particular functions may not preserve names and metadata in the feature set.
#' This occurs when an operation needs to consolidate two different feature sets into one,
#' e.g., when combining two IndexedRelations that do not have identical feature sets,
#' In such cases, a warning will be issued if information is likely to be lost such that the results are not as expected.
#' Users wanting to modify names or metadata of the feature sets should do so via the \code{featureSets<-} method.
#'
#' @author Aaron Lun
#' @examples
#' #####################
#' #### Constructor ####
#' #####################
#'
#' library(GenomicRanges)
#' promoters <- GRanges("chrA", IRanges(1:10*20, 1:10*20+10))
#' enhancers <- GRanges("chrA", IRanges(1:20*10, 1:20*10+10))
#'
#' # You can construct from supplying features:
#' partner1 <- sample(length(promoters), 100, replace=TRUE)
#' partner2 <- sample(length(enhancers), 100, replace=TRUE)
#' rel <- IndexedRelations(
#'     list(
#'         promoter=promoters[partner1],
#'         enhancer=enhancers[partner2]
#'     )
#' )
#' 
#' # But it's more efficient to just supply indices, where possible:
#' rel <- IndexedRelations(
#'     list(promoter=partner1, enhancer=partner2),
#'     featureSets=list(promoters=promoters, enhancers=enhancers)
#' )
#'
#' IndexedRelations(rel) # does nothing.
#' 
#' #################
#' #### Getters ####
#' #################
#'
#' partners(rel)
#' partnerNames(rel)
#' partnerFeatures(rel, "promoter")
#' partnerFeatures(rel, 1)
#' partnerNames(rel)
#' 
#' featureSets(rel)
#'
#' #################
#' #### Setters ####
#' #################
#' 
#' rel2 <- rel
#' partners(rel2)$promoter <- rev(partners(rel2)$promoter)
#' partners(rel2)
#'
#' partnerNames(rel2) <- c("P", "E")
#' partnerNames(rel2)
#'
#' partnerFeatures(rel2, "P") <- rev(partnerFeatures(rel2, "P")) # by feature
#' partners(rel2)
#'
#' featureSets(rel2)$P <- resize(featureSets(rel2)$P, width=25)
#' featureSets(rel2)$P
#' 
#' ##################################
#' #### Subsetting and combining ####
#' ##################################
#'
#' c(rel, rel)
#' rel[1:5]
#' rel3 <- rel
#' rel3[1:10] <- rel[10:1]
#' rel3
#' 
#' @name IndexedRelations
#' @docType class
#' @aliases IndexedRelations-class IndexedRelations
#' partners partners,IndexedRelations-method partners<- partners<-,IndexedRelations-method
#' partnerNames partnerNames,IndexedRelations-method partnerNames<- partnerNames<-,IndexedRelations-method
#' partnerFeatures partnerFeatures,IndexedRelations-method partnerFeatures<- partnerFeatures<-,IndexedRelations-method
#' npartners npartners,IndexedRelations-method
#' featureSets featureSets,IndexedRelations-method featureSets<- featureSets<-,IndexedRelations-method
#' parallelSlotNames,IndexedRelations-method
#' show,IndexedRelations-method
#' names,IndexedRelations-method names<-,IndexedRelations-method
#' $,IndexedRelations-method $<-,IndexedRelations-method
#' bindROWS,IndexedRelations-method
NULL

#' @export
#' @importFrom BiocGenerics unique match
#' @importFrom S4Vectors DataFrame
#' @importClassesFrom S4Vectors List
IndexedRelations <- function(x, featureSets=NULL) {
    if (is(x, "IndexedRelations")) {
        return(x)
    }

    if (is.null(featureSets)) {
        featureSets <- x
        for (i in seq_along(featureSets)) {
            current <- unique(x[[i]])
            featureSets[[i]] <- current
            x[[i]] <- match(x[[i]], current)
        }
    }

    x <- lapply(x, as.integer)
    x <- lapply(x, unname)
    if (is.null(names(x))) {
        names(x) <- sprintf("X.%i", seq_along(x))
    }
    x$check.names <- FALSE
    df <- do.call(DataFrame, x)

    new("IndexedRelations", partners=df, featureSets=as(featureSets, "List"))
}

.oob <- function(indices, N) {
    any(indices > N) || any(indices < 1L) || any(is.na(indices))
}

#' @importFrom S4Vectors setValidity2
setValidity2("IndexedRelations", function(object) {
    msg <- character(0)
    rlt <- partners(object)
    itr <- .featureSets(object)

    failed <- length(itr)!=ncol(rlt)
    if (failed) {
        msg <- c(msg, "number of partners and feature sets should be the same")
    }

    # Checking partners
    for (i in seq_len(ncol(rlt))) {
        current <- rlt[[i]]
        if (!is.integer(current)) {
            msg <- c(msg, sprintf("column %i of 'partners' is not integer", i))
        }

        if (!failed) {
            cur.store <- itr[[i]]
            if (.oob(current, NROW(cur.store))) {
                msg <- c(msg, sprintf("column %i of 'partners' contains out-of-bounds indices", i))
            }
        }
    }
 
    if (length(msg)) {
        return(msg)
    }
    return(TRUE) 
})

#################################
# Getters and setters: partners #
#################################

# An internal function with no overhead.
.partners <- function(x) x@partners

#' @export
setMethod("partners", "IndexedRelations", .partners)

#' @export
setMethod("partnerNames", "IndexedRelations", function(x) names(partners(x)))

#' @export
setMethod("npartners", "IndexedRelations", function(x) ncol(partners(x)))

#' @export
#' @importFrom S4Vectors extractROWS
setMethod("partnerFeatures", "IndexedRelations", function(x, type) {
    if (is.character(type)) {
        type <- match(type, partnerNames(x))
    }
    ids <- partners(x)[[type]]
    cur.store <- .featureSets(x)[[type]]
    extractROWS(cur.store, ids)
})

# An internal version with no overhead.
setReplaceMethod(".partners", "IndexedRelations", function(x, value) {
    x@partners <- value
    x
})

#' @export
#' @importClassesFrom S4Vectors DataFrame
setReplaceMethod("partners", "IndexedRelations", function(x, value) {
    .partners(x) <- as(value, "DataFrame")
    validObject(x)
    x
})

#' @export
setReplaceMethod("partnerNames", "IndexedRelations", function(x, value) {
    names(.partners(x)) <- value
    names(.featureSets(x)) <- value
    x
})

#' @importFrom BiocGenerics match
#' @importFrom S4Vectors mcols bindROWS
.combine_features <- function(incoming, ref) {
    if (identical(incoming, ref)) {
        m <- seq_along(incoming)
    } else {
        # Complaining if names might be compromised, if 'incoming'
        # contains names (discarded if they aren't added to 'ref')
        # or if 'ref' contains names (expands NULL names in 'incoming'
        # to empty characters, which isn't quite right).
        if (!is.null(names(incoming)) || !is.null(names(ref))) {
            warning("potential modification of names in reorganized feature sets")
        }

        # Same logic for the metadata fields.
        mi <- mcols(incoming)
        mr <- mcols(ref)
        if ((!is.null(mi) && ncol(mi) > 0L) || (!is.null(mr) && ncol(mr) > 0L)) {
            warning("potential modification of metadata in reorganized feature sets")
        }

        m <- match(incoming, ref)
        lost <- is.na(m)
        if (any(lost)) {
            lost.values <- incoming[lost]
            lost.ref <- unique(lost.values)
            m[lost] <- NROW(ref) + match(lost.values, lost.ref)

            # strictly appends, to avoid invaliding indices to 'ref'.
            ref <- bindROWS(ref, list(lost.ref))
        }
    }
    list(id=m, ref=ref)
}

#' @export
setReplaceMethod("partnerFeatures", "IndexedRelations", function(x, type, ..., value) {
    if (is.character(type)) {
        type <- match(type, partnerNames(x))
    }
    U <- unique(value)
    .featureSets(x)[[type]] <- U
    .partners(x)[[type]] <- match(value, U)
    x
})

####################################
# Getters and setters: featureSets #
####################################

# An internal version with no overhead.
.featureSets <- function(x) x@featureSets

#' @export
setMethod("featureSets", "IndexedRelations", function(x) {
    out <- .featureSets(x)
    names(out) <- partnerNames(x)
    out
})

# An internal version with no overhead.
setReplaceMethod(".featureSets", "IndexedRelations", function(x, value) {
    x@featureSets <- value
    x
})

#' @export
#' @importClassesFrom S4Vectors List
setReplaceMethod("featureSets", "IndexedRelations", function(x, value) {
    .featureSets(x) <- as(value, "List")
    validObject(x)
    x
})

##########################
# Getters and setters: $ #
##########################

#' @export
#' @importFrom S4Vectors mcols
setMethod("$", "IndexedRelations", function(x, name) mcols(x)[[name]])

#' @export
#' @importFrom S4Vectors mcols<-
setReplaceMethod("$", "IndexedRelations", function(x, name, value) {
    mcols(x)[[name]] <- value
    x
})

################################
# Subset and combining methods #
################################

#' @export
#' @importFrom S4Vectors parallelSlotNames
setMethod("parallelSlotNames", "IndexedRelations", function(x) 
    c("partners", callNextMethod()) # Handles [, length, mcols synchronization.
)

#' @export
#' @importFrom S4Vectors bindROWS
setMethod("bindROWS", "IndexedRelations", function(x, objects = list(), use.names = TRUE, ignore.mcols = FALSE, check = TRUE) {
    output <- standardizeFeatureSets(x, objects)
    x <- output$x
    objects <- output$objects
    callNextMethod()
})

#################
# Miscellaneous #
#################

#' @export
setMethod("names", "IndexedRelations", function(x) rownames(partners(x)))

#' @export
setReplaceMethod("names", "IndexedRelations", function(x, value) {
    rownames(.partners(x)) <- value
    x
})
