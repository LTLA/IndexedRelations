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
#' \code{IndexedRelations(x, features=NULL)} will return a IndexedRelations object given a list or data.frame of features in \code{x}.
#' Features can be represented by any \linkS4class{Vector}-like data type in each element of \code{x}.
#' Parallel elements of \code{x} represent partners involved in a single relationship, i.e.,
#' the first element of \code{x[[1]]} is in a relationship with the first element of \code{x[[2]]} and so on.
#' Thus, all elements of \code{x} should be of the same length.
#'
#' If \code{features} is specified, this should contain a list of feature sets for each type of feature.
#' In this case, \code{x} should only contain integer indices.
#' This specifies the partners involved in each relationship by indexing the relevant feature set.
#' Each element of \code{x} is assumed to point to the corresponding element of \code{features},
#' i.e., the first element of \code{x} contains indices to the first element of \code{features} and so on.
#' 
#' If \code{x} is a IndexedRelations object, it is returned without further modification.
#' 
#' @section Getter methods:
#' In the following code snippets, \code{x} is a IndexedRelations object.
#' \describe{
#' \item{\code{partners(x)}:}{Returns a \linkS4class{DataFrame} of partner indices.
#' Each row represents a relationship and each column represents a partner.
#' The \code{i}th column contains an integer index that points to a feature set defined by \code{mapping(x)[i]}.
#' }
#' \item{\code{partnerNames(x)}:}{Returns a character vector of names used for each parter in a relationship.
#' This may be \code{NULL} if the partners are not named.}
#' \item{\code{partner(x, type, id=FALSE)}:}{If \code{id=TRUE}, this returns an integer index for the partner specified by \code{type}.
#' If \code{id=FALSE}, this returns the partners as features.
#' \code{type} can be an integer scalar or a string if the partners are named.}
#' \item{\code{featureSets(x)}:}{Returns a \linkS4class{List} of feature sets.
#' Each entry corresponds to a separate feature set.}
#' \item{\code{featureSetNames(x)}:}{Returns a character vector of names used for each feature set.
#' This may be \code{NULL} if the feature sets are not named.}
#' \item{\code{mapping(x)}:}{Returns an integer vector specifying the mapping from partners to feature sets.
#' The indices of the \code{i}th partner point towards \code{featureSets(x)[[mapping(x)[i]]]}.}
#' }
#'
#' @section Setter methods:
#' In the following code snippets, \code{x} is a IndexedRelations object.
#' \describe{
#' \item{\code{partners(x) <- value}:}{Replaces the partner indices with \code{value}.
#' \code{value} should have the same number of columns as \code{partners(x)},
#' and should contain integer indices that point to valid entries of the corresponding feature set.}
#' \item{\code{partnerNames(x) <- value}:}{Replaces the names used for each partner with \code{value}, a character vector or \code{NULL}.}
#' \item{\code{partner(x, type, id=FALSE) <- value}:}{If \code{id=TRUE}, this replaces the indices for the partner specified by \code{type} with new integer indices in \code{value}.
#' If \code{id=TRUE}, \code{value} is assumed to contain the partners as features,
#' and replacement is performed while appending new features to the corresponding feature set in \code{featureSets(x)}.
#' \code{type} can be an integer scalar or a string if the partners are named.}
#' \item{\code{featureSets(x) <- value}:}{Replaces the feature sets with \code{value}.
#' \code{value} should have the same number of feature sets as \code{featureSets(x)},
#' and each set in \code{value} should be at least as long as the set that it replaces.} 
#' \item{\code{featureSetNames(x)}:}{Replaces the names used for each feature set with \code{value}, a character vector or \code{NULL}.}
#' }
#'
#' @section Combining and subsetting:
#' An IndexedRelations instance behaves like a vector in terms of subsetting and combining.
#' Specifically, it behaves like any other one-dimensional \linkS4class{Vector} subclass.
#' 
#' @section Miscellaneous:
#' \code{show(x)} will show information about an IndexedRelations \code{x}, including a preview of the \code{\link{head}} relationships.
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
#'         enhacner=enhancers[partner2]
#'     )
#' )
#' 
#' # But it's more efficient to just supply indices, where possible:
#' rel <- IndexedRelations(
#'     list(promoter=partner1, enhancer=partner2),
#'     features=list(promoters=promoters, enhancers=enhancers)
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
#' partner(rel, "promoter")
#' partner(rel, 1, id=TRUE)
#' partnerNames(rel)
#' 
#' featureSets(rel)
#' featureSetNames(rel)
#'
#' mapping(rel) # always seq_along(featureSets(rel)) for IndexedRelations.
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
#' partner(rel2, "P") <- rev(partner(rel2, "P")) # by feature
#' partners(rel2)
#'
#' partner(rel2, "E", id=TRUE) <- rev(partner(rel2, "E", id=TRUE)) # by ID
#' partners(rel2)
#' 
#' featureSets(rel2)$promoters <- resize(featureSets(rel2)$promoters, width=25)
#' featureSets(rel2)$promoters
#' 
#' featureSetNames(rel2) <- c("Pset", "Eset")
#' featureSetNames(rel2)
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
#' @aliases partners partners,IndexedRelations-method partners<- partners<-,IndexedRelations-method
#' @aliases partnerNames partnerNames,IndexedRelations-method partnerNames<- partnerNames<-,IndexedRelations-method
#' @aliases partner partner,IndexedRelations-method partner<- partner<-,IndexedRelations-method
#' @aliases features features,IndexedRelations-method features<- features<-,IndexedRelations-method
#' @aliases featureNames featureNames,IndexedRelations-method featureNames<- featureNames<-,IndexedRelations-method
#' @aliases feature feature,IndexedRelations-method feature<- feature<-,IndexedRelations-method
#' @aliases mapping mapping,IndexedRelations-method
#' @aliases parallelSlotNames,IndexedRelations-method
#' @aliases show,IndexedRelations-method
NULL

#' @export
#' @importFrom BiocGenerics match
#' @importFrom S4Vectors DataFrame
#' @importClassesFrom S4Vectors List
IndexedRelations <- function(x, features=NULL) {
    if (is(x, "IndexedRelations")) {
        return(x)
    }

    if (is.null(features)) {
        features <- x
        for (i in seq_along(features)) {
            current <- unique(x[[i]])
            features[[i]] <- current
            x[[i]] <- match(x[[i]], current)
        }
    }

    mapping <- seq_along(x)
    x <- lapply(x, as.integer)
    x <- lapply(x, unname)
    new("IndexedRelations", partners=do.call(DataFrame, x), features=as(features, "List"), mapping=mapping)
}

.oob <- function(indices, N) {
    any(indices > N) || any(indices < 1L) || any(is.na(indices))
}

#' @importFrom S4Vectors setValidity2
setValidity2("IndexedRelations", function(object) {
    msg <- character(0)
    rlt <- partners(object)
    map <- mapping(object)
    itr <- featureSets(object)

    # Checking mapping.
    if (length(map)!=ncol(rlt)) {
        msg <- c(msg, "length of 'mapping' should be the same as 'ncol(partners)'")
    } 
    if (.oob(map, length(itr))) {
        msg <- c(msg, "out-of-bounds 'mapping' indices")
    }

    # Checking feature stores.
    if (!length(itr) && ncol(rlt)) {
        msg <- c(msg, "'features' cannot be empty when 'partners' is not empty")
    }

    # Checking partners
    for (i in seq_len(ncol(rlt))) {
        current <- rlt[[i]]
        if (!is.integer(current)) {
            msg <- c(msg, sprintf("column %i of 'partners' is not integer", i))
        }

        cur.store <- itr[[map[i]]]
        if (.oob(current, length(cur.store))) {
            msg <- c(msg, sprintf("column %i of 'partners' contains out-of-bounds indices", i))
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

.map2store <- function(x, type) {
    if (is.character(type)) {
        type <- match(type, partnerNames(x))
    }
    mapping(x)[type]
}

#' @export
setMethod("partners", "IndexedRelations", function(x) x@partners)

#' @export
setMethod("partnerNames", "IndexedRelations", function(x) colnames(partners(x)))

#' @export
setMethod("partner", "IndexedRelations", function(x, type, id=FALSE) {
    ids <- partners(x)[[type]]
    if (id) {
        return(ids)
    }

    ftype <- .map2store(x, type)
    cur.store <- featureSets(x)[[ftype]]
    cur.store[ids]
})

#' @export
setReplaceMethod("partners", "IndexedRelations", function(x, value) {
    x@partners <- value
    x
})

#' @export
setReplaceMethod("partnerNames", "IndexedRelations", function(x, value) {
    colnames(partners(x)) <- value
    x
})

#' @export
#' @importFrom BiocGenerics match
setReplaceMethod("partner", "IndexedRelations", function(x, type, id=FALSE, ..., value) {
    if (!id) { 
        ftype <- .map2store(x, type)
        cur.store <- featureSets(x)[[ftype]]
        m <- match(value, cur.store)

        lost <- is.na(m)
        if (any(lost)) {
            lost.values <- value[lost]
            cur.store <- c(cur.store, unique(lost.values))
            m[lost] <- match(lost.values, cur.store)
            featureSets(x)[[type]] <- cur.store
        }

        value <- m
    }

    partners(x)[[type]] <- value 
    x
})

#################################
# Getters and setters: features #
#################################

#' @export
setMethod("featureSets", "IndexedRelations", function(x) x@features)

#' @export
setMethod("featureSetNames", "IndexedRelations", function(x) names(featureSets(x)))

#' @export
setReplaceMethod("featureSets", "IndexedRelations", function(x, value) {
    x@features <- value
    x
})

#' @export
setReplaceMethod("featureSetNames", "IndexedRelations", function(x, value) {
    names(featureSets(x)) <- value
    x
})

####################
# Getters: mapping #
####################

#' @export
setMethod("mapping", "IndexedRelations", function(x) x@mapping)

################################
# Subset and combining methods #
################################

#' @export
#' @importFrom S4Vectors parallelSlotNames
setMethod("parallelSlotNames", "IndexedRelations", function(x) 
    c("partners", callNextMethod()) # EASY!
)

########
# show #
########

#' @export
#' @importFrom methods show
#' @importFrom S4Vectors mcols metadata
#' @importFrom utils head capture.output
setMethod("show", "IndexedRelations", function(object) {
    N <- length(object)
    cat(sprintf("%s containing %i relation%s\n", class(object), N, if (N==1L) "" else "s"))

    meta <- mcols(object)
    nmeta <- if (is.null(meta)) 0 else ncol(meta)    
    meta.names <- colnames(meta)
    if (nmeta > 3) meta.names <- c(head(meta.names, 3), "...")
    meta.names <- if (nmeta) paste0(vapply(meta.names, deparse, FUN.VALUE=""), collapse=" ") else ""
    cat(sprintf("mcols names(%i): %s\n", nmeta, meta.names))

    meta <- metadata(object)
    nmeta <- length(meta)
    meta.names <- names(meta)
    if (nmeta > 3) meta.names <- c(head(meta.names, 3), "...")
    meta.names <- if (nmeta) paste0(vapply(meta.names, deparse, FUN.VALUE=""), collapse=" ") else ""
    cat(sprintf("metadata names(%i): %s\n", nmeta, meta.names))

    p <- partners(object)
    pn <- partnerNames(object)
    fn <- featureSetNames(object)

    for (i in seq_len(ncol(p))) {
        cat("\n")
        pname <- sprintf("Partner %i", i)
        if (!is.null(pn)) { pname <- paste0(pname, " (", deparse(pn[i]), ")") }

        j <- .map2store(object, i)
        fname <- sprintf("from feature set %i", j)
        if (!is.null(fn)) { fname <- paste0(fname, " (", deparse(fn[j]), ")") }
        cat(paste0(pname, " ", fname, "\n"))

        cur.p <- head(p[,i])
        cur.store <- featureSets(object)[[j]]
        cur.p <- cur.store[cur.p]
        X <- capture.output(show(cur.p))
        X <- sprintf("head| %s", X)
        cat(X, sep="\n")
    }
})
