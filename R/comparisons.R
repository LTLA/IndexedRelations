#' @export
#' @importFrom S4Vectors pcompare
setMethod("pcompare", c("IndexedRelations", "IndexedRelations"), function(x, y) {
    std.feat <- .standardize_featureSets(x, list(y), clean=TRUE)
    x <- std.feat$x
    y <- std.feat$objects[[1]]

    output <- integer(max(length(x), length(y)))
    px <- partners(x)
    py <- partners(y)

    for (i in seq_len(ncol(px))) {
        current <- sign(px[,i] - py[,i]) # recycles.
        undecided <- output==0
        output[undecided] <- current[undecided]
    }

    output
})

#' @export
#' @importFrom BiocGenerics match
setMethod("match", c("IndexedRelations", "IndexedRelations"), function(x, table, nomatch = NA_integer_, incomparables = NULL, ...) {
    std.feat <- .standardize_featureSets(x, list(table), clean=TRUE)
    x <- std.feat$x
    table <- std.feat$objects[[1]]
 
    combined <- rbind(partners(x), partners(table))

    # 'all.origins' ensures that the 'table' entry is first if any entries of 'x' are equal,
    # and that the sort is stable with respect to elements within 'table'.
    all.origins <- c(rep(NA_integer_, length(x)), seq_along(table))
    o <- do.call(order, c(as.list(combined[seq_len(N)]), list(all.origins)))
    combined <- combined[o,]
    all.origins <- all.origins[o]

    is.unique <- Reduce("|", lapply(combined, FUN=function(x) c(TRUE, diff(x)>0L)))
    has.origin <- cumsum(is.unique)
    has.origin <- all.origins[has.origin]
    has.origin[o] <- has.origin
    head(has.origin, length(x))
})

#' @export
#' @importFrom BiocGenerics order
setMethod("order", "IndexedRelations", function(..., na.last=TRUE, decreasing=FALSE, method=c("auto", "shell", "radix")) {
    objects <- list(...)
    objects <- lapply(objects, .clean_featureSets)
    all.partners <- lapply(objects, partners)
    all.partners <- unlist(lapply(all.partners, as.list), recursive=FALSE)
    do.call(order, c(all.partners, list(na.last=na.list, decreasing=decreasing, method=method)))
})
