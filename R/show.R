# The show method involves enough submethods that I've just
# put it into a separate file; this is documented in IndexedRelations.R

#' @importFrom S4Vectors mcols showAsCell
.makeNakedMatFromIndexedRelations <- function(x) {
    collected <- vector("list", npartners(x))
    for (i in seq_along(collected)) {
        collected[[i]] <- showAsCell(partnerFeatures(x, i))
    }
    ans <- do.call(cbind, collected)
    colnames(ans) <- partnerNames(x)

    x_mcols <- mcols(x, use.names = FALSE)
    if (!is.null(x_mcols) && ncol(x_mcols) > 0L) {
        tmp <- do.call(data.frame, c(lapply(x_mcols, showAsCell), list(check.names = FALSE)))
        ans <- cbind(ans, `|` = rep.int("|", length(x)), as.matrix(tmp))
    }

    ans
}

#' @importFrom S4Vectors mcols
showIndexedRelations <- function(x, margin = "", print.classinfo = FALSE) {
    x_class <- class(x)
    x_len <- length(x)
    x_mcols <- mcols(x, use.names = FALSE)
    x_nmc <- if (is.null(x_mcols)) 0L else ncol(x_mcols)
    cat(x_class, " object with ", 
        x_len, " relation", ifelse(x_len == 1L, "", "s"), " and ", 
        x_nmc, " metadata column", ifelse(x_nmc == 1L, "", "s"), 
        ":\n", sep = "")
    
    out <- S4Vectors:::makePrettyMatrixForCompactPrinting(x, .makeNakedMatFromIndexedRelations)

    if (print.classinfo) {
        .COL2CLASS <- lapply(mapping(x), function(y) class(featureSets(x)[[y]]))
        names(.COL2CLASS) <- partnerNames(x)
        classinfo <- S4Vectors:::makeClassinfoRowForCompactPrinting(x, .COL2CLASS)
        stopifnot(identical(colnames(classinfo), colnames(out)))
        out <- rbind(classinfo, out)
    }
    if (nrow(out) != 0L) {
        rownames(out) <- paste0(margin, rownames(out))
    }

    print(out, quote = FALSE, right = TRUE, max = length(out))
}

#' @export
#' @importFrom methods show
setMethod("show", "IndexedRelations", function(object) {
    showIndexedRelations(object, margin="  ", print.classinfo=TRUE)
})
