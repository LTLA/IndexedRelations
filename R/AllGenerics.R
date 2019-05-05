#' @export
setGeneric("partners", function(x) standardGeneric("partners"))

#' @export
setGeneric("partnerNames", function(x) standardGeneric("partnerNames"))

#' @export
setGeneric("npartners", function(x) standardGeneric("npartners"))

#' @export
setGeneric("partnerFeatures", function(x, ...) standardGeneric("partnerFeatures"))

#' @export
setGeneric("partners<-", function(x, value) standardGeneric("partners<-"))

#' @export
setGeneric("partnerNames<-", function(x, value) standardGeneric("partnerNames<-"))

#' @export
setGeneric("partnerFeatures<-", function(x, ..., value) standardGeneric("partnerFeatures<-"))

#' @export
setGeneric("featureSets", function(x) standardGeneric("featureSets"))

#' @export
setGeneric("featureSets<-", function(x, value) standardGeneric("featureSets<-"))

# For internal use only.
setGeneric(".partners<-", function(x, value) standardGeneric(".partners<-"))

setGeneric(".featureSets<-", function(x, value) standardGeneric(".featureSets<-"))
