#' @export
#' @import methods
#' @importClassesFrom S4Vectors Vector DataFrame List
setClass("IndexedRelations", contains="Vector", slots=c(partners="DataFrame", features="List", mapping="integer"))
