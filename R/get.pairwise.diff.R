#' Get pairwise difference between two comparison groups
#'
#' Get pairwise difference between two comparison groups
#' @param x is numeric values of two groups e.g. MESOR between two groups (from just.get.means.cosinor())
#'
#' @return The difference between the two groups. e.g  "MESOR_A_vs_B" and it returns the value of B-A.
#' @export
#'
#' @examples
get.pairwise.diff<-function(x){
  nm <- apply(combn(rev(names(x)), 2), 2, paste0, collapse = "_vs_")
  out <- apply(combn(x, 2), 2, function(x){x[2]-x[1]})
  names(out)<-nm
  return(out)
}
