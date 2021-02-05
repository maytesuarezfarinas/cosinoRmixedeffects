#' Get the correct value of acrophase
#'
#' Given beta (b_rrr) and gamma (b_sss), get the correct value of acrophase.
#'
#' @param b_rrr is the beta coefficient for x (beta), which is cos(2πt/τ), τ is period
#' @param b_sss is the beta coefficient for z (gamma), which is sin(2πt/τ), τ is period
#'
#' @return correct value of acrophase
#' @export
#'
#' @examples
#'
correct.acrophase.msf<-function (b_rrr,b_sss) {
  a<-atan(abs(b_sss/b_rrr))
  if (b_rrr > 0 & b_sss > 0) { k=0; g=-1}
  if (b_rrr > 0 & b_sss < 0) { k= -2*pi; g=1}
  if (b_rrr < 0 & b_sss > 0) { k= -pi; g=1}
  if (b_rrr < 0 & b_sss < 0) { k= -pi; g=-1}
  acrophase<-(k+g*a)
  return(acrophase)
}




