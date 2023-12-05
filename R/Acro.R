#' Get the Acrophase and PeakTime
#'
#' @param period = 24
#' @param acrophase
#'
#' @return
#' @export
#'
#'

fromAcrophaseToTime<-function(acrophase, period=24){-(acrophase*period)/(2*pi)}
