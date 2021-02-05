#' Create cosinor parameters using the time variable and the cosinor period
#'
#' @param time a vector of time variable name, with same units as that of the cosinor period
#' @param period the length of the cosinor period, eg.24
#' @param data the dataframe with the time and period information
#'
#' @return
#' @export
#'
#' @examples
#' db.model<-create.cosinor.param(time="Hour_of_Day", period=24, data=data)
create.cosinor.param<-function(time, period, data){
  db.cosinor<-mutate(data,
                     t=data[,time],
                     rrr=cos(2*pi*t/period),
                     sss=sin(2*pi*t/period))
}
