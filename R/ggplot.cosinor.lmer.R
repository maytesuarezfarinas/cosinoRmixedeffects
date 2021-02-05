#' Plot a cosinor model
#'
#' Given a lmer model fit, generate a plot of the data with the fitted values.
#'
#'
#' @param object  a lmer fit object (lmerMod object)
#' @param x_str  a character vector naming the covariate(s) to be plotted
#' @param period  the length of time for a cycle.
#' @param db.means  the data frame with mean values for MESOR, amplitude, and acrophase, for each comparison group
#' @param DATA  a data frame containing the variables named in lmer object
#'
#' @return
#' @export
#'
#' @examples
#' ggplot.cosinor.lmer(object=f1.a,
#'                     x_str="T0toT14",
#'                     period=24,
#'                     db.means=db.means,
#'                     DATA=db.model)
#'

ggplot.cosinor.lmer<- function(object=NULL,
                                 x_str=NULL,
                                 period=NULL,
                                 db.means=NULL,
                                 DATA=NULL){

  ### Create simulate time data for each group ###
  # a<-lapply(x_str, function(d){unique(object@frame[,d])}) # list, each element is a model variable, unique values
  time.var='h'

  random_id_var<-strsplit(as.character(formula(object))[3], "|", fixed = TRUE)[[1]][2]
  random_id_var<-gsub(")","",random_id_var)
  random_id_var<-gsub(" ","",random_id_var)


  a<-lapply(x_str, function(d){unique(db.means[,d])})
  xd<-as.data.frame(Reduce(expand.grid.df, a)) # Gets all values of each variable
  xd<-cbind.data.frame(xd, paste('Subject',1:nrow(xd)))
  a[[3]]<-(seq(0, period, length.out = 200))
  newdata<-expand.grid.df(xd, time=(seq(0, period, length.out = 200)))
  colnames(newdata)<-c(x_str, random_id_var, time.var)

  ## Add levels
  if (!is.null(x_str)) {
    newdata$levels=''
    for (d in x_str) {
      newdata$levels <- paste(newdata$levels, paste0(d, "=", newdata[,d]))
    }
  }

  ## Get Mean Acrophase, Amplitude and MESOR for each group
  db.var<-sapply(x_str, function(i){
    db.means[,i]
  })
  colnames(db.var)<-paste0("VAR",1:length(x_str))

  db.means2<-cbind.data.frame(db.means, db.var)

  FORM<-paste0(paste0(colnames(db.var),collapse = "+"),"~Param")

  db.params<-dcast(db.means2, FORM, value.var = "MEAN")

  db.var<-sapply(x_str, function(i){
    newdata[,i]
  })
  colnames(db.var)<-paste0("VAR",1:length(x_str))

  newdata<-cbind.data.frame(newdata, db.var)

  db.plot<-merge(newdata, db.params, by=colnames(db.var))

  db.plot$Yhat<-with(db.plot, MESOR+Amplitude*cos((2*pi*h/period)+Acrophase))

  db.plot$GROUP<-apply(db.plot[,grep("VAR", colnames(db.plot)), drop=F], MARGIN = 1, FUN=function(vec){
    paste(vec, collapse = "")
  })

  ### rough est: what is t when Yhat = A

  db.plot<-mutate(db.plot,
                  T_AMP=-(Acrophase*period)/(2*pi))



  if (missing(x_str) || is.null(x_str)) {
    ggplot(newdata, aes_string(x = time.var, y = "Yhat"))
  }
  else {
    ggplot(db.plot, aes_string(x = time.var, y = "Yhat", color="GROUP"))
  }
}





