#' Bootstrap to get standard error of MESOR, amplitude and acrophase contrasts
#'
#' Bootstrap function takes in a fitted merMod object as input and returns the statistics (e.g. standard error) of pair-wise contrasts
#' of MESOR, amplitude and acrophase
#'
#' @param contrast.frm a string formula specifying the names of the predictors over which emmeans are desired.
#'
#' @return
#' @export
#'
#' @examples
#'
#' f1.b<-lmer(hrv~age+gender+T0toT14+Tneg7toT7+
#'            gender*rrr+gender*sss+
#'            T0toT14*rrr+T0toT14*sss+
#'            Tneg7toT7*rrr+Tneg7toT7*sss+
#'            (1|participant_id),
#'            data=db.model, na.action = na.omit)
#'
#' boot.cont<-bootMer(f1.b,FUN = create.boot.FUN.cont(contrast.frm='~T0toT14+Tneg7toT7'), nsim =500, parallel = "multicore", ncpus=8)
#'
#'
#'
create.boot.FUN.cont<-function(contrast.frm){
  boot.FUN.cont=function(.){just.get.contrasts.cosinor(., contrast.frm=contrast.frm)};
  return(boot.FUN.cont)}
