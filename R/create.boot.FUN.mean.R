#' Bootstrap to get standard error of mean MESOR, amplitude and acrophase
#'
#' Bootstrap function takes in a fitted merMod object as input and returns the statistics (e.g. standard error) of mean
#' MESOR, amplitude and acrophase in each group.
#'
#' @param contrast.frm  a string formula specifying the names of the predictors over which emmeans are desired.
#'
#' @return
#' @export
#'
#' @examples
#' f1.b<-lmer(hrv~age+gender+T0toT14+Tneg7toT7+
#'            gender*rrr+gender*sss+
#'            T0toT14*rrr+T0toT14*sss+
#'            Tneg7toT7*rrr+Tneg7toT7*sss+
#'            (1|participant_id),
#'            data=db.model, na.action = na.omit)
#'
#' boot.mean<-bootMer(f1.b,FUN = create.boot.FUN.mean(contrast.frm='~T0toT14+Tneg7toT7'), nsim =500, parallel = "multicore", ncpus=8)
#'
#'


create.boot.FUN.mean<-function(contrast.frm){
  boot.FUN.mean=function(.){just.get.means.cosinor(., contrast.frm=contrast.frm)};
  return(boot.FUN.mean)}

