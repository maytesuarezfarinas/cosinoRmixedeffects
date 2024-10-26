
#' Get MESOR(M), Amplitude(A), Acrophase
#'
#' Get the estimated marginal means for the specified factors or factor combinations in a linear model,
#' and transform to non-linear parameters MESOR, amplitude, and acrophase.
#'
#' @param fit  the lmer object from lmer()
#' @param contrast.frm  a string formula specifying the names of the predictors over which emmeans are desired.
#' @param ... other arguments passed on to methods
#'
#' @return
#' @export
#'
#' @examples
#'  f1.a<-lmer(hrv~age+gender+T0toT14+
#'             gender*rrr+gender*sss+
#'             T0toT14*rrr+T0toT14*sss+
#'             (1|participant_id),
#'             data=db.model, na.action = na.omit)
#'
#'  just.get.means.cosinor(fit=f1.a, contrast.frm='~T0toT14')
#'
#'
just.get.means.cosinor<- function(fit, contrast.frm,...) {
  #get the fitted contrasts and transform to Amp,Acr
  contrast.frm<-as.formula(contrast.frm)
  mf <- fit #object$fit

  groups.M<-emmeans(mf, contrast.frm)
  groups.rrr<-emtrends(mf, contrast.frm,'rrr')
  groups.sss<-emtrends(mf, contrast.frm,'sss')

  ## assemble summary matrix
  groups.rrr.db<-as.data.frame(groups.rrr)
  w<-which(colnames(groups.rrr.db)=='rrr.trend')-1
  groups.names<-as.vector(apply(groups.rrr.db,1,function(x){paste0(x[1:w],collapse=',')}))

  pars.raw.mesor<-groups.M@linfct %*% fixef(mf)
  pars.raw.rrr<-groups.rrr@linfct %*% fixef(mf)
  pars.raw.sss<-groups.sss@linfct %*% fixef(mf)

  names(pars.raw.mesor)<-paste('MESOR_',groups.names)
  amp<-apply(cbind(pars.raw.rrr,pars.raw.sss),1,function(x){sqrt(sum(x[1]^2+x[2]^2))})
  names(amp) <- paste0('Amplitude_',groups.names)

  acr<-apply(cbind(pars.raw.rrr,pars.raw.sss), 1,
             function(x){correct.acrophase.msf(b_rrr=x[1],b_sss=x[2])})
  names(acr) <- paste0('Acrophase_',groups.names)

  return(c(pars.raw.mesor,amp,acr))
}
