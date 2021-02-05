#' Get contrasts and confidence interval for MESOR, Amplitude, Acrophase
#'
#' The function takes in a fitted merMod object as input and returns the statistics (e.g. standard error) of pair-wise contrasts
#' of MESOR, amplitude and acrophase. The output are bootstrapped estimates, 95% confidence intervals and p values for each comtrast
#' for MESOR, Amplitude and acrophase.
#'
#' @param fit the object from lmer()
#' @param contrast.frm a string formula specifying the names of the predictors over which emmeans are desired.
#' @param nsim the number of simulations, positive integer; the bootstrap B (or R) for bootMer function.Default is 500.
#' @param parallel the type of parallel operation to be used (if any) for bootMer function. The default is "multicore".
#' @param ncpus integer: number of processes to be used in parallel operation: typically one would choose this to be the number of available CPUs.Default is 8.
#' @param conftype a character string representing the type of interval required.The value must be one of "norm", "basic","perc".
#' @param conflevel The confidence level required, default is 0.95.
#' @param ... additional argument(s) for methods.
#'
#' @return
#' @export
#'
#' @examples
#' library(lme4)
#' library(emmeans)
#' data("db.model")
#' f<-fit.cosinor.mixed(y="hrv", x="gender", random="1|participant_id", data=db.model)
#' summary(f)
#' get.contrasts.ci.cosinor(f,contrast.frm="~gender")
#'
get.contrasts.ci.cosinor<-function(fit,contrast.frm, nsim=500,parallel = "multicore", ncpus=8, conftype="norm", conflevel= 0.95,...){

  ## bootstrap to get contrasts
  t0<-Sys.time()
  doMC::registerDoMC(8)
  boot.cont<-bootMer(fit,
                     FUN = create.boot.FUN.cont(contrast.frm=contrast.frm),
                     nsim =nsim,
                     parallel = parallel,
                     ncpus=ncpus)
  Sys.time()-t0


  ps<-sapply(1:length(boot.cont$t0), function(i, boot.object){
    t0_obs<-boot.object$t0[i]; t_obs<-boot.object$t[,i]
    #z_underHA=t_obs-mean(t_obs)+t0_obs  #~N(t*)
    #z_underH0<-z-t0_obs                      #~N(0)
    z_underH0<-t_obs-mean(t_obs) # create null distribution
    plot(density(z_underH0), xlim=range(c(t0_obs,z_underH0)))
    points(t0_obs,0,pch='*',cex=5)
    p_2tails<-mean(abs(z_underH0) > abs(t0_obs))},
    boot.object=boot.cont, USE.NAMES = T)

  ## Final db containing deltas
  db.delta<-cbind(confint(boot.cont, type=conftype, level=conflevel),
                  boot.estimate=boot.cont$t0,
                  boot.SE=apply(boot.cont$t,2,sd),
                  pvalue=ps)

  db.delta
}






