
get.contrasts.ci.cosinor<-function (fit, contrast.frm, nsim = 500, parallel = "multicore", 
          ncpus = 8, conftype = "perc", conflevel = 0.95, do.plot=FALSE,...) 
{
  t0 <- Sys.time()
  doMC::registerDoMC(8)
  boot.cont <- bootMer(fit, FUN = create.boot.FUN.cont(contrast.frm = contrast.frm), 
                       nsim = nsim, parallel = parallel, ncpus = ncpus)
  Sys.time() - t0
  ps <- sapply(1:length(boot.cont$t0), function(i, boot.object) {
    t0_obs <- boot.object$t0[i]
    t_obs <- boot.object$t[, i]
    z_underH0 <- t_obs - mean(t_obs)
    if (do.plot){
    plot(density(z_underH0), xlim = range(c(t0_obs, z_underH0)))
    points(t0_obs, 0, pch = "*", cex = 5)
    }
    p_2tails <- mean(abs(z_underH0) > abs(t0_obs))
  }, boot.object = boot.cont, USE.NAMES = T)
  db.delta <- cbind(confint(boot.cont, type = conftype, level = conflevel), 
                    boot.estimate = boot.cont$t0, boot.SE = apply(boot.cont$t, 
                                                                  2, sd), pvalue = ps)
  return(db.delta)
}
