#' Get the stars for statistical test plots
#'
#' @param db.delta the output from function get.contrasts.ci.cosinor()
#' @param contrast.frm a string formula specifying the names of the predictors over which emmeans are desired.
#'
#' @return
#' @export
#'
#' @examples
#' stat.test.stars(db.delta=db.delta, contrast.frm = "~gender")
#'

stat.test.stars <- function (db.delta, db.means, contrast.frm)
{
  require(rstatix)
  db.delta <- as.data.frame(db.delta)
  stat.test <- data.frame(Param = factor(strsplit2(rownames(db.delta), "_")[, 1]), .y. = "MEAN",
                          group1 = strsplit2(rownames(db.delta), "_")[, 2],
                          group2 = strsplit2(rownames(db.delta), "_")[,5],
                          n1 = 10, n2 = 10, statistic = db.delta$boot.estimate,
                          df = db.delta$boot.SE, p = db.delta$pvalue)

  stat.test <- stat.test %>% add_significance(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 0.1, 1), symbols = c("****", "***", "**", "*", "+", "ns"))

  stat.test <- stat.test %>% add_x_position()
  stat.test <- do.call("rbind.data.frame", lapply(c("MESOR", "Amplitude", "Acrophase", "PeakTime"), function(L) {
    db.temp <- subset(stat.test, Param == L)
    contrast.frm <- mgsub(c("[|]", "[*]"), c("_", "_"), contrast.frm)
    db.temp2 <- add_y_position(db.temp, formula = as.formula(paste0("`97.5 %`",
                                                                    contrast.frm)), data = subset(db.means, Param == L))}))
  $
    stat.test <- mutate(stat.test, contrast = paste0(group1, "_vs_", group2))

  stat.test
}

