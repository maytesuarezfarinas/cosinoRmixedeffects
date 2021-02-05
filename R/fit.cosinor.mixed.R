#' Fit a cosinor mixed model
#'
#' Given an outcome, predictor variable, fit the cosinor mixed model with optional covariate effects
#'
#' @param y a vector specifying the outcome variable /phenotype variable
#' @param x a vector specifying the predictor variables
#' @param data a dataframe with the x,y, covariates, random variables
#' @param na.action how to deal with missing data, default is na.action = na.omit
#' @param random a vector specifying the random effects as specified in lmer formula. e.g. "1|participant_id".
#' @param interaction a vector  specifying the variables for interactions
#'
#' @return
#' @export
#'
#' @examples
#'
#' library(lme4)
#' data("db.model")
#'
#' ## examine the effects of gender on MESOR, acrophase, amplitude on hrv
#' f<-fit.cosinor.mixed(y="hrv", x="gender", random="1|participant_id", data=db.model)
#' summary(f)
#'
#' ## examine the interaction effects of gender and baseline bmi categories on MESOR, acrophase, amplitude on hrv
#' f2<-fit.cosinor.mixed(y="hrv",interaction=c("gender","bmi_baseline_cat"),random="1|participant_id", data=db.model)
#' summary(f2)
#'
fit.cosinor.mixed<-function(y, x = NULL, random, interaction=NULL, data, na.action=na.omit){


  ## random effects part
  random.form<-paste0(paste("(",random,")", collapse = "+"))

  ## x are the covariates to estimate effects on MESOR, Amplitude and acrophase

  if (is.null(x)==TRUE & is.null(interaction) == TRUE){
    print("Error: Please put in covariates or interaction variables")
  }
  else{
    if (is.null(x)==TRUE & is.null(interaction) ==FALSE){

      ## interactions are interaction terms for adjustment
      ## interactions mainly on acrophase and amplitude, therefore, no MESOR coefficients kept for this
      interaction.term<-paste0(paste(interaction, collapse="*"))

      ## the interaction formula, with each term individually added in the model (From x.form), and the interaction term times rrr and sss in the model
      interaction.form<-paste0(interaction.term,"+", paste(interaction.term,"*","rrr", collapse = "+"),"+", paste(interaction.term,"*","sss", collapse = "+"))

      model.formula<-as.formula(paste0(y,"~",interaction.form,"+",random.form))
    }
    else{
      if(is.null(x) == FALSE & is.null(interaction)==TRUE){
        ## the most common condition with only covariates without interaction terms
        x.form<-paste0(paste(x, collapse="+"), "+",paste(x,"*","rrr", collapse = "+"),"+", paste(x,"*","sss", collapse = "+"))  ## when x is not NULL
        ## final model
        model.formula<-as.formula(paste0(y,"~",x.form, "+",random.form))
      }
      else{
        ##is.null(x)==FALSE & is.null(interaction) == FALSE)
        ## with x and interaction terms
        x.form<-paste0(paste(x, collapse="+"), "+",paste(x,"*","rrr", collapse = "+"),"+", paste(x,"*","sss", collapse = "+"))

        ## interactions are interaction terms for adjustment
        ## interactions mainly on acrophase and amplitude, therefore, no MESOR coefficients kept for this
        interaction.term<-paste0(paste(interaction, collapse="*"))

        ## the interaction formula, with each term individually added in the model (From x.form), and the interaction term times rrr and sss in the model
        interaction.form<-paste0(interaction.term,"+", paste(interaction.term,"*","rrr", collapse = "+"),"+", paste(interaction.term,"*","sss", collapse = "+"))

        model.formula<-as.formula(paste0(y,"~",x.form, "+",interaction.form,"+",random.form))
      }
    }
  }


  fit<-lmer(model.formula,
            data=data,
            na.action = na.action)

}



