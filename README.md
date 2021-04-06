# cosinoRmixedeffects
The *cosinoRmixedeffects* package can be implemented for the estimation and prediction of the a mixed-effects cosinor model for longitudinal periodic data. *cosinoRmixedeffects* package fits a mixed-effects cosinor model and utilizing the functionality of the [emmeans]((httpZs://github.com/rvlenth/emmeans)) package provides estimated marginal means (EMMs) and contrast for binary, categorical predictors and their interactions. In the example presented here, we will use a circadian rhythm data collected over several days to estimate daily changes assuming random intercept for each subject and a correlated covariance structure.  Please refer to [vignettes](https://maytesuarezfarinas.github.io/cosinoRmixedeffects/vignettes/Introduction.html) for detailed usage examples. 

#### *cosinoRmixedeffects* functionality for the Warrior Watch Study is available in the preprint:
1. [Hirten, Robert P., et al.](https://www.medrxiv.org/content/10.1101/2020.11.06.20226803v1) "Longitudinal Physiological Data from a Wearable Device Identifies SARS-CoV-2 Infection and Symptoms and Predicts COVID-19 Diagnosis." medRxiv (2020).

## Installation

```{r setup}
remotes::install_github("maytesuarezfarinas/cosinoRmixedeffects")

library(cosinoRmixedeffects)
```

