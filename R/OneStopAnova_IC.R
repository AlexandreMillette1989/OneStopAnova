#' @title OneStopAnova_IC
#'
#' @description This function generates Confidence Interval from One-Way | Two-Way Anova function object
#'
#' @usage OneStopAnova_IC(aov = "", p = "")
#'
#' @param aov: object holding the anova results
#'
#' @param p: confidence level (0.025 = 95 percent)
#'
#' @examples
#' aov = aov(Concentration ~ Trait + Sexe + Trait:Sexe, data = calcium)
#' OneStopAnova_IC(aov = aov, p = 0.025)
#'
#' @export

OneStopAnova_IC = function(aov = "", p = ""){
  anova_predictions_table = expand.grid(aov$xlevels)
  anova_predictions_means = predict(aov, newdata = anova_predictions_table, se.fit = TRUE)
  anova_predictions_table$fit = anova_predictions_means$fit
  anova_predictions_table$se.fit = anova_predictions_means$se.fit
  anova_predictions_table$lower_ic = anova_predictions_table$fit + qt(p = p, df = aov$df.residual) * anova_predictions_table$se.fit
  anova_predictions_table$upper_ic = anova_predictions_table$fit - qt(p = p, df = aov$df.residual) * anova_predictions_table$se.fit
  return(kable(anova_predictions_table))
}
