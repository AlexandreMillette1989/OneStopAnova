#' @title OneStopAnova
#'
#' @description This package generates One-Way and Two-Way Anova with assumptions verification, optional Log and optional Tukey.
#'
#' @usage OneStopAnova(Quantitative = "",
#'              Qualitative = "",
#'              Qualitative2 = "",
#'              var_names = c(Quantitative = "",
#'                            Qualitative = "",
#'                            Qualitative2 = ""),
#'              Log = TRUE/FALSE,
#'              Tukey = TRUE/FALSE)
#'
#' @param Quantitative: The Quantitative variable
#'
#' @param Qualitative: The Qualitative variable
#'
#' @param Qualitative2: The Second Qualitative variable
#'
#' @param var_names: The Names of the Selected Variables
#'
#' @param Log: FALSE by default. If TRUE, Log Transformation of the Quantitative Variable
#'
#' @param Tukey: FALSE by default. If TRUE, Tukey Test on the Selected Variables
#'
#' @examples
#' OneStopAnova(Quantitative = "calcium$Concentration",
#'              Qualitative = "calcium$Trait",
#'              Qualitative2 = "calcium$Sexe",
#'              var_names = c(Quantitative = "Concentration",
#'                            Qualitative = "Trait",
#'                            Qualitative2 = "Sexe"),
#'              Log = TRUE,
#'              Tukey = TRUE)
#'
#' @export

OneStopAnova = function(Quantitative, Qualitative, Qualitative2, var_names = c(Quantitative = "", Qualitative = "", Qualitative2 = "", ...), Log = c(TRUE, FALSE), Tukey = c(TRUE, FALSE)){
  ## Var names

  var_qualitative = var_names[2]
  var_qualitative2 = var_names[3]
  var_qualitative_by_qualitative2 = sprintf("%s:%s", var_qualitative, var_qualitative2)

  var_names = c(var_qualitative, var_qualitative2, var_qualitative_by_qualitative2)

  ## Log == FALSE
  if((missing(Log))||(Log == FALSE)){

    ## One-way Anova
    if(missing(Qualitative2)){
      one_way_anova = aov(Quantitative~Qualitative)
      par(mfrow = c(1,2))

      ## 1. Supposition homoscédasticité
      plot(residuals(one_way_anova)~fitted(one_way_anova), ylab = "Résidus", xlab = "Valeurs prédites", main = "Résidus vs valeurs prédites", cex.lab = 1.2)
      levene = leveneTest(Quantitative ~ Qualitative)
      levene = round((levene),3)
      options(knitr.kable.NA = "")
      levene = levene %>%
        mutate(
          `Pr(>F)` = if_else(`Pr(>F)` == 0, 0.001, `Pr(>F)`))
      if(levene$`Pr(>F)`[1] <= 0.05){
        levene_title = "One-Way Anova Levene's Test for Homogeneity of Variance (center = median) \
        \
        WARNING: Evidence suggests that the variance across groups is statistically significantly different. \
        WARNING: We cannot assume the homogeneity of variances in the different treatment groups."
      } else {
        levene_title = "One-Way Anova Levene's Test for Homogeneity of Variance (center = median) \
        \
        There is no evidence to suggest that the variance across groups is statistically significantly different. \
        We can assume the homogeneity of variances in the different treatment groups."
      }
      levene_kable = kable(levene, caption = levene_title,
                           format = "pandoc")

      ## 2. Supposition de normalité des résidus
      qqnorm(residuals(one_way_anova), ylab = "Quantiles observés", xlab = "Quantiles théoriques", main = "Graphique quantile-quantile", cex.lab = 1.2)
      qqline(residuals(one_way_anova))

      one_way_anova_residuals = residuals(object = one_way_anova)
      anderson_darling = ad.test(one_way_anova_residuals)
      anderson_darling_table = cbind(anderson_darling$statistic, anderson_darling$p.value)
      colnames(anderson_darling_table) = c("Test Statistic", "p")
      rownames(anderson_darling_table) = c()
      anderson_darling_table = round((anderson_darling_table),3)
      anderson_darling_table = as.data.frame(anderson_darling_table)
      anderson_darling_table = anderson_darling_table %>%
        mutate(
          p = if_else(p == 0, 0.001, p))
      colnames(anderson_darling_table) = c("Test Statistic", "P-Value")

      if(anderson_darling$p.value <= 0.05){
        ad_title = "One-Way Anova Anderson-Darling Normality Test \
        \
        WARNING: We have sufficient evidence to reject the null hypothesis.
        WARNING: It is safe to say that the data doesn't follow a normal distribution."
      } else {
        ad_title = "One-Way Anova Anderson-Darling Normality Test \
        \
        We do not have sufficient evidence to reject the null hypothesis.
        It is safe to say that the data follows a normal distribution."
      }
      anderson_darling_kable = kable(anderson_darling_table, caption = ad_title, format = "pandoc")


      ## Transform One-Way Anova To DataFrame For Kable Summary
      one_way_anova_table = data.frame(unclass(summary(one_way_anova)))
      one_way_anova_table = round((one_way_anova_table),3)
      one_way_anova_table[, "P_symbols"] = "NA"
      colnames(one_way_anova_table) = c("Df", "Sum Sq", "Mean Sq", "F Value", "Pr(>F)", "P_symbols")
      one_way_anova_table[is.na(one_way_anova_table)] = "NA"
      one_way_anova_table = one_way_anova_table %>%
        mutate(
          `Pr(>F)` = if_else(`Pr(>F)` == "0", "0.001", `Pr(>F)`))
      rownames(one_way_anova_table) = c(var_names[1], "Residuals")

      ## Significance Symbol Loop
      for(i in seq_along(one_way_anova_table$P_symbols)){
        row = one_way_anova_table$`Pr(>F)`[i]
        row_symbol = one_way_anova_table$P_symbols[i]
        if(row <= 0.001){
          row_symbol = paste0("***")
        }else if(row <= 0.01){
          row_symbol = paste0("**")
        }else if(row <= 0.05){
          row_symbol = paste0("*")
        } else if(row <= 0.10){
          row_symbol = paste0(".")
        } else if(row <= ""){
          row_symbol = paste0(" ")
        } else
          row_symbol = paste0(" ")
        one_way_anova_table$`Pr(>F)`[i] = row
        one_way_anova_table$P_symbols[i] = row_symbol
      }

      ## Rearrange Table
      one_way_anova_table[one_way_anova_table == "NA"] = ""
      colnames(one_way_anova_table) = c("Df", "Sum Sq", "Mean Sq", "F Value", "Pr(>F)", "Significance Symbol")
      rownames(one_way_anova_table) = c(var_names[1], "Residuals")

      ## Tukey == FALSE
      if(missing(Tukey)||(Tukey == FALSE)){

        ## Résultat One-Way ANOVA | No Tukey | No Log
        one_way_anova_kable = kable(one_way_anova_table, format = "pandoc", caption = "One-Way Anova Summary")
        one_way_anova_summary = list(levene_kable,
                                     anderson_darling_kable,
                                     one_way_anova_kable)

        return(one_way_anova_summary)
      }

      ## Tukey == TRUE
      if(Tukey == TRUE){

        one_way_anova_tukey_qualitative = TukeyHSD(one_way_anova, which = "Qualitative")
        one_way_anova_tukey_table = data.frame(unclass(one_way_anova_tukey_qualitative$Qualitative))
        one_way_anova_tukey_table = round((one_way_anova_tukey_table ),3)
        one_way_anova_tukey_table[, "P_symbols"] = "NA"
        colnames(one_way_anova_tukey_table) = c("Difference", "Lower", "Upper", "Adjusted", "Significance")
        one_way_anova_tukey_table = one_way_anova_tukey_table %>%
          mutate(
            Adjusted = if_else(Adjusted == 0, 0.001, Adjusted))
        colnames(one_way_anova_tukey_table) = c("Difference", "Lower", "Upper", "Adjusted", "Significance")
        rownames(one_way_anova_tukey_table) = c(row.names(one_way_anova_tukey_qualitative$Qualitative))
        one_way_anova_tukey_table

        ## Tukey significance Symbol Loop
        for(i in seq_along(one_way_anova_tukey_table$Significance)){
          row = one_way_anova_tukey_table$Adjusted[i]
          row_symbol = one_way_anova_tukey_table$Significance[i]
          if(row <= 0.001){
            row_symbol = paste0("***")
          }else if(row <= 0.01){
            row_symbol = paste0("**")
          }else if(row <= 0.05){
            row_symbol = paste0("*")
          } else if(row <= 0.10){
            row_symbol = paste0(".")
          } else if(row <= ""){
            row_symbol = paste0(" ")
          } else
            row_symbol = paste0(" ")
          one_way_anova_tukey_table$Adjusted[i] = row
          one_way_anova_tukey_table$Significance[i] = row_symbol
        }
        colnames(one_way_anova_tukey_table) = c("Difference", "Lower 95", "Upper 95", "Adjusted P-Value", "Significance Symbol")

        ## Summary String Title
        summary_title_string1 = "One-Way Anova Tukey"
        summary_title_string2 = var_names[1]
        summary_title_string3 = var_names[2]
        summary_title_string4 = "Summary"
        summary_title_qualitative = paste(summary_title_string1, summary_title_string2, summary_title_string4)
        summary_title_qualitative2 = paste(summary_title_string1, summary_title_string3, summary_title_string4)

        ## Listing the summaries
        one_way_anova_tukey_print_summary = kable(one_way_anova_tukey_table, format = "pandoc", caption = summary_title_qualitative)
        one_way_anova_print_summary = kable(one_way_anova_table, format = "pandoc", caption = summary_title_qualitative2)
        one_way_anova_print_list = list(levene_kable,
                                        anderson_darling_kable,
                                        one_way_anova_print_summary,
                                        one_way_anova_tukey_print_summary)

        ## Résultat One-Way ANOVA | Tukey
        return(one_way_anova_print_list)
      }
    }

    ## Two-way Anova
    else {
      two_way_anova = aov(Quantitative ~ Qualitative + Qualitative2 + Qualitative:Qualitative2)
      par(mfrow = c(1,2))

      ## 1. Supposition homoscédasticité
      plot(residuals(two_way_anova)~fitted(two_way_anova), ylab = "Résidus", xlab = "Valeurs prédites", main = "Résidus vs valeurs prédites", cex.lab = 1.2)
      levene = leveneTest(Quantitative ~ Qualitative:Qualitative2)
      levene = round((levene),3)
      options(knitr.kable.NA = "")
      levene = levene %>%
        mutate(
          `Pr(>F)` = if_else(`Pr(>F)` == 0, 0.001, `Pr(>F)`))
      if(levene$`Pr(>F)`[1] <= 0.05){
        levene_title = "Two-Way Anova Levene's Test for Homogeneity of Variance (center = median) \
        \
        WARNING: Evidence suggests that the variance across groups is statistically significantly different. \
        WARNING: We cannot assume the homogeneity of variances in the different treatment groups."
      } else {
        levene_title = "Two-Way Anova Levene's Test for Homogeneity of Variance (center = median) \
        \
        There is no evidence to suggest that the variance across groups is statistically significantly different. \
        We can assume the homogeneity of variances in the different treatment groups."
      }
      levene_kable = kable(levene, caption = levene_title,
                           format = "pandoc")

      ## 2. Supposition de normalité des résidus
      qqnorm(residuals(two_way_anova), ylab = "Quantiles observés", xlab = "Quantiles théoriques", main = "Graphique quantile-quantile", cex.lab = 1.2)
      qqline(residuals(two_way_anova))

      two_way_anova_residuals = residuals(object = two_way_anova)
      anderson_darling = ad.test(two_way_anova_residuals)
      anderson_darling_table = cbind(anderson_darling$statistic, anderson_darling$p.value)
      colnames(anderson_darling_table) = c("Test Statistic", "p")
      rownames(anderson_darling_table) = c()
      anderson_darling_table = round((anderson_darling_table),3)
      anderson_darling_table = as.data.frame(anderson_darling_table)
      anderson_darling_table = anderson_darling_table %>%
        mutate(
          p = if_else(p == 0, 0.001, p))
      colnames(anderson_darling_table) = c("Test Statistic", "P-Value")
      if(anderson_darling$p.value <= 0.05){
        ad_title = "Two-Way Anova Anderson-Darling Normality Test \
        \
        WARNING: We have sufficient evidence to reject the null hypothesis.
        WARNING: It is safe to say that the data doesn't follow a normal distribution."
      } else {
        ad_title = "Two-Way Anova Anderson-Darling Normality Test \
        \
        We do not have sufficient evidence to reject the null hypothesis.
        It is safe to say that the data follows a normal distribution."
      }
      anderson_darling_kable = kable(anderson_darling_table, caption = ad_title, format = "pandoc")

      ## Transform Two-Way Anova To DataFrame For Kable Summary
      two_way_anova_table = data.frame(unclass(summary(two_way_anova)))
      two_way_anova_table = round((two_way_anova_table),3)
      two_way_anova_table[, "P_symbols"] = "NA"
      colnames(two_way_anova_table) = c("Df", "Sum Sq", "Mean Sq", "F Value", "Pr(>F)", "P_symbols")
      two_way_anova_table[is.na(two_way_anova_table)] = "NA"
      two_way_anova_table = two_way_anova_table %>%
        mutate(
          `Pr(>F)` = if_else(`Pr(>F)` == "0", "0.001", `Pr(>F)`))
      rownames(two_way_anova_table) = c(var_names[1],var_names[2],var_names[3], "Residuals")

      ## Significance Symbol Loop
      for(i in seq_along(two_way_anova_table$P_symbols)){
        row = two_way_anova_table$`Pr(>F)`[i]
        row_symbol = two_way_anova_table$P_symbols[i]
        if(row <= 0.001){
          row_symbol = paste0("***")
        }else if(row <= 0.01){
          row_symbol = paste0("**")
        }else if(row <= 0.05){
          row_symbol = paste0("*")
        } else if(row <= 0.10){
          row_symbol = paste0(".")
        } else if(row <= ""){
          row_symbol = paste0(" ")
        } else
          row_symbol = paste0(" ")
        two_way_anova_table$`Pr(>F)`[i] = row
        two_way_anova_table$P_symbols[i] = row_symbol
      }

      ## Rearrange Table
      two_way_anova_table[two_way_anova_table == "NA"] = ""
      colnames(two_way_anova_table) = c("Df", "Sum Sq", "Mean Sq", "F Value", "Pr(>F)", "Significance Symbol")

      ## Tukey == FALSE
      if(missing(Tukey)||(Tukey == FALSE)){

        ## Résultat Two-Way ANOVA | No Tukey | No Log
        two_way_anova_kable = kable(two_way_anova_table, format = "pandoc", caption = "Two-Way Anova Summary")
        two_way_anova_summary = list(levene_kable,
                                   anderson_darling_kable,
                                   two_way_anova_kable)
        return(two_way_anova_summary)
      }

      ## Tukey == TRUE
      if(Tukey == TRUE){

        # Qualitative 1
        two_way_anova_tukey_qualitative = TukeyHSD(two_way_anova, which = "Qualitative")
        two_way_anova_tukey_table_qualitative1 = data.frame(unclass(two_way_anova_tukey_qualitative$Qualitative))
        two_way_anova_tukey_table_qualitative1 = round((two_way_anova_tukey_table_qualitative1),3)
        two_way_anova_tukey_table_qualitative1[, "P_symbols"] = "NA"
        colnames(two_way_anova_tukey_table_qualitative1) = c("Difference", "Lower", "Upper", "Adjusted", "Significance")
        two_way_anova_tukey_table_qualitative1 = two_way_anova_tukey_table_qualitative1 %>%
          mutate(
            Adjusted = if_else(Adjusted == 0, 0.001, Adjusted))
        colnames(two_way_anova_tukey_table_qualitative1) = c("Difference", "Lower", "Upper", "Adjusted", "Significance")
        rownames(two_way_anova_tukey_table_qualitative1) = c(row.names(two_way_anova_tukey_qualitative$Qualitative))

        for(i in seq_along(two_way_anova_tukey_table_qualitative1$Significance)){
          row = two_way_anova_tukey_table_qualitative1$Adjusted[i]
          row_symbol = two_way_anova_tukey_table_qualitative1$Significance[i]
          if(row <= 0.001){
            row_symbol = paste0("***")
          }else if(row <= 0.01){
            row_symbol = paste0("**")
          }else if(row <= 0.05){
            row_symbol = paste0("*")
          } else if(row <= 0.10){
            row_symbol = paste0(".")
          } else if(row <= ""){
            row_symbol = paste0(" ")
          } else
            row_symbol = paste0(" ")
          two_way_anova_tukey_table_qualitative1$Adjusted[i] = row
          two_way_anova_tukey_table_qualitative1$Significance[i] = row_symbol
        }
        colnames(two_way_anova_tukey_table_qualitative1) = c("Difference", "Lower 95", "Upper 95", "Adjusted P-Value", "Significance Symbol")

        # Qualitative 2
        two_way_anova_tukey_qualitative2 = TukeyHSD(two_way_anova, which = "Qualitative2")
        two_way_anova_tukey_table_qualitative2 = data.frame(unclass(two_way_anova_tukey_qualitative2$Qualitative2))
        two_way_anova_tukey_table_qualitative2 = round((two_way_anova_tukey_table_qualitative2),3)
        two_way_anova_tukey_table_qualitative2[, "P_symbols"] = "NA"
        colnames(two_way_anova_tukey_table_qualitative2) = c("Difference", "Lower", "Upper", "Adjusted", "Significance")
        two_way_anova_tukey_table_qualitative2 = two_way_anova_tukey_table_qualitative2 %>%
          mutate(
            Adjusted = if_else(Adjusted == 0, 0.001, Adjusted))
        colnames(two_way_anova_tukey_table_qualitative2) = c("Difference", "Lower", "Upper", "Adjusted", "Significance")
        rownames(two_way_anova_tukey_table_qualitative2) = c(row.names(two_way_anova_tukey_qualitative2$Qualitative2))

        ## Tukey significance Symbol Loop
        for(i in seq_along(two_way_anova_tukey_table_qualitative2$Significance)){
          row = two_way_anova_tukey_table_qualitative2$Adjusted[i]
          row_symbol = two_way_anova_tukey_table_qualitative2$Significance[i]
          if(row <= 0.001){
            row_symbol = paste0("***")
          }else if(row <= 0.01){
            row_symbol = paste0("**")
          }else if(row <= 0.05){
            row_symbol = paste0("*")
          } else if(row <= 0.10){
            row_symbol = paste0(".")
          } else if(row <= ""){
            row_symbol = paste0(" ")
          } else
            row_symbol = paste0(" ")
          two_way_anova_tukey_table_qualitative2$Adjusted[i] = row
          two_way_anova_tukey_table_qualitative2$Significance[i] = row_symbol
        }
        colnames(two_way_anova_tukey_table_qualitative2) = c("Difference", "Lower 95", "Upper 95", "Adjusted P-Value", "Significance Symbol")

        # Qualitative1:Qualitative2
        two_way_anova_tukey_qualitative_pair = TukeyHSD(two_way_anova, which = "Qualitative:Qualitative2")
        two_way_anova_tukey_table_qualitative_pair = data.frame(unclass(two_way_anova_tukey_qualitative_pair$`Qualitative:Qualitative2`))
        two_way_anova_tukey_table_qualitative_pair = round((two_way_anova_tukey_table_qualitative_pair),3)
        two_way_anova_tukey_table_qualitative_pair[, "P_symbols"] = "NA"
        colnames(two_way_anova_tukey_table_qualitative_pair) = c("Difference", "Lower", "Upper", "Adjusted", "Significance")
        two_way_anova_tukey_table_qualitative_pair= two_way_anova_tukey_table_qualitative_pair %>%
          mutate(
            Adjusted = if_else(Adjusted == 0, 0.001, Adjusted))
        colnames(two_way_anova_tukey_table_qualitative_pair) = c("Difference", "Lower", "Upper", "Adjusted", "Significance")
        rownames(two_way_anova_tukey_table_qualitative_pair) = c(row.names(two_way_anova_tukey_qualitative_pair$`Qualitative:Qualitative2`))

        ## Tukey significance Symbol Loop
        for(i in seq_along(two_way_anova_tukey_table_qualitative_pair$Significance)){
          row = two_way_anova_tukey_table_qualitative_pair$Adjusted[i]
          row_symbol = two_way_anova_tukey_table_qualitative_pair$Significance[i]
          if(row <= 0.001){
            row_symbol = paste0("***")
          }else if(row <= 0.01){
            row_symbol = paste0("**")
          }else if(row <= 0.05){
            row_symbol = paste0("*")
          } else if(row <= 0.10){
            row_symbol = paste0(".")
          } else if(row <= ""){
            row_symbol = paste0(" ")
          } else
            row_symbol = paste0(" ")
          two_way_anova_tukey_table_qualitative_pair$Adjusted[i] = row
          two_way_anova_tukey_table_qualitative_pair$Significance[i] = row_symbol
        }
        colnames(two_way_anova_tukey_table_qualitative_pair) = c("Difference", "Lower 95", "Upper 95", "Adjusted P-Value", "Significance Symbol")

        ## Summary String Title
        summary_title_string1 = "Two-Way Anova Tukey"
        summary_title_string2 = var_names[1]
        summary_title_string3 = var_names[2]
        summary_title_string4 = var_names[3]
        summary_title_string5 = "Summary"
        summary_title_qualitative = paste(summary_title_string1, summary_title_string2, summary_title_string5)
        summary_title_qualitative2 = paste(summary_title_string1, summary_title_string3, summary_title_string5)
        summary_title_qualitative_pair = paste(summary_title_string1, summary_title_string4, summary_title_string5)

        ## Listing the summaries
        two_way_anova_tukey_print_summary1 = kable(two_way_anova_tukey_table_qualitative1, format = "pandoc", caption = summary_title_qualitative)
        two_way_anova_tukey_print_summary2 = kable(two_way_anova_tukey_table_qualitative2, format = "pandoc", caption = summary_title_qualitative2)
        two_way_anova_tukey_print_summary_pair = kable(two_way_anova_tukey_table_qualitative_pair, format = "pandoc", caption = summary_title_qualitative_pair)
        two_way_anova_print_summary = kable(two_way_anova_table, format = "pandoc", caption = "Two-Way Anova Summary")
        two_way_anova_print_list = list(levene_kable,
                                        anderson_darling_kable,
                                        two_way_anova_print_summary,
                                        two_way_anova_tukey_print_summary1,
                                        two_way_anova_tukey_print_summary2,
                                        two_way_anova_tukey_print_summary_pair)

        ## Résultat Two-Way ANOVA | Tukey | No Log
        return(two_way_anova_print_list)
      }
    }
  }
  else{
    ## Log == TRUE
    if(Log == TRUE){
      Quantitative_Log = log(Quantitative)

      ## One-Way Anova
      if(missing(Qualitative2)){
        one_way_anova = aov(Quantitative_Log~Qualitative)
        par(mfrow = c(1,2))

        ## 1. Supposition homoscédasticité
        plot(residuals(one_way_anova)~fitted(one_way_anova), ylab = "Résidus", xlab = "Valeurs prédites", main = "Résidus vs valeurs prédites (Log)", cex.lab = 1.2)

        levene = leveneTest(Quantitative_Log ~ Qualitative)
        levene = round((levene),3)
        options(knitr.kable.NA = "")

        levene = levene %>%
          mutate(
            `Pr(>F)` = if_else(`Pr(>F)` == 0, 0.001, `Pr(>F)`))

        if(levene$`Pr(>F)`[1] <= 0.05){
          levene_title = "One-Way Anova Levene's Test for Homogeneity of Variance (center = median) (Log)\
        \
        WARNING: Evidence suggests that the variance across groups is statistically significantly different. \
        WARNING: We cannot assume the homogeneity of variances in the different treatment groups."
        } else {
          levene_title = "One-Way Anova Levene's Test for Homogeneity of Variance (center = median) (Log)\
        \
        There is no evidence to suggest that the variance across groups is statistically significantly different. \
        We can assume the homogeneity of variances in the different treatment groups."
        }
        levene_kable = kable(levene, caption = levene_title,
                             format = "pandoc")

        ## 2. Supposition de normalité des résidus
        qqnorm(residuals(one_way_anova), ylab = "Quantiles observés", xlab = "Quantiles théoriques", main = "Graphique quantile-quantile (Log)", cex.lab = 1.2)
        qqline(residuals(one_way_anova))

        one_way_anova_residuals = residuals(object = one_way_anova)
        anderson_darling = ad.test(one_way_anova_residuals)
        anderson_darling_table = cbind(anderson_darling$statistic, anderson_darling$p.value)
        colnames(anderson_darling_table) = c("Test Statistic", "p")
        rownames(anderson_darling_table) = c()
        anderson_darling_table = round((anderson_darling_table),3)
        anderson_darling_table = as.data.frame(anderson_darling_table)
        anderson_darling_table = anderson_darling_table %>%
          mutate(
            p = if_else(p == 0, 0.001, p))
        colnames(anderson_darling_table) = c("Test Statistic", "P-Value")

        if(anderson_darling$p.value <= 0.05){
          ad_title = "One-Way Anova Anderson-Darling Normality Test (Log)\
        \
        WARNING: We have sufficient evidence to reject the null hypothesis.
        WARNING: It is safe to say that the data doesn't follow a normal distribution."
        } else {
          ad_title = "One-Way Anova Anderson-Darling Normality Test (Log)\
        \
        We do not have sufficient evidence to reject the null hypothesis.
        It is safe to say that the data follows a normal distribution."
        }
        anderson_darling_kable = kable(anderson_darling_table, caption = ad_title, format = "pandoc")

        ## Transform One-Way Anova To DataFrame For Kable Summary
        one_way_anova_table = data.frame(unclass(summary(one_way_anova)))
        one_way_anova_table = round((one_way_anova_table),3)
        one_way_anova_table[, "P_symbols"] = "NA"
        colnames(one_way_anova_table) = c("Df", "Sum Sq", "Mean Sq", "F Value", "Pr(>F)", "P_symbols")
        one_way_anova_table[is.na(one_way_anova_table)] = "NA"
        one_way_anova_table = one_way_anova_table %>%
          mutate(
            `Pr(>F)` = if_else(`Pr(>F)` == "0", "0.001", `Pr(>F)`))
        rownames(one_way_anova_table) = c(var_names[1], "Residuals")

        ## Significance Symbol Loop
        for(i in seq_along(one_way_anova_table$P_symbols)){
          row = one_way_anova_table$`Pr(>F)`[i]
          row_symbol = one_way_anova_table$P_symbols[i]
          if(row <= 0.001){
            row_symbol = paste0("***")
          }else if(row <= 0.01){
            row_symbol = paste0("**")
          }else if(row <= 0.05){
            row_symbol = paste0("*")
          } else if(row <= 0.10){
            row_symbol = paste0(".")
          } else if(row <= ""){
            row_symbol = paste0(" ")
          } else
            row_symbol = paste0(" ")
          one_way_anova_table$`Pr(>F)`[i] = row
          one_way_anova_table$P_symbols[i] = row_symbol
        }

        ## Rearrange Table
        one_way_anova_table[one_way_anova_table == "NA"] = ""
        colnames(one_way_anova_table) = c("Df", "Sum Sq", "Mean Sq", "F Value", "Pr(>F)", "Significance Symbol")
        rownames(one_way_anova_table) = c(var_names[1], "Residuals")

        ## Tukey == FALSE
        if(missing(Tukey)||(Tukey == FALSE)){
          ## Résultat One-Way ANOVA | No Tukey | Log
          one_way_anova_kable = kable(one_way_anova_table, format = "pandoc", caption = "One-Way Anova Summary (Log)")
          one_way_anova_summary = list(levene_kable,
                                       anderson_darling_kable,
                                       one_way_anova_kable)
          return(one_way_anova_summary)
        }

        ## Tukey == TRUE
        if(Tukey == TRUE){

          one_way_anova_tukey_qualitative = TukeyHSD(one_way_anova, which = "Qualitative")
          one_way_anova_tukey_table = data.frame(unclass(one_way_anova_tukey_qualitative$Qualitative))
          one_way_anova_tukey_table = round((one_way_anova_tukey_table ),3)
          one_way_anova_tukey_table[, "P_symbols"] = "NA"
          colnames(one_way_anova_tukey_table) = c("Difference", "Lower", "Upper", "Adjusted", "Significance")
          one_way_anova_tukey_table = one_way_anova_tukey_table %>%
            mutate(
              Adjusted = if_else(Adjusted == 0, 0.001, Adjusted))
          colnames(one_way_anova_tukey_table) = c("Difference", "Lower", "Upper", "Adjusted", "Significance")
          rownames(one_way_anova_tukey_table) = c(row.names(one_way_anova_tukey_qualitative$Qualitative))
          one_way_anova_tukey_table

          ## Tukey significance Symbol Loop
          for(i in seq_along(one_way_anova_tukey_table$Significance)){
            row = one_way_anova_tukey_table$Adjusted[i]
            row_symbol = one_way_anova_tukey_table$Significance[i]
            if(row <= 0.001){
              row_symbol = paste0("***")
            }else if(row <= 0.01){
              row_symbol = paste0("**")
            }else if(row <= 0.05){
              row_symbol = paste0("*")
            } else if(row <= 0.10){
              row_symbol = paste0(".")
            } else if(row <= ""){
              row_symbol = paste0(" ")
            } else
              row_symbol = paste0(" ")
            one_way_anova_tukey_table$Adjusted[i] = row
            one_way_anova_tukey_table$Significance[i] = row_symbol
          }
          colnames(one_way_anova_tukey_table) = c("Difference", "Lower 95", "Upper 95", "Adjusted P-Value", "Significance Symbol")

          ## Summary String Title
          summary_title_string1 = "One-Way Anova Tukey"
          summary_title_string2 = var_names[1]
          summary_title_string3 = var_names[2]
          summary_title_string4 = "Summary (Log)"
          summary_title_qualitative = paste(summary_title_string1, summary_title_string2, summary_title_string4)
          summary_title_qualitative2 = paste(summary_title_string1, summary_title_string3, summary_title_string4)

          ## Listing the summaries

          one_way_anova_tukey_print_summary = kable(one_way_anova_tukey_table, format = "pandoc", caption = summary_title_qualitative)
          one_way_anova_print_summary = kable(one_way_anova_table, format = "pandoc", caption = summary_title_qualitative2)
          one_way_anova_print_list = list(levene_kable,
                                          anderson_darling_kable,
                                          one_way_anova_print_summary,
                                          one_way_anova_tukey_print_summary)

          ## Résultat One-Way ANOVA | Tukey  | Log
          return(one_way_anova_print_list)
        }
      }
      else {
        ## Two-Way Anova
        two_way_anova = aov(Quantitative_Log ~ Qualitative + Qualitative2 + Qualitative:Qualitative2)
        par(mfrow = c(1,2))

        ## 1. Supposition homoscédasticité
        plot(residuals(two_way_anova)~fitted(two_way_anova), ylab = "Résidus", xlab = "Valeurs prédites", main = "Résidus vs valeurs prédites (Log)", cex.lab = 1.2)
        levene = leveneTest(Quantitative_Log ~ Qualitative:Qualitative2)
        levene = round((levene),3)
        options(knitr.kable.NA = "")
        levene = levene %>%
          mutate(
            `Pr(>F)` = if_else(`Pr(>F)` == 0, 0.001, `Pr(>F)`))

        if(levene$`Pr(>F)`[1] <= 0.05){
          levene_title = "Two-Way Anova Levene's Test for Homogeneity of Variance (center = median) (Log)\
        \
        WARNING: Evidence suggests that the variance across groups is statistically significantly different. \
        WARNING: We cannot assume the homogeneity of variances in the different treatment groups."
        } else {
          levene_title = "Two-Way Anova Levene's Test for Homogeneity of Variance (center = median) (Log)\
        \
        There is no evidence to suggest that the variance across groups is statistically significantly different. \
        We can assume the homogeneity of variances in the different treatment groups."
        }
        levene_kable = kable(levene, caption = levene_title,
                             format = "pandoc")

        ## 2. Supposition de normalité des résidus
        qqnorm(residuals(two_way_anova), ylab = "Quantiles observés", xlab = "Quantiles théoriques", main = "Graphique quantile-quantile (Log)", cex.lab = 1.2)
        qqline(residuals(two_way_anova))

        two_way_anova_residuals = residuals(object = two_way_anova)
        anderson_darling = ad.test(two_way_anova_residuals)
        anderson_darling_table = cbind(anderson_darling$statistic, anderson_darling$p.value)
        colnames(anderson_darling_table) = c("Test Statistic", "p")
        rownames(anderson_darling_table) = c()
        anderson_darling_table = round((anderson_darling_table),3)
        anderson_darling_table = as.data.frame(anderson_darling_table)
        anderson_darling_table = anderson_darling_table %>%
          mutate(
            p = if_else(p == 0, 0.001, p))
        colnames(anderson_darling_table) = c("Test Statistic", "P-Value")

        if(anderson_darling$p.value <= 0.05){
          ad_title = "Two-Way Anova Anderson-Darling Normality Test (Log)\
        \
        WARNING: We have sufficient evidence to reject the null hypothesis.
        WARNING: It is safe to say that the data doesn't follow a normal distribution."
        } else {
          ad_title = "Two-Way Anova Anderson-Darling Normality Test (Log)\
        \
        We do not have sufficient evidence to reject the null hypothesis.
        It is safe to say that the data follows a normal distribution."
        }
        anderson_darling_kable = kable(anderson_darling_table, caption = ad_title, format = "pandoc")

        ## Transform Two-Way Anova To DataFrame For Kable Summary
        two_way_anova_table = data.frame(unclass(summary(two_way_anova)))
        two_way_anova_table = round((two_way_anova_table),3)
        two_way_anova_table[, "P_symbols"] = "NA"
        colnames(two_way_anova_table) = c("Df", "Sum Sq", "Mean Sq", "F Value", "Pr(>F)", "P_symbols")
        two_way_anova_table[is.na(two_way_anova_table)] = "NA"
        two_way_anova_table = two_way_anova_table %>%
          mutate(
            `Pr(>F)` = if_else(`Pr(>F)` == "0", "0.001", `Pr(>F)`))
        rownames(two_way_anova_table) = c(var_names[1],var_names[2],var_names[3], "Residuals")

        ## Significance Symbol Loop
        for(i in seq_along(two_way_anova_table$P_symbols)){
          row = two_way_anova_table$`Pr(>F)`[i]
          row_symbol = two_way_anova_table$P_symbols[i]
          if(row <= 0.001){
            row_symbol = paste0("***")
          }else if(row <= 0.01){
            row_symbol = paste0("**")
          }else if(row <= 0.05){
            row_symbol = paste0("*")
          } else if(row <= 0.10){
            row_symbol = paste0(".")
          } else if(row <= ""){
            row_symbol = paste0(" ")
          } else
            row_symbol = paste0(" ")
          two_way_anova_table$`Pr(>F)`[i] = row
          two_way_anova_table$P_symbols[i] = row_symbol
        }

        ## Rearrange Table
        two_way_anova_table[two_way_anova_table == "NA"] = ""
        colnames(two_way_anova_table) = c("Df", "Sum Sq", "Mean Sq", "F Value", "Pr(>F)", "Significance Symbol")

        ## Tukey == FALSE
        if(missing(Tukey)||(Tukey == FALSE)){

          ## Résultat Two-Way ANOVA | No Tukey | Log
          two_way_anova_kable = kable(two_way_anova_table, format = "pandoc", caption = "Two-Way Anova Summary (Log)")
          two_way_anova_summary = list(levene_kable,
                                       anderson_darling_kable,
                                       two_way_anova_kable)
          return(two_way_anova_summary)
        }

        ## Tukey == TRUE
        if(Tukey == TRUE){

          # Qualitative 1
          two_way_anova_tukey_qualitative = TukeyHSD(two_way_anova, which = "Qualitative")
          two_way_anova_tukey_table_qualitative1 = data.frame(unclass(two_way_anova_tukey_qualitative$Qualitative))
          two_way_anova_tukey_table_qualitative1 = round((two_way_anova_tukey_table_qualitative1),3)
          two_way_anova_tukey_table_qualitative1[, "P_symbols"] = "NA"
          colnames(two_way_anova_tukey_table_qualitative1) = c("Difference", "Lower", "Upper", "Adjusted", "Significance")
          two_way_anova_tukey_table_qualitative1 = two_way_anova_tukey_table_qualitative1 %>%
            mutate(
              Adjusted = if_else(Adjusted == 0, 0.001, Adjusted))
          colnames(two_way_anova_tukey_table_qualitative1) = c("Difference", "Lower", "Upper", "Adjusted", "Significance")
          rownames(two_way_anova_tukey_table_qualitative1) = c(row.names(two_way_anova_tukey_qualitative$Qualitative))

          for(i in seq_along(two_way_anova_tukey_table_qualitative1$Significance)){
            row = two_way_anova_tukey_table_qualitative1$Adjusted[i]
            row_symbol = two_way_anova_tukey_table_qualitative1$Significance[i]
            if(row <= 0.001){
              row_symbol = paste0("***")
            }else if(row <= 0.01){
              row_symbol = paste0("**")
            }else if(row <= 0.05){
              row_symbol = paste0("*")
            } else if(row <= 0.10){
              row_symbol = paste0(".")
            } else if(row <= ""){
              row_symbol = paste0(" ")
            } else
              row_symbol = paste0(" ")
            two_way_anova_tukey_table_qualitative1$Adjusted[i] = row
            two_way_anova_tukey_table_qualitative1$Significance[i] = row_symbol
          }
          colnames(two_way_anova_tukey_table_qualitative1) = c("Difference", "Lower 95", "Upper 95", "Adjusted P-Value", "Significance Symbol")

          # Qualitative 2
          two_way_anova_tukey_qualitative2 = TukeyHSD(two_way_anova, which = "Qualitative2")
          two_way_anova_tukey_table_qualitative2 = data.frame(unclass(two_way_anova_tukey_qualitative2$Qualitative2))
          two_way_anova_tukey_table_qualitative2 = round((two_way_anova_tukey_table_qualitative2),3)
          two_way_anova_tukey_table_qualitative2[, "P_symbols"] = "NA"
          colnames(two_way_anova_tukey_table_qualitative2) = c("Difference", "Lower", "Upper", "Adjusted", "Significance")
          two_way_anova_tukey_table_qualitative2 = two_way_anova_tukey_table_qualitative2 %>%
            mutate(
              Adjusted = if_else(Adjusted == 0, 0.001, Adjusted))
          colnames(two_way_anova_tukey_table_qualitative2) = c("Difference", "Lower", "Upper", "Adjusted", "Significance")
          rownames(two_way_anova_tukey_table_qualitative2) = c(row.names(two_way_anova_tukey_qualitative2$Qualitative2))

          ## Tukey significance Symbol Loop
          for(i in seq_along(two_way_anova_tukey_table_qualitative2$Significance)){
            row = two_way_anova_tukey_table_qualitative2$Adjusted[i]
            row_symbol = two_way_anova_tukey_table_qualitative2$Significance[i]
            if(row <= 0.001){
              row_symbol = paste0("***")
            }else if(row <= 0.01){
              row_symbol = paste0("**")
            }else if(row <= 0.05){
              row_symbol = paste0("*")
            } else if(row <= 0.10){
              row_symbol = paste0(".")
            } else if(row <= ""){
              row_symbol = paste0(" ")
            } else
              row_symbol = paste0(" ")
            two_way_anova_tukey_table_qualitative2$Adjusted[i] = row
            two_way_anova_tukey_table_qualitative2$Significance[i] = row_symbol
          }
          colnames(two_way_anova_tukey_table_qualitative2) = c("Difference", "Lower 95", "Upper 95", "Adjusted P-Value", "Significance Symbol")

          # Qualitative1:Qualitative2
          two_way_anova_tukey_qualitative_pair = TukeyHSD(two_way_anova, which = "Qualitative:Qualitative2")
          two_way_anova_tukey_table_qualitative_pair = data.frame(unclass(two_way_anova_tukey_qualitative_pair$`Qualitative:Qualitative2`))
          two_way_anova_tukey_table_qualitative_pair = round((two_way_anova_tukey_table_qualitative_pair),3)
          two_way_anova_tukey_table_qualitative_pair[, "P_symbols"] = "NA"
          colnames(two_way_anova_tukey_table_qualitative_pair) = c("Difference", "Lower", "Upper", "Adjusted", "Significance")
          two_way_anova_tukey_table_qualitative_pair= two_way_anova_tukey_table_qualitative_pair %>%
            mutate(
              Adjusted = if_else(Adjusted == 0, 0.001, Adjusted))
          colnames(two_way_anova_tukey_table_qualitative_pair) = c("Difference", "Lower", "Upper", "Adjusted", "Significance")
          rownames(two_way_anova_tukey_table_qualitative_pair) = c(row.names(two_way_anova_tukey_qualitative_pair$`Qualitative:Qualitative2`))

          ## Tukey significance Symbol Loop
          for(i in seq_along(two_way_anova_tukey_table_qualitative_pair$Significance)){
            row = two_way_anova_tukey_table_qualitative_pair$Adjusted[i]
            row_symbol = two_way_anova_tukey_table_qualitative_pair$Significance[i]
            if(row <= 0.001){
              row_symbol = paste0("***")
            }else if(row <= 0.01){
              row_symbol = paste0("**")
            }else if(row <= 0.05){
              row_symbol = paste0("*")
            } else if(row <= 0.10){
              row_symbol = paste0(".")
            } else if(row <= ""){
              row_symbol = paste0(" ")
            } else
              row_symbol = paste0(" ")
            two_way_anova_tukey_table_qualitative_pair$Adjusted[i] = row
            two_way_anova_tukey_table_qualitative_pair$Significance[i] = row_symbol
          }
          colnames(two_way_anova_tukey_table_qualitative_pair) = c("Difference", "Lower 95", "Upper 95", "Adjusted P-Value", "Significance Symbol")

          ## Summary String Title
          summary_title_string1 = "Two-Way Anova Tukey"
          summary_title_string2 = var_names[1]
          summary_title_string3 = var_names[2]
          summary_title_string4 = var_names[3]
          summary_title_string5 = "Summary (Log)"
          summary_title_qualitative = paste(summary_title_string1, summary_title_string2, summary_title_string5)
          summary_title_qualitative2 = paste(summary_title_string1, summary_title_string3, summary_title_string5)
          summary_title_qualitative_pair = paste(summary_title_string1, summary_title_string4, summary_title_string5)

          ## Listing the summaries
          two_way_anova_tukey_print_summary1 = kable(two_way_anova_tukey_table_qualitative1, format = "pandoc", caption = summary_title_qualitative)
          two_way_anova_tukey_print_summary2 = kable(two_way_anova_tukey_table_qualitative2, format = "pandoc", caption = summary_title_qualitative2)
          two_way_anova_tukey_print_summary_pair = kable(two_way_anova_tukey_table_qualitative_pair, format = "pandoc", caption = summary_title_qualitative_pair)

          two_way_anova_print_summary = kable(two_way_anova_table, format = "pandoc", caption = "Two-Way Anova Summary (Log)")
          two_way_anova_print_list = list(levene_kable,
                                          anderson_darling_kable,
                                          two_way_anova_print_summary,
                                          two_way_anova_tukey_print_summary1,
                                          two_way_anova_tukey_print_summary2,
                                          two_way_anova_tukey_print_summary_pair)

          ## Résultat Two-Way ANOVA | Tukey | Log
          return(two_way_anova_print_list)
        }
      }
    }
  }
}
