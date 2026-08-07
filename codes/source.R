# ------------------------------------------------------------------------------
# packages
# ------------------------------------------------------------------------------

library(arrow)
library(stringr)
library(data.table)
library(dplyr)
library(tidyr)
library(broom)

library(ggplot2)
library(viridis)
library(ggridges)
library(ggdist)
library(patchwork)
library(svglite)

library(lme4)
library(car)
library(performance)
library(survminer)    
library(survival)
library(scales)
library(multcomp)
library(coxme)
library(emmeans)
library(openxlsx)
library(irr)


euclid_dis <- function(x0, y0, x1, y1){
  sqrt((x0-x1)*(x0-x1) + (y0-y1)*(y0-y1))
}


round3 <- function(df){
  df |> mutate(across(where(is.numeric), \(x) round(x, 3)))
}
Anova_table <- function(fit){
  tibble(tidy(Anova(fit)), formula = deparse(formula(fit) )) |> 
    round3()
}
get_coef <- function(fit){
  res <- summary(fit)
  res$coefficients |> round(3)
}

