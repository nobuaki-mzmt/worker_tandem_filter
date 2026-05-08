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
library(patchwork)

library(survminer)    
library(survival)
library(scales)
library(multcomp)
library(coxme)


euclid_dis <- function(x0, y0, x1, y1){
  sqrt((x0-x1)*(x0-x1) + (y0-y1)*(y0-y1))
}