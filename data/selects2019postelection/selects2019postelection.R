library(tidyverse)
library(haven)
library(questionr)
library(devtools)

setwd("F:/master-thesis/data")

selectscum <- read_sav("495_Selects_CumulativeFile_Data_1971-2019_v2.3.0.sav") %>% 
  filter(year == 2019)
selects2019new_recoded <- read_sav("selects2019NEW/selects2019new_recoded.sav")

selects2019new$incomeqhh <- selectscum$income_hh
selects2019new$incomeq <- selectscum$income

selects2019Latest_filt <- 
selects2019Latest  
  

unique(selects2019new$userid - selectscum$useridpy)

write_sav(selects2019new, "selects2019new_recoded.sav")

names(selects2019new)

freq(selects2019new_recoded$op_taxes_hi)
freq(selects2019new_recoded$recop_env_vs_growth)


selects2019new_recoded <- 
  selects2019new_recoded %>% 
  mutate(
    recop_aband_fossfule = case_when(
      op_aband_fossfule %in% c(1,2) ~ "in_favor",
      is.na(op_aband_fossfule) | op_aband_fossfule == 3 ~ "neither_nor",
      op_aband_fossfule %in% c(4,5) ~ "against"
    ),
    recop_env_vs_growth = case_when(
      op_env_vs_growth %in% c(1,2) ~ "environment",
      is.na(op_env_vs_growth) | op_env_vs_growth == 3 ~ "neither_nor",
      op_env_vs_growth %in% c(4,5) ~ "econ_growth"
    ),
    recop_eq_op_foreign = case_when(
      op_eq_op_foreign %in% c(1,2) ~ "equal_opportunities",
      is.na(op_eq_op_foreign) | op_eq_op_foreign == 3 ~ "neither_nor",
      op_eq_op_foreign %in% c(4,5) ~ "better_chances_ch"
    ),
    recop_eu_int = case_when(
      op_eu_int %in% c(1,2) ~ "in_favor",
      is.na(op_eu_int) | op_eu_int == 3 ~ "neither_nor",
      op_eu_int %in% c(4,5) ~ "against"
    ),
    recop_minfranch_incr = case_when(
      op_minfranch_incr %in% c(1,2) ~ "in_favor",
      is.na(op_minfranch_incr) | op_minfranch_incr == 3 ~ "neither_nor",
      op_minfranch_incr %in% c(4,5) ~ "against"
    ),
    recop_modern_vs_trad_ch = case_when(
      op_modern_vs_trad_ch %in% c(1,2) ~ "modern_ch",
      is.na(op_modern_vs_trad_ch) | op_modern_vs_trad_ch == 3 ~ "neither_nor",
      op_modern_vs_trad_ch %in% c(4,5) ~ "defend_traditions"
    ),
    recop_retirement67 = case_when(
      op_retirement67 %in% c(1,2) ~ "in_favor",
      is.na(op_retirement67) | op_retirement67 == 3 ~ "neither_nor",
      op_retirement67 %in% c(4,5) ~ "against"
    ),
    recop_samesex_marriage = case_when(
      op_samesex_marriage %in% c(1,2) ~ "in_favor",
      is.na(op_samesex_marriage) | op_samesex_marriage == 3 ~ "neither_nor",
      op_samesex_marriage %in% c(4,5) ~ "against"
    ),
    recop_socspend = case_when(
      op_socspend %in% c(1,2) ~ "for_reduction",
      is.na(op_socspend) | op_socspend == 3 ~ "neither_nor",
      op_socspend %in% c(4,5) ~ "for_increase"
    ),
    recop_state_reduce_inc_ineq = case_when(
      op_state_reduce_inc_ineq %in% c(1,2) ~ "in_favor",
      is.na(op_state_reduce_inc_ineq) | op_state_reduce_inc_ineq == 3 ~ "neither_nor",
      op_state_reduce_inc_ineq %in% c(4,5) ~ "against"
    ),
    recop_statevsmarket = case_when(
      op_statevsmarket %in% c(1,2) ~ "state",
      is.na(op_statevsmarket) | op_statevsmarket == 3 ~ "neither_nor",
      op_statevsmarket %in% c(4,5) ~ "market"
    ),
    recop_taxes_hi = case_when(
      op_taxes_hi %in% c(1,2) ~ "for_increase",
      is.na(op_taxes_hi) | op_taxes_hi == 3 ~ "neither_nor",
      op_taxes_hi %in% c(4,5) ~ "for_reduction"
    )
  )

write_sav(selects2019new_recoded, "selects2019_postelection.sav")














