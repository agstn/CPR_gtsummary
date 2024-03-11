##+ message = FALSE
# PACKAGES
pacman::p_load(tidyverse, haven, labelled)
pacman::p_load(gtreg, gtsummary)

# THEME
theme_gtsummary_language('en', big.mark = '')
theme_gtsummary_compact()

# READ
adsl_orig <- haven::read_xpt(
   'https://raw.githubusercontent.com/cdisc-org/sdtm-adam-pilot-project/master/updated-pilot-submission-package/900172/m5/datasets/cdiscpilot01/analysis/adam/datasets/adsl.xpt') %>% 
   mutate(ARM = factor(ARM,
                       levels = c('Placebo', 'Xanomeline Low Dose', 'Xanomeline High Dose'),
                       labels = c('Placebo<br>', 'Xanomeline<br>Low Dose', 'Xanomeline<br>High Dose')))

cm_orig <- haven::read_xpt(
   'https://raw.githubusercontent.com/cdisc-org/sdtm-adam-pilot-project/master/updated-pilot-submission-package/900172/m5/datasets/cdiscpilot01/tabulations/sdtm/cm.xpt')

cm <- left_join(
   cm_orig,
   adsl_orig %>% 
      select(USUBJID, ARM) ) %>% 
   mutate(CMDECOD_ALL = 'Patients receiving at least one concomitant medication') 

t1 <- tbl_ae(
   data = cm,
   id = USUBJID,
   ae = CMDECOD_ALL,
   strata = ARM,
   id_df = adsl_orig,
   statistic = '{n} ({p}%)',
   digits = c(0, 0),
   zero_symbol = '0') %>% 
   modify_header(
      label ~ '**Therapeutic class, n (%)**'
   )

t2 <- tbl_ae(
   data = cm,
   id = USUBJID,
   ae = CMDECOD,
   soc = CMCLAS,
   strata = ARM,
   id_df = adsl_orig,
   statistic = '{n} ({p}%)',
   digits = c(0, 0),
   zero_symbol = '0') 

tbl_stack(tbls = list(t1, t2), quiet = TRUE) %>% 
   modify_header(
      label ~ '**Therapeutic class, n (%)**') %>% 
   modify_caption(
      '**Table 14-7.04<br>Summary of Concomitant Medications (Number of Subjects)**')
