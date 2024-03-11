#+ message = FALSE
# PACKAGES
pacman::p_load(tidyverse, haven)
pacman::p_load(gtsummary)

# THEME
theme_gtsummary_compact()

# IMPORT DATA
adsl_orig <- haven::read_xpt(
   'https://raw.githubusercontent.com/cdisc-org/sdtm-adam-pilot-project/master/updated-pilot-submission-package/900172/m5/datasets/cdiscpilot01/analysis/adam/datasets/adsl.xpt')

# UPDATE DATA
adsl <- adsl_orig %>% 
   mutate(TRT01P = factor(TRT01P, 
                          levels = c('Placebo','Xanomeline Low Dose','Xanomeline High Dose'),
                          labels = c('Placebo', 'Low Dose','High Dose')))

# TABLE
tbl_summary(
   data = adsl,
   by = TRT01P,
   include = c(ITTFL, SAFFL, EFFFL, COMP24FL, DCDECOD),
   value = list(everything() ~ 'Y',
                DCDECOD ~ 'COMPLETED'),
   label = list( ITTFL ~ 'Intent-To-Treat (ITT)', 
                 SAFFL ~ 'Safety', 
                 EFFFL ~ 'Efficacy', 
                 COMP24FL ~ 'Complete Week 24',
                 DCDECOD ~ 'Complete Study')) %>% 
   modify_header(
      label = '',
      all_stat_cols(stat_0 = FALSE) ~ '**{level}<br>(N={n})**') %>% 
   add_overall(
      last = TRUE,
      col_label =  '**Total<br>(N={N})**') %>% 
   modify_spanning_header(
      c(stat_2, stat_3) ~ '**Xanomeline**') %>% 
   modify_footnote(
      all_stat_cols() ~ 'N in column headers represents number of subjects entered in study (i.e., signed informed consent).') %>%
   modify_table_styling(
      columns = label,
      rows = label == "Intent-To-Treat (ITT)",
      footnote = "The ITT population includes all subjects randomized.") %>% 
   modify_table_styling(
      columns = label,
      rows = label == "Safety",
      footnote = "The Safety population includes all randomized subjects known to have taken at least one dose of randomized study drug.") %>% 
   modify_table_styling(
      columns = label,
      rows = label == "Efficacy",
      footnote = "The Efficacy population includes all subjects in the safety population who also have at least one post-baseline ADAS-Cog and CIBIC+ assessment.") %>% 
   modify_caption(
      '**Table 14-1.01 <br> Summary of Populations**')

