##+ message = FALSE
# PACKAGES
pacman::p_load(tidyverse, haven, labelled)
pacman::p_load(gtsummary)

# THEME
theme_gtsummary_language('en', big.mark = '')
theme_gtsummary_compact()

# READ
adlbc_orig <-  haven::read_xpt(
   'https://raw.githubusercontent.com/cdisc-org/sdtm-adam-pilot-project/master/updated-pilot-submission-package/900172/m5/datasets/cdiscpilot01/analysis/adam/datasets/adlbc.xpt')  

adlbh_orig <-  haven::read_xpt(
   'https://raw.githubusercontent.com/cdisc-org/sdtm-adam-pilot-project/master/updated-pilot-submission-package/900172/m5/datasets/cdiscpilot01/analysis/adam/datasets/adlbh.xpt')

# COMBINE & SUBSEST
adlb <- bind_rows(
   CHEMISTRY = adlbc_orig,
   HEMATOLOGY = adlbh_orig,
   .id = 'ORIG') %>% 
   filter(VISIT != 'SCREENING 1') %>% 
   filter(SAFFL == 'Y', AVISITN != 99) %>% 
   filter(!is.na(VISIT), !is.na(TRTA), !is.na(BNRIND), !is.na(ANRIND), !is.na(PARAM)) %>%
   mutate(TRTP = factor(TRTP, 
                        levels = c('Placebo','Xanomeline Low Dose','Xanomeline High Dose'),
                        labels = c('Placebo', 'Low Dose','High Dose'))) %>% 
   mutate(ANRIND = factor(ANRIND,
                          levels = c('N','H'),
                          labels = c('Normal','High')) ) %>% 
   mutate(BNRIND = factor(BNRIND,
                          levels = c('N','H'),
                          labels = c('Normal','High')) ) %>%
   mutate(VISIT = factor(VISIT) %>% fct_reorder(AVISITN)) %>% 
   mutate(PARAM = str_remove_all(PARAM, '\\(.*?\\)') %>% str_trim() ) %>% 
   select(ORIG, PARAM, PARAMCD, USUBJID , TRTP, VISIT, ANRIND, BNRIND) %>% 
   filter(!str_starts(PARAMCD, '_') ) %>% 
   filter(PARAM %in% c('Alanine Aminotransferase', 'Albumin', 'Basophils', 'Eosinophils'))

# NEST
adlb_n <- adlb %>% 
   nest_by(ORIG, PARAM, PARAMCD, TRTP, VISIT)

# TABLE
#+ message = FALSE
adlb_t <- adlb_n %>% 
   mutate(t_cross = list(
      tbl_cross(
         data = data,
         col = ANRIND,
         row = BNRIND,
         label = list(BNRIND ~ str_glue('{VISIT}') ),
         percent = 'row',
         margin = 'row',
         margin_text = 'n',
         missing = 'no') %>% 
         modify_table_body(
            ~.x %>% 
               arrange(row_type, var_type) ) )
   )

# SUMMARISE: STACK
adlb_w <- adlb_t %>% 
   select(-data) %>% 
   pivot_wider(names_from = TRTP,
               values_from = t_cross) %>%
   rowwise() %>% 
   mutate(t_cross = 
             tbl_merge(tbls = list(Placebo, `Low Dose`, `High Dose`),
                       tab_spanner  = c('**Placebo**','**Low Dose**','**High Dose**')) %>% list()
   ) %>% 
   group_by(ORIG, PARAM, PARAMCD) %>% 
   summarise(t_cross = tbl_stack(t_cross, group_header = NULL, quiet = TRUE) %>% list() ) %>% 
   group_by(ORIG) %>% 
   summarise(t_cross = tbl_stack(t_cross, group_header = PARAM, quiet = TRUE) %>% list() ) 

# PRINT
#+ results = 'asis'
for(i in 1:2){
   adlb_w$t_cross[[i]] %>% 
      modify_table_body(
         ~.x %>% 
            mutate(row_type = ifelse(label == 'n', 'level', row_type)) %>% 
            mutate(across(starts_with('stat_'), ~str_replace_all(.x, '0 \\(0%\\)|0 \\(NA%\\)', '0'))) %>% 
            mutate(across(starts_with('stat_'), 
                          ~case_when(
                             label == 'n' ~ str_replace_all(.x, ' \\(.*?\\)', ''),
                             .default = .x) ) ) ) %>% 
      modify_header(
         starts_with('stat_1_') ~ '**Normal at<br>Baseline**',
         starts_with('stat_2_') ~ '**High at<br>Baseline**') %>% 
      modify_footnote(
         all_stat_cols() ~ 'Only subjects with baseline results are included in the summary.<br>
                            There were no subjects with abnormal low values at baseline.') %>% 
      modify_column_alignment(
         all_stat_cols(), 'left') %>% 
      modify_caption(
         str_glue('**Table 14-6.04 {adlb_w$ORIG[[i]]}<br>Shifts of Laboratory Values During Treatment, Categorized Based on Threshold Ranges, by Visit**')) %>% 
      bstfun::bold_italicize_group_labels(bold = TRUE) %>% 
      knitr::knit_print() %>% 
      cat()
}

