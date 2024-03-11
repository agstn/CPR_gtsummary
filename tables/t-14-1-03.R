#+ message = FALSE
# PACKAGES
pacman::p_load(tidyverse, haven)
pacman::p_load(gtsummary)

# THEME
theme_gtsummary_compact()

# IMPORT DATA
adsl_orig <- haven::read_xpt(
   'https://raw.githubusercontent.com/cdisc-org/sdtm-adam-pilot-project/master/updated-pilot-submission-package/900172/m5/datasets/cdiscpilot01/analysis/adam/datasets/adsl.xpt')

# LONG
adsl_l <- adsl_orig %>% 
   select(SUBJID, SITEGR1, SITEID, TRT01P, ITTFL, EFFFL, COMP24FL ) %>% 
   pivot_longer(cols = -c(SUBJID, SITEGR1, SITEID, TRT01P),
                names_to = 'DESC',
                values_to = 'YN') %>%
   mutate(YN = ifelse(YN == 'Y', 1, 0)) %>% 
   filter(YN == 1) %>% 
   mutate(DESC = factor(DESC, 
                        levels = c('ITTFL','EFFFL','COMP24FL'),
                        labels = c('ITT','Eff','Com'))) %>% 
   mutate(TRT01P = factor(TRT01P, 
                          levels = c('Placebo', 'Xanomeline Low Dose', 'Xanomeline High Dose'),
                          labels = c('Placebo', 'Xanomeline Low Dose', 'Xanomeline High Dose'))) %>% 
   mutate(SITE = str_glue('{SITEGR1} \U2014 {SITEID}'))

# TABLE
tbl_merge( 
   tbls = list(
      adsl_l %>% tbl_cross(row = SITE,
                           col = DESC,
                           margin = 'row'),
      
      adsl_l %>% filter(TRT01P == 'Placebo') %>% 
         tbl_cross(row = SITE,
                   col = DESC,
                   margin = 'row'),
      
      adsl_l %>% filter(TRT01P == 'Xanomeline Low Dose') %>% 
         tbl_cross(row = SITE,
                   col = DESC,
                   margin = 'row'),
      
      adsl_l %>% filter(TRT01P == 'Xanomeline High Dose') %>% 
         tbl_cross(row = SITE,
                   col = DESC,
                   margin = 'row')),
   tab_spanner = c('**<br>&nbsp;Total<br>(N=254)**', '**Xanomeline<br>Low Dose<br>(N=84)**', '**Xanomeline<br>High Dose<br>(N=84)**', '**<br>&nbsp;Placebo<br>(N=86)**')) %>% 
   modify_table_body(
      ~.x %>% 
         filter(!(variable == 'SITE' & row_type == 'label')) %>% 
         mutate(across(all_stat_cols(), ~ifelse(.x %in% c(0, NA), '0', .x) ) ) %>% 
         relocate(c(stat_1_1, stat_2_1, stat_3_1), .after = last_col())) %>% 
   modify_header(
      label           ~ '**Pooled \U2014 Site ID**',
      all_stat_cols() ~ '**{level}**') %>% 
   modify_footnote(
      starts_with('stat_1_') ~ 'ITT: Number of subjects in the ITT population',
      starts_with('stat_2_') ~ 'Eff: Number of subjects in the Efficacy population',
      starts_with('stat_3_') ~ 'Com: Number of subjects completing Week 24.') %>%
   modify_column_alignment(
      columns = all_stat_cols() , align = 'right') %>% 
   modify_caption(
      '**Table 14-1.03<br>Summary of Number of Subjects By Site**') 
