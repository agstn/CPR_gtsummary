#+ message = FALSE
# PACKAGES
pacman::p_load(tidyverse, haven)
pacman::p_load(gtsummary)

# THEME
theme_gtsummary_language('en', big.mark = '')
theme_gtsummary_compact()

# Read in ADSL
adsl_orig <-  haven::read_xpt(
   'https://raw.githubusercontent.com/cdisc-org/sdtm-adam-pilot-project/master/updated-pilot-submission-package/900172/m5/datasets/cdiscpilot01/analysis/adam/datasets/adsl.xpt') 


# STACK
adsl_l <- bind_rows(
   list(
      COMP24FL =  adsl_orig %>% filter(COMP24FL == 'Y') , 
      SAFFL    =  adsl_orig %>% filter(SAFFL == 'Y')
   ),
   .id = 'POP') %>% 
   select(POP, USUBJID, TRT01P, AVGDD, CUMDOSE) %>% 
   mutate(TRT01P = factor(TRT01P, 
                          levels = c('Placebo','Xanomeline Low Dose','Xanomeline High Dose'),
                          labels = c('Placebo', 'Xanomeline<br>Low Dose','Xanomeline<br>High Dose'))) %>% 
   mutate(POP = factor(POP,
                       labels = c('**Completers at Week 24** <br>',
                                  '**Safety Population** <br> <small>Includes completers and early terminators</small>') ))

# TABLE
adsl_l %>%
   tbl_strata(
      strata = POP,
      .tbl_fun =
         ~ .x %>%
         tbl_summary(
            by = TRT01P,
            include = c(AVGDD, CUMDOSE),
            label = list(AVGDD ~ 'Average daily dose (mg)',
                         CUMDOSE ~ 'Cumulative dose at end of study'),
            type = list(everything() ~ 'continuous2'),
            digits    = list(everything() ~ c(            0,        1,      2,          1,       1,       1)),
            statistic = list(everything() ~ c('{N_nonmiss}', '{mean}', '{sd}', '{median}', '{min}', '{max}'))) %>% 
         modify_header(
            label = '',
            all_stat_cols() ~ '**{level}<br>(n={n})**') %>% 
         modify_table_body(
            ~ .x %>%
               mutate(label = ifelse(row_type == 'level', str_to_lower(label), label) ) %>% 
               mutate(label = case_when(
                  label == 'minimum' ~ 'min',
                  label == 'maximum' ~ 'max',
                  .default = label))),
      .header = '**{strata}<br>(n={n})**',
      .combine_with = 'tbl_merge') %>% 
   modify_table_styling(
      columns = label,
      rows =  variable == 'CUMDOSE' & row_type  == 'label',
      footnote = 'End of Study refers to week 26/Early Termination.') %>% 
   modify_caption(
      '**Table 14-4.01<br> Summary of Planned Exposure to Study Drug, as of End of Study**' )


