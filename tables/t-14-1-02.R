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
                          labels = c('Placebo', 'Low Dose','High Dose'))) %>% 
   mutate(DCDECOD = case_when(
      COMP24FL == 'Y' & COMP24FL != 'Complete' ~ 'Completed',
      .default = DCDECOD)) %>% 
   mutate(DCDECOD = factor(DCDECOD) %>% 
             str_to_title() %>% 
             fct_recode(`Subject decided to withdraw` = 'Withdrawal By Subject') %>% 
             fct_relevel('Adverse Event',
                         'Death',
                         'Lack Of Efficacy',
                         'Lost To Follow-Up',
                         'Subject decided to withdraw')) %>% 
   mutate(COMP24FL = factor(COMP24FL, 
                            levels = c('Y','N'), 
                            labels = c('Completed Week 24','Early Termination (prior to Week 24)')) )

# TEST
p_loefl <- adsl %>%
   mutate(loefl = ifelse(DCREASCD %in% 'Lack of Efficacy', 1, 0)) %>%
   with(.,table(TRT01P, loefl)) %>% 
   fisher.test() %>% 
   broom::tidy() %>% 
   pull(p.value)

# TABLE
tbl_summary(
   data = adsl,
   by = TRT01P,
   include = c(COMP24FL, DCDECOD),
   label = list(COMP24FL = 'Completion Status',
                DCDECOD = 'Reason for Early Termination (prior to Week 24)'),
   missing = 'always',
   missing_text = 'Missing') %>% 
   modify_header(
      label = '',
      all_stat_cols(stat_0 = FALSE) ~ '**{level}<br>(N={n})**') %>% 
   add_overall(
      last = TRUE,
      col_label =  '**Total<br>(N={N})**') %>% 
   remove_row_type(
      variables = DCDECOD,
      type = 'level',
      level_value = 'Completed') %>% 
   add_p(
      test = list(COMP24FL ~ 'fisher.test',
                  DCDECOD  ~ 'chisq.test'),
      pvalue_fun = function(x) style_pvalue(x, digits = 3)) %>%
   modify_footnote(
      all_stat_cols() ~ NA,
      p.value ~ "Fisher's exact test.") %>% 
   modify_spanning_header(
      c(stat_2, stat_3) ~ '**Xanomeline**') %>% 
   modify_table_styling(
      columns = label,
      rows = label == 'Lack Of Efficacy',
      footnote = 'Based on either patient/caregiver perception or physician perception.') %>% 
   modify_table_body(
      ~.x %>% 
         mutate(label  = case_match(
            label,
            'Physician Decision'          ~ 'Physician decided to withdraw subject',
            'Study Terminated By Sponsor' ~ 'Sponsor decision',
            .default = label)) %>% 
         mutate(p.value  = ifelse(variable == 'DCDECOD' & label == 'Lack Of Efficacy', p_loefl, p.value )) %>% 
         mutate(across(all_stat_cols(), ~gsub('^0.*', '0 (0%)', .))) ) %>%  
   modify_caption(
      '**Table 14-1.02 <br> Summary of End of Study Data**') 
