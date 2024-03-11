#+ message = FALSE
# PACKAGES
pacman::p_load(tidyverse, haven)
pacman::p_load(gtsummary)

# THEME
theme_gtsummary_compact()

# DATA LOCF
adqscibc_orig <-  haven::read_xpt(
   'https://raw.githubusercontent.com/cdisc-org/sdtm-adam-pilot-project/master/updated-pilot-submission-package/900172/m5/datasets/cdiscpilot01/analysis/adam/datasets/adqscibc.xpt') 

adqscibc <- adqscibc_orig %>% 
   filter(EFFFL == 'Y' & ITTFL == 'Y', AVISITN %in% c(8, 16, 24) & ANL01FL=='Y') %>%
   select(USUBJID, TRTP, SITEGR1, AVISIT, AVISITN, AVAL) %>% 
   mutate(AVALC = factor(AVAL, levels = c(1:7) ) %>% 
             fct_recode('Marked improvement'   = '1',
                        'Moderate improvement' = '2',
                        'Minimal improvement'  = '3',
                        'No Change'            = '4',
                        'Minimal worsening'    = '5',
                        'Moderate worsening'   = '6',
                        'Marked worsening'     = '7') ) %>% 
   mutate(across(c(USUBJID, SITEGR1, AVISIT), factor)) %>% 
   mutate(TRTP = factor(TRTP, 
                        levels = c('Placebo','Xanomeline Low Dose','Xanomeline High Dose'),
                        labels = c('Placebo', 'Low Dose','High Dose'))) %>% 
   mutate(AVISIT = factor(AVISIT,
                          levels = c( 'Week 8', 'Week 16', 'Week 24') ) )

# cMH TEST
mntl_test <- function (data, variable, by, strata = 'SITEGR1', ...) {
   expr(
      stats::mantelhaen.test(
         !!data[[variable]], 
         as.factor(!!data[[by]]),
         as.factor(!!data[[strata]]) )) %>% 
      eval() %>% 
      broom::tidy() %>% 
      select(p.value, method)
}
# mntl_test(adqscibc %>% filter(AVISIT == 'Week 8'), 'AVAL', 'TRTP', strata = 'SITEGR1')

# SUMMARY
t_summary <- adqscibc %>% 
   tbl_strata2(
      strata = AVISIT,
      .tbl_fun = 
         ~ .x %>% 
         tbl_summary(
            include = AVALC,
            by = TRTP,
            label = list(AVALC = .y)) %>% 
         add_p( 
            test = AVALC ~ 'mntl_test',
            pvalue_fun = \(.) style_pvalue(., digits = 3)),
      .combine_with = 'tbl_stack',
      .combine_args = list(group_header = NULL),
      .quiet = TRUE)

# CROSS
adqscibc %>% 
   tbl_strata2(
      strata = AVISIT,
      .tbl_fun = 
         ~ .x %>% 
         tbl_cross(
            label = list(AVALC = .y ),
            row = AVALC,
            col = TRTP, 
            digits = c(0, 0),
            percent = 'column',
            margin = 'row',
            margin_text = 'n') %>%
         add_p( pvalue_fun = \(.) style_pvalue(., digits = 3)),
      .combine_with = 'tbl_stack',
      .combine_args = list(group_header = NULL)) %>%
   modify_table_body(
      ~ .x %>%
         mutate(across(starts_with('stat_'), ~ifelse(var_label=='n', str_replace(.x, '\\(100%\\)', ''), .x) ) ) %>% 
         mutate(across(starts_with('stat_'), ~ifelse(var_label!='n', str_replace(.x, '\\(0%\\)', ''), .x) ) ) %>% 
         rows_update(t_summary$table_body %>% filter(!is.na(p.value)),
                     by = c('tbl_id1', 'var_label', 'row_type', 'label') ) %>%
         slice(c(1,n(),2:8), .by = tbl_id1)) %>%
   modify_header( label ~ '**Assessment**',
                  all_stat_cols() ~ '**{level}**',
                  p.value         ~ '**p-value**') %>% 
   modify_spanning_header(
      stat_1            ~ '&nbsp;',
      c(stat_2, stat_3) ~ '**Xanomeline**' ) %>%
   modify_column_alignment(all_stat_cols(), 'left') %>% 
   # modify_column_indent(columns = label, undo = TRUE) %>% 
   modify_footnote(
      p.value ~ 'Overall comparison of treatments using CMH test (Pearson Chi-Square), controlling for site group.') %>% 
   modify_caption(
      '**Table 14-3.13<br>CIBIC+ - Categorical Analysis - LOCF**')
