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
   filter(SAFFL == 'Y', ANL01FL == 'Y', AVISITN != 99) %>% 
   select(ORIG, PARCAT1, PARAM, PARAMCD, USUBJID , TRTA, AVISIT, AVISITN, AVAL, CHG, LBNRIND) %>% 
   mutate(TRTA = factor(TRTA, 
                        levels = c('Placebo','Xanomeline Low Dose','Xanomeline High Dose'),
                        labels = c('Placebo', 'Low Dose','High Dose'))) %>% 
   mutate(LBNRIND = factor(LBNRIND,
                           levels = c('LOW','NORMAL','HIGH'),
                           labels = c('Low','Normal','High')) ) %>% 
   mutate(PARAM = str_remove_all(PARAM, '\\(.*?\\)') %>% str_trim() ) %>% 
   mutate(PARAM = str_remove_all(PARAM, 'change from previous visit, relative to normal range') %>% str_trim() ) %>% 
   mutate(PARAM = str_remove_all(PARAM, 'change from previous visit, relative to normal rang') %>% str_trim() ) %>% 
   filter(!is.na(PARAM), !is.na(TRTA), !is.na(LBNRIND)) %>%
   filter(LBNRIND %in% c('Low','Normal','High')) %>% 
   filter(str_starts(PARAMCD, '_') )

# NEST
adlb_n <- adlb %>% 
   nest_by(ORIG) 

# TABLES
adlb_t <- adlb_n %>% 
   mutate(t_cross   = list(
      tbl_strata(
         data = data %>% set_variable_labels(PARAM = str_glue('{ORIG}')),
         strata = PARAM,
         .tbl_fun = 
            ~.x %>% 
            tbl_cross(row = TRTA,
                      col = LBNRIND,
                      margin = NULL,
                      percent = 'row',
                      digits = c(0, 0)) %>% 
            add_p(),
         .quiet = TRUE,
         .combine_with = 'tbl_stack'
      )
   ),
   t_summary = list(
      tbl_strata(
         data = data %>% set_variable_labels(PARAM = str_glue('{ORIG}')),
         strata = TRTA,
         .tbl_fun = 
            ~.x %>% 
            tbl_summary(
               by = LBNRIND,
               include = PARAM,
               percent = 'row',
               digits = everything() ~ c(0, 0) ) %>%
            add_p() %>% 
            modify_header(all_stat_cols() ~ '**{level}**'),
         .header = '**{strata}**',
         .quiet = TRUE
      )
   ),
   t_combine = list(
      t_summary %>% 
         modify_column_hide(columns = c(test_name_1, test_result_1, p.value_1,
                                        test_name_2, test_result_2, p.value_2)) %>% 
         modify_table_body(
            ~.x %>% 
               mutate(across(starts_with('stat_'), ~str_replace(., '0 \\(0%\\)', '0'))) %>% 
               select(-test_name_3, -test_result_3, -p.value_3) %>% 
               left_join(
                  t_cross$table_body %>% 
                     filter(!is.na(alternative)) %>% 
                     mutate(variable = 'PARAM',
                            var_label = str_glue('{ORIG}'),
                            row_type = 'level',
                            label = groupname_col,
                            test_name_3 = test_name,
                            test_results_3 = test_result,
                            p.value_3 = p.value) %>% 
                     select(variable, var_label, row_type, label, test_name_3, test_results_3,  p.value_3)
               ) 
         ) 
   )
   )

# STACK & PRINT
tbl_stack(tbls = adlb_t$t_combine) %>%
   bold_labels() %>%
   modify_column_alignment(
      all_stat_cols(), 'left') %>% 
   modify_spanning_header(
      c(p.value_3) ~ '**&nbsp;**') %>% 
   modify_fmt_fun(
      update = p.value_3 ~ function(x) style_pvalue(x, digits = 3)) %>% 
   modify_header(
      label  ~ '') %>% 
   modify_footnote(
      all_stat_cols() ~ 
         'Percentages are based on the number of subjects with non-missing assessments (i.e., the total of the
      subjects in the low, normal, and high categories) within each treatment group') %>% 
   modify_caption(
      '**Table 14-6.03<br>Frequency of Normal and Abnormal (Clinically Significant Change from Previous Visit) Laboratory Values During Treatment**')
