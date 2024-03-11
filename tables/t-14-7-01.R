##+ message = FALSE
# PACKAGES
pacman::p_load(tidyverse, haven, labelled)
pacman::p_load(gtsummary)

# THEME
theme_gtsummary_language('en', big.mark = '')
theme_gtsummary_compact()

# READ
advs_orig <- haven::read_xpt(
   'https://raw.githubusercontent.com/cdisc-org/sdtm-adam-pilot-project/master/updated-pilot-submission-package/900172/m5/datasets/cdiscpilot01/analysis/adam/datasets/advs.xpt')

advs <- advs_orig %>% 
   filter(SAFFL == 'Y', ANL01FL == 'Y') %>% 
   mutate(EOTFL = ifelse(AVISIT == 'End of Treatment', 'Y', ''),
          W24FL = ifelse(AVISIT == 'Week 24', 'Y', '') ) %>% 
   filter(EOTFL == 'Y' | W24FL == 'Y' | ABLFL == 'Y') %>%
   filter(PARAM %in% c('Diastolic Blood Pressure (mmHg)',
                       'Pulse Rate (BEATS/MIN)',
                       'Systolic Blood Pressure (mmHg)')) %>% 
   mutate(TRTP = factor(TRTP,
                        levels = c('Placebo', 'Xanomeline Low Dose', 'Xanomeline High Dose'),
                        labels = c('Placebo', 'Xan. Low', 'Xan. High')),
          AVISIT = factor(AVISIT,
                          levels = c('Baseline', 'Week 24', 'End of Treatment'),
                          labels = c('Baseline','Week 24', 'End of Trt.')),
          PARAM = factor(PARAM,
                         levels = c('Systolic Blood Pressure (mmHg)',
                                    'Diastolic Blood Pressure (mmHg)',
                                    'Pulse Rate (BEATS/MIN)'),
                         labels = c('Systolic Blood Pressure (mmHg)',
                                    'Diastolic Blood Pressure (mmHg)',
                                    'Pulse (bpm)'))
   ) %>% 
   select( PARAM, PARAMCD, USUBJID, ATPT, TRTP, AVISIT, AVAL)


# NEST
advs_n <- advs %>% 
   nest_by(PARAM, PARAMCD, ATPT, TRTP, AVISIT)

# STATS
my_stats <- function(data, variable, ...) {
   data %>% 
      select({{variable}}) %>% 
      drop_na() %>% 
      summarise( # n = length(AVAL),
         Mean = mean(AVAL),
         SD = sd(AVAL),
         Median = median(AVAL),
         `Min.` = min(AVAL),
         `Max.` = max(AVAL))
}

# TABLES
advs_t <- advs_n %>% 
   mutate( t = list( 
      tbl_summary(
         data = data,
         include = AVAL,
         label = list(AVAL ~ str_glue('{AVISIT}')),
         statistic = list(everything() ~ '{N_nonmiss}'),
         missing = 'no'
      ) %>% 
         add_stat(
            fns = everything() ~ my_stats
         )
   )
   )

# STACK
t <- with(advs_t,       
          tbl_stack(t, group_header = str_glue('{PARAM}_{ATPT}_{TRTP}'), quiet = TRUE) )

# MODIFY OBJECT TABLE
t$table_body <- t$table_body %>% 
   separate(groupname_col, 
            into = c('groupname_col_1',
                     'groupname_col_2',
                     'groupname_col_3' ),
            sep = '_') %>% 
   mutate(across(starts_with('groupname_col_'), 
                 ~case_when(
                    is.na(lag(.x))             ~ .x,
                    .x == lag(.x) ~ NA_character_, TRUE ~ .x) ) 
   )

# MODIFY OBJECT STYLING HEADER
t$table_styling$header <- 
   t$table_styling$header %>% 
   add_row( t$table_styling$header[1,] %>% mutate(column = 'groupname_col_1', label = 'Measure') ) %>% 
   add_row( t$table_styling$header[1,] %>% mutate(column = 'groupname_col_2', label = 'Position') ) %>% 
   add_row( t$table_styling$header[1,] %>% mutate(column = 'groupname_col_3', label = 'Treatment') ) %>% 
   slice(-1)

# PRINT
t %>% 
   modify_fmt_fun(
      list(
         Mean ~ \(x) style_number(x, digits = 1),
         SD ~ \(x) style_number(x, digits = 2),
         c('Median','Min.','Max.') ~ \(x) style_number(x, digits = 1) ) ) %>% 
   modify_header(
      label ~ '**Planned<br>Relative<br>Time**',
      stat_0 ~ '**n**',
      Mean ~ '**Mean**',              
      SD ~ '**SD**',                 
      Median~ '**Median**',             
      Min. ~ '**Min.**',               
      Max. ~ '**Max.**',  
      groupname_col_1  ~ '**Measure**',
      groupname_col_2 ~ '**Position**',
      groupname_col_3 ~ '**Treatment**') %>% 
   modify_table_styling(
      columns = label,
      rows = label == 'End of Trt.',
      footnote = 'End of treatment is the last on-treatment assessment of the specified vital sign (on or before the Week 24 visit).' ) %>% 
   modify_footnote(
      all_stat_cols() ~ NA) %>% 
   modify_column_alignment(where(is.numeric), 'right') %>% 
   modify_caption(
      '**Table 14-7.01<br>Summary of Vital Signs at Baseline and End of Treatment**')
