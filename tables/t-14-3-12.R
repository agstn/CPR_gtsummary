#+ message = FALSE
# PACKAGES
pacman::p_load(tidyverse, haven)
pacman::p_load(gt, gtsummary)
pacman::p_load(emmeans)

# THEME
theme_gtsummary_compact()

# OPTIONS
theme_gtsummary_language(
   language = 'en',
   decimal.mark = '.',
   ci.sep = '; ',
   iqr.sep = '; ',
   set_theme = TRUE
)

# IMPORT DATA
adqsnpix_orig <- haven::read_xpt(
   'https://raw.githubusercontent.com/cdisc-org/sdtm-adam-pilot-project/master/updated-pilot-submission-package/900172/m5/datasets/cdiscpilot01/analysis/adam/datasets/adqsnpix.xpt')

# SUBSET
adqsnpix <- adqsnpix_orig %>%
   filter(EFFFL == 'Y' & ITTFL == 'Y' & PARAMCD == 'NPTOTMN') %>%
   filter(AVISITN %in% c(0, 98)) %>% 
   mutate(TRTP = factor(TRTP, 
                        levels = c('Placebo','Xanomeline Low Dose','Xanomeline High Dose'),
                        labels = c('Placebo', 'Low Dose','High Dose'))) %>% 
   select(USUBJID, SITEGR1, TRTP, AVISITN, AVAL)

# TRANSFORM
adqsnpix <- adqsnpix %>% 
   mutate(AVISITN = factor(AVISITN, labels = c('BASE','AVAL'))) %>% 
   pivot_wider(names_from = AVISITN,
               values_from = AVAL)

# REGRESSION
lm_pairwise <- function(data, variable, by, adj.vars = c('BASE','SITEGR1'), ...) {
   f <- str_glue('{variable} ~ {paste(adj.vars,  collapse = " + ")} + {by}')
   t <- str_glue('~ {by}') %>% as.formula()
   l <- lm(as.formula(f), data = data)
   e <- l %>% 
      emmeans::emmeans(specs = t, calc = c(n = '.wgt.')) %>% 
      emmeans::contrast(method = 'revpairwise') %>% 
      summary(infer = TRUE, adjust = 'none')
   b <- e %>% 
      select( -df, -t.ratio) %>% 
      rename(label = contrast) %>% 
      pivot_wider(names_from = label,
                  values_from = -1) %>% 
      janitor::clean_names()
   b
}

lm_trend  <- function(data, variable, by, adj.vars = c('BASE','SITEGR1'),...){
   f <- str_glue('{variable} ~ {paste(adj.vars,  collapse = " + ")} + {by}')
   t <- str_glue('~ {by}') %>% as.formula()
   l <- lm(as.formula(f), 
           data = data %>% 
              mutate(!!sym(by) := factor(!!sym(by), labels = c(0, 54, 81)) %>% 
                        as.character() %>% 
                        as.numeric() ) )
   b <- l %>% broom::tidy(conf.int = TRUE) %>% slice(n())
   b %>% select(p.value)
}

# SUMMARY w/ PAIRWISE
tbl_summary(
   data = adqsnpix,
   by = TRTP,
   include = c(BASE, AVAL),
   statistic = list(everything() ~ c('{N_nonmiss}<br>{mean} ({sd})<br>{median} ({min};{max})')),
   digit = list(everything() ~ c(0, 1, 2, 1, 0, 0)),
   missing = 'no',
   label = list(BASE ~ 'Baseline',
                AVAL ~ 'Mean of Weeks 4-24')) %>% 
   add_stat_label(
      label = all_continuous() ~ c('n<br>Mean (SD)<br>Median (Range)') ) %>% 
   add_stat(fns = AVAL ~ lm_trend ) %>% 
   add_stat(fns = AVAL ~ lm_pairwise) %>% 
   modify_fmt_fun( list(
      starts_with('p.value')    ~ function(x) style_pvalue(x, digits = 3) ,
      starts_with('estimate_')  ~ function(x) style_number(x, digits = 1),
      starts_with('se_')        ~ function(x) style_number(x, digits = 2),
      starts_with('lower_cl')   ~ function(x) style_number(x, digits = 1),
      starts_with('upper_cl')   ~ function(x) style_number(x, digits = 1))) %>% 
   modify_column_merge(
      pattern = '{p_value_low_dose_placebo}<br>
      {estimate_low_dose_placebo} ({se_low_dose_placebo})<br>
      ({lower_cl_low_dose_placebo};{upper_cl_low_dose_placebo})<br>',
      rows = !is.na(estimate_low_dose_placebo)) %>% 
   modify_column_merge(
      pattern = '{p_value_high_dose_placebo}<br>
      {estimate_high_dose_placebo} ({se_high_dose_placebo})<br>
      ({lower_cl_high_dose_placebo};{upper_cl_high_dose_placebo})<br>',
      rows = !is.na(estimate_high_dose_placebo)) %>% 
   modify_column_merge(
      pattern = '{p_value_high_dose_low_dose}<br>
      {estimate_high_dose_low_dose} ({se_high_dose_low_dose})<br>
      ({lower_cl_high_dose_low_dose};{upper_cl_high_dose_low_dose})<br>',
      rows = !is.na(estimate_high_dose_low_dose)) %>% 
   modify_header(
      label = '',
      all_stat_cols() ~ '**{level}<br>(N={n})**',
      p.value ~ '**Trend Test**',
      p_value_low_dose_placebo ~ '**Low Dose -<br>Placebo**', 
      p_value_high_dose_placebo ~ '**High Dose -<br>Placebo**',
      p_value_high_dose_low_dose ~ '**High Dose -<br>Low Dose**') %>%    
   modify_spanning_header(
      c(stat_2, stat_3) ~ '<br><br>**Xanomeline**',
                p.value ~ '<br><br>**p-value**',
      starts_with('p_value_') ~ '**p-value<br>Diff of LS Means (SE)<br>95% CI**') %>% 
   modify_footnote(
      update = list(
         c(p.value, 
           p_value_low_dose_placebo,
           p_value_high_dose_placebo,
           p_value_high_dose_low_dose) ~ 'Based on Analysis of covariance (ANCOVA) model with treatment and site group as factors and baseline value as a covariate.<br>
                                          Pairwise comparison with treatment as a categorical variable: p-values without adjustment for multiple comparisons<br>
                                          Note that only assessments falling within the assessment window are included in the summary for a visit.',
         c(p.value)                    ~ 'Test for a non-zero coefficient for treatment (dose) as a continuous variable.') ) %>% 
   modify_caption(
      '**Table 14-3.12<br>Mean NPI-X Total Score from Week 4 through Week 24 - Windowed**') %>% 
   as_gt() %>% 
   gt::fmt_markdown(columns = -c(p.value))
