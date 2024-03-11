##+ message = FALSE
# PACKAGES
pacman::p_load(tidyverse, haven, labelled)
pacman::p_load(gtsummary)

# THEME
theme_gtsummary_language('en', big.mark = '')
theme_gtsummary_compact()

# READ
adlbhy_orig <-  haven::read_xpt(
   'https://raw.githubusercontent.com/cdisc-org/sdtm-adam-pilot-project/master/updated-pilot-submission-package/900172/m5/datasets/cdiscpilot01/analysis/adam/datasets/adlbhy.xpt')  

# COMBINE & SUBSEST
adlbhy <-adlbhy_orig  %>% 
   filter(SAFFL == 'Y', PARAMCD %in% c('TRANSHY', 'HYLAW'), !is.na(BASE), AVISITN > 0) %>% 
   group_by(USUBJID) %>%
   filter(AVAL    == max(AVAL)) %>%
   filter(AVISITN == max(AVISITN)) %>%
   ungroup()

adlbhy2 <- adlbhy %>%
   select(USUBJID, PARAMCD, PARAM, TRTP, BASE, AVAL) %>% 
   mutate(AVAL = factor(AVAL, labels = c('Normal','High') ) ,
          BASE = factor(BASE, labels = c('Normal','High')),
          TRTP = factor(TRTP, levels = c('Placebo','Xanomeline Low Dose','Xanomeline High Dose')) ) %>% 
   mutate(ALL = 1)

adlbhy_n <- adlbhy2 %>% 
   nest_by(PARAM, PARAMCD) %>% 
   arrange(desc(PARAM))

# CMH FUNCTION
my_cmh <- function(data, variable, by, ...) {
   table( data[['BASE']],
          data[['TRTP']],
          data[['AVAL']] ) %>% 
      mantelhaen.test() %>% 
      broom::tidy() %>% 
      select(method, p.value)
}

# SUMMARISE: STACK
adlbhy_t <- adlbhy_n %>% 
   mutate(
      t = list(
         tbl_summary(
            data = data %>% mutate(inter = interaction(BASE, TRTP) ),
            by = inter,
            label = list(ALL ~ 'n'),
            include = c(ALL, AVAL),
            statistic = list(
               ALL ~ '{n}',
               AVAL ~ '{n} ({p}%)'
            ),
            digits = list(
               AVAL ~ c(0, 0))) %>% 
            add_stat(fns = list( ALL ~ my_cmh) ) 
      )
   ) 

# PRINT
with(adlbhy_t,
     tbl_stack(tbls = t, group_header = PARAM, quiet = TRUE) )  %>% 
   modify_table_body(
      ~.x %>% 
         filter(!(row_type == 'label' & label != 'n')) %>% 
         mutate(across(starts_with('stat_'), ~str_replace_all(.x, '0 \\(0%\\)|0 \\(NA%\\)', '0'))) %>% 
         janitor::remove_empty(which = 'cols')
   ) %>% 
   modify_header(
      label ~ ' ',
      c(stat_1, stat_3, stat_5) ~ '**Normal at<br>Baseline**',
      c(stat_2, stat_4, stat_6) ~ '**High at<br>Baseline**',
      p.value ~ '**p-value**'
   ) %>% 
   modify_spanning_header(
      c(stat_1, stat_2) ~ '**Placebo (N=86)**',
      c(stat_3, stat_4) ~ '**Xan. Low (N=84)**',
      c(stat_5, stat_6) ~ '**Xan. High (N=84)**'
   ) %>% 
   modify_column_hide(method) %>% 
   modify_column_alignment(
      all_stat_cols(), 'left'
   ) %>%
   modify_fmt_fun( c(p.value) ~ function(x) style_pvalue(x, digits = 3) ) %>% 
   modify_footnote(
      all_stat_cols() ~ 
         'NOTES: Only subjects with baseline results are included in the summary.<br>
      The single subject with elevated transaminase and elevated bilirubin also had elevated alk phos (>3xULN).<br>
      There were no subjects with abnormal low values at baseline.<br>
      A subject is counted only once for each analyte. A change will be considered shifting from normal at<br>
      baseline to abnormal or from abnormal at baseline to normal at any visit during the treatment.<br> 
      The treatment period is defined as any planned visit after Week 0 (Visit 3), up to and including Week 24 (Visit 12).',
      p.value ~ 'CMH test for general association, controlling for status at baseline.') %>% 
   modify_column_indent(columns = label, undo = TRUE) %>% 
   modify_caption(
      "**Table 14-6.06<br>Shifts of Hy's Law Values During Treatment**") %>% 
   bstfun::bold_italicize_group_labels(bold = TRUE)
