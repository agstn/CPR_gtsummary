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
   CHEMISTRY  = adlbc_orig,
   HEMATOLOGY = adlbh_orig,
   .id = 'ORIG') %>% 
   filter(SAFFL == 'Y', ANL01FL == 'Y') %>% 
   filter(!is.na(PARAM), !is.na(TRTP), !is.na(BNRIND), !is.na(ANRIND), AVISITN != 99) 

adlb_n <- adlb %>% 
   mutate(ANRIND = factor(ANRIND, levels = c('N','H'), labels = c('Normal','High') ) ,
          BNRIND = factor(BNRIND, levels = c('N','H'), labels = c('Normal','High')),
          TRTP = factor(TRTP, levels = c('Placebo','Xanomeline Low Dose','Xanomeline High Dose')) ) %>% 
   select(ORIG, PARAM, PARAMCD, USUBJID, TRTP, ANRIND, BNRIND) %>% 
   drop_na(ANRIND, BNRIND) %>% 
   mutate(ALL = 1) %>% 
   filter(!str_starts(PARAMCD, '_') ) %>% 
   mutate(PARAM = str_remove_all(PARAM, '\\(.*?\\)') %>% str_trim() ) %>% 
   filter(PARAM %in% c('Alanine Aminotransferase', 
                       'Albumin', 
                       'Alkaline Phosphatase',
                       'Aspartate Aminotransferase',
                       'Bilirubin',
                       'Basophils', 
                       'Eosinophils',
                       'Hemoglobin',
                       'Lymphocytes')) %>% 
   nest_by(ORIG, PARAM, PARAMCD)

# CMH FUNCTION
my_cmh <- function(data, variable, by, ...) {
   table( data[['ANRIND']],
          data[['TRTP']],
          data[['BNRIND']] ) %>% 
      mantelhaen.test() %>% 
      broom::tidy() %>% 
      select(method, p.value)
}

# SUMMARISE: STACK
adlb_t <- adlb_n %>% 
   mutate(
      t = list(
         tbl_summary(
            data = data %>% mutate(inter = interaction(BNRIND, TRTP) ),
            by = inter,
            label = list(ALL ~ 'n'),
            include = c(ALL, ANRIND),
            statistic = list(
               ALL ~ '{n}',
               ANRIND ~ '{n} ({p}%)'),
            digits = list(
               ANRIND ~ c(0, 0)) ) %>% 
            add_stat(fns = list( ALL ~ my_cmh) ) )
   ) %>% 
   group_by(ORIG) %>% 
   summarise(t_orig = tbl_stack(tbls = t, group_header = PARAM, quiet = TRUE) %>% list() )

# PRINT
#+ results = 'asis'
for(i in 1){
   adlb_t$t_orig[[1]]  %>% 
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
      There were no subjects with abnormal low values at baseline.<br>
      A subject is counted only once for each analyte. A change will be considered <br>
      shifting from normal at baseline to abnormal or from abnormal at baseline to normal at any visit during the treatment.<br>
      The treatment period is defined as any planned visit after Week 0 (Visit 3), up to and including Week 24 (Visit 12).',
      p.value ~ 
         'CMH test for general association, controlling for status at baseline.') %>% 
      modify_column_indent(columns = label, undo = TRUE) %>% 
      modify_caption(
         '**Table 14-6.05 {adlb_t$ORIG[[i]]} <br>Shifts of Laboratory Values During Treatment, Categorized Based on Threshold Ranges**') %>% 
      bstfun::bold_italicize_group_labels(bold = TRUE) %>% 
      knitr::knit_print() %>% 
      cat()
}
