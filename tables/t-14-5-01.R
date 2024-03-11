#+ message = FALSE
# PACKAGES
pacman::p_load(tidyverse, haven)
pacman::p_load(gtsummary, gtreg)

# THEME
theme_gtsummary_language('en', big.mark = '')
theme_gtsummary_compact()

# READ ADSL
adsl_orig <-  haven::read_xpt(
   'https://raw.githubusercontent.com/cdisc-org/sdtm-adam-pilot-project/master/updated-pilot-submission-package/900172/m5/datasets/cdiscpilot01/analysis/adam/datasets/adsl.xpt') 

adsl <- adsl_orig %>% 
   filter(SAFFL == 'Y') %>% 
   rename(TRTA = TRT01A) %>% 
   mutate(TRTA = factor(TRTA, 
                        levels = c('Placebo','Xanomeline Low Dose','Xanomeline High Dose'),
                        labels = c('Placebo', 'Low Dose','High Dose')))

# READ ADAE
adae_orig <- haven::read_xpt(
   'https://raw.githubusercontent.com/cdisc-org/sdtm-adam-pilot-project/master/updated-pilot-submission-package/900172/m5/datasets/cdiscpilot01/analysis/adam/datasets/adae.xpt') 

adae <- adae_orig %>% 
   filter(SAFFL == 'Y' & TRTEMFL == 'Y') %>% 
   mutate(TRTA = factor(TRTA, 
                        levels = c('Placebo','Xanomeline Low Dose','Xanomeline High Dose'),
                        labels = c('Placebo', 'Low Dose','High Dose'))) %>% 
   mutate(across(c(AEBODSYS, AETERM), ~str_to_title(.x))) 
   # %>% filter(AEBODSYS %in% c('Cardiac Disorders') )

# COMPLETE DATA AE SOC
cmpl_ <- .complete_ae_data(
   data = adae,
   id   = "USUBJID",
   ae   = "AETERM",
   soc  = "AEBODSYS",
   strata = 'TRTA',
   id_df = adsl) %>% 
   mutate(across(c(`..soc..`,`..ae..`), ~ factor(.x)) ) 

# COMPLETE DATA TT
cmpl_tt <- .complete_ae_data(
   data = adae %>% mutate(AETERM   = 'Any Body System',
                          AEBODSYS = 'Any Body System'),
   id   = "USUBJID",
   ae   = "AETERM",
   soc  = "AEBODSYS",
   strata = 'TRTA',
   id_df = adsl) %>% 
   mutate(across(c(`..soc..`,`..ae..`), ~ factor(.x)) ) 

# LONG w/ WITH TOTALS
cmpl_l <- 
   bind_rows(cmpl_,
             cmpl_tt) %>% 
   pivot_longer(cols = c(`..soc..`,`..ae..`)) %>% 
   mutate(ae = ifelse(name == '..soc..', str_glue('All {soc}'), str_glue('{ae}')) ) %>% 
   nest_by(name, soc, ae) %>% 
   filter(! (name == '..ae..' & ae == 'Any Body System')) %>% 
   arrange(soc, ae)


cmpl_tbl <- cmpl_l %>% 
   mutate(
      t_n  = list(
         tbl_summary(
            data = data,
            by = strata,
            include = `by`,
            value = list(`by` ~ 'Overall'),
            label = list(`by` ~ ae),
            statistic = list(everything() ~ '[{n}]')
         ) 
      ),
      t_3g = list( 
         tbl_summary(
            data = data %>% filter(value == TRUE),
            by = strata,
            value = list(`by` ~ 'Overall'),
            include = `by`,
            label = list(`by` ~ ae),
         ) 
      ),
      t_PvsL = list(
         tbl_summary(
            data = data %>% 
               filter(value == TRUE) %>% 
               filter(strata %in% c('Placebo','Low Dose')) %>% 
               droplevels(),
            by = strata,
            value = list(`by` ~ 'Overall'),
            include = `by`,
            label = list(`by` ~ ae),
         ) %>% 
            add_p(
               test = list(all_categorical() ~ 'fisher.test'),
               pvalue_fun = function(x) style_pvalue(x, digits = 3) 
            ) %>% 
            add_significance_stars(
               thresholds = c(0.15),
               hide_p = FALSE,
               pattern = "{p.value}{stars}"
            ) 
      ),
      t_PvsH = list(
         tbl_summary(
            data = data %>% 
               filter(value == TRUE) %>% 
               filter(strata %in% c('Placebo','High Dose')) %>% 
               droplevels(),
            by = strata,
            value = list(`by` ~ 'Overall'),
            include = `by`,
            label = list(`by` ~ ae),
         ) %>% 
            add_p(
               test = list(all_categorical() ~ 'fisher.test'),
               pvalue_fun = function(x) style_pvalue(x, digits = 3) 
            ) %>% 
            add_significance_stars(
               thresholds = c(0.15),
               hide_p = FALSE,
               pattern = "{p.value}{stars}"
            ) 
      ),
      t = list(
         tbl_merge( tbls = list(t_3g,
                                t_n,
                                t_PvsL %>% modify_column_hide(all_stat_cols()) ,
                                t_PvsH %>% modify_column_hide(all_stat_cols()) ),
                    tab_spanner = FALSE)
      )
      
   )

# STACK
with(cmpl_tbl,
     tbl_stack(tbls = t,
               quiet = TRUE) ) %>%
   modify_column_merge( "{stat_1_1} {stat_1_2}") %>% 
   modify_column_merge( "{stat_2_1} {stat_2_2}") %>% 
   modify_column_merge( "{stat_3_1} {stat_3_2}") %>% 
   modify_header(
      label = '',
      c(stat_1_1, 
        stat_2_1,
        stat_3_1) ~ '**{level}<br>(N = {n})**',
      p.value_3 ~ '**Placebo v<br>Low Dose**',
      p.value_4 ~ '**Placebo v<br>High Dose**') %>% 
   modify_spanning_header(
      c(stat_2_1, stat_3_1) ~ "**Xanomeline**",
      c(p.value_3, p.value_4) ~ "**Fisher's Exact p-values**") %>% 
   modify_table_body(
      ~ .x %>%
         mutate(stat_3_2_n =  str_extract(stat_3_2 , "\\d+") %>% as.numeric()) %>% 
         mutate(across(starts_with('stat_'), ~str_replace_all(.x, "\\[0\\]", ""))) %>%
         mutate(across(starts_with('stat_'), ~str_replace(., "0 \\(0%\\)", "0"))) %>% 
         slice(c(n(), 1:(n()-1)) ) ) %>% 
   modify_table_body(
      ~ .x %>%
         mutate(row_type = ifelse( label %in% label[!str_detect(label, "^All")], 'level','label') ) ) %>% 
   modify_column_alignment(columns = all_stat_cols(), align = "left") %>% 
   bold_labels() %>% 
   modify_footnote(
      all_stat_cols() ~ 
         "Treatment emergent events are defined as events which start on or after the start of treatment.<br>
      Adverse events are coded using MedDRA.<br>
      Percentages are based on the number of subjects in the safety population within each treatment group.<br>
      [AE] represents the total number of times an event was recorded.",
      starts_with('p.value') ~ 
   "P-values are based on Fisher's Exact test for the comparison of placebo versus each active treatment
      group. An asterisk is appended to p-values that are less than 0.15.") %>% 
   modify_caption('**Table 14-5.01<br>Incidence of Treatment Emergent Adverse Events by Treatment Group**')

