#+ message = FALSE
# PACKAGES
pacman::p_load(tidyverse, haven)
pacman::p_load(gtsummary)

# THEME
theme_gtsummary_language('en', big.mark = '')
theme_gtsummary_compact()

# READ
adlbc <-  haven::read_xpt(
   'https://raw.githubusercontent.com/cdisc-org/sdtm-adam-pilot-project/master/updated-pilot-submission-package/900172/m5/datasets/cdiscpilot01/analysis/adam/datasets/adlbc.xpt') %>% 
   filter(SAFFL == 'Y' & (AVISITN != 99 | (AVISITN == 99 & AENTMTFL=='Y'))) 

adlbh <-  haven::read_xpt(
   'https://raw.githubusercontent.com/cdisc-org/sdtm-adam-pilot-project/master/updated-pilot-submission-package/900172/m5/datasets/cdiscpilot01/analysis/adam/datasets/adlbh.xpt') %>% 
   filter(SAFFL == 'Y' & (AVISITN != 99 | (AVISITN == 99 & AENTMTFL=='Y')) &
             !(PARAM %in% c('Anisocytes', 'Poikilocytes', 'Microcytes', 'Macrocytes', 'Polychromasia')))

# BIND
adlb <- bind_rows(
   CHEMISTRY = adlbc,
   HEMATOLOGY = adlbh,
   .id = 'ORIG') %>% 
   select(ORIG, PARCAT1, PARAM, PARAMCD, USUBJID , TRTA, AVISIT, AVISITN, AVAL, CHG) %>% 
   mutate(TRTA = factor(TRTA, 
                        levels = c('Placebo','Xanomeline Low Dose','Xanomeline High Dose'),
                        labels = c('Placebo', 'Low Dose','High Dose'))) %>% 
   mutate(AVISIT = str_trim(AVISIT)) %>% 
   mutate(AVISIT = factor(AVISIT) %>% 
             fct_reorder(AVISITN) ) 

# LONG
adlb_l <- adlb %>% 
   pivot_longer(cols = c(AVAL, CHG),
                names_to = 'VARIABLE',
                values_to = 'VALUE')

# NEST
adlb_n <- adlb_l %>% 
   nest_by(ORIG, PARCAT1, PARAM, PARAMCD, AVISIT, AVISITN, VARIABLE) 

# FUNCTION
add_by_n <- function(data, variable, by, tbl, ...) {
   data %>%
      select(all_of(c(variable, by))) %>%
      dplyr::group_by(.data[[by]]) %>%
      dplyr::summarise_all(~sum(!is.na(.))) %>%
      rlang::set_names(c("by", "variable")) %>%
      dplyr::left_join(
         tbl$df_by %>% select(by, by_col),
         by = "by"
      ) %>%
      mutate(
         by_col = paste0("add_n_", by_col),
         variable = style_number(variable)
      ) %>%
      select(-by) %>%
      tidyr::pivot_wider(names_from = by_col, 
                         values_from = variable)
}

# TABLE
adlb_t <- adlb_n %>% 
   filter(!str_starts(PARAMCD, "_")) %>% 
   mutate(t = case_when(
      VARIABLE == 'AVAL' ~ list( 
         tbl_summary(
            data = data,
            by = TRTA,
            include = VALUE,
            type = list(VALUE ~ 'continuous'),
            label = list(VALUE ~ str_glue('{AVISIT}')),
            statistic = list(VALUE ~ '{mean} ({sd})'),
            digits = list(VALUE ~ c(1, 2)),
            missing = 'no') %>% 
            add_stat(
               fns = everything() ~ add_by_n) %>% 
            modify_header(starts_with("add_n_stat") ~ "**N**") %>%
            modify_table_body(
               ~ .x %>%
                  dplyr::relocate(add_n_stat_1, .before = stat_1) %>%
                  dplyr::relocate(add_n_stat_2, .before = stat_2) %>% 
                  dplyr::relocate(add_n_stat_3, .before = stat_3)) ),
      .default =  list(
         tbl_summary(
            data = data,
            by = TRTA,
            include = VALUE,
            type = list(VALUE ~ 'continuous'),
            label = list(VALUE ~ str_glue('{AVISIT}')),
            statistic = list(VALUE ~ '{mean} ({sd})'),
            digits = list(VALUE ~ c(1, 2)),
            missing = 'no')
      ))
   )

# MERGE
adlb_t2 <- adlb_t %>% 
   select(-data) %>% 
   pivot_wider(
      names_from = VARIABLE,
      values_from = t) %>% 
   rowwise() %>% 
   mutate(
      BOTH = list(
         tbl_merge( tbls = list(AVAL, CHG ),
                    tab_spanner = FALSE) )
   )

# STACK & PRINT TABLE
#+ results = 'asis'

for( i in unique(adlb_t2$ORIG) ) {
   
   with(adlb_t2 %>% filter(ORIG == i),
        tbl_stack(BOTH,
                  group_header = str_glue('{PARAM}'),
                  quiet = TRUE) ) %>%
      modify_table_body(
         ~.x %>% 
            select(
               tbl_id1,
               groupname_col, label,
               add_n_stat_1_1, stat_1_1, stat_1_2,
               add_n_stat_2_1, stat_2_1, stat_2_2,
               add_n_stat_3_1, stat_3_1, stat_3_2,
               everything() ) %>% 
            mutate(across(all_stat_cols(), ~str_replace_all(., "NA \\(NA\\)", "")))
      ) %>% 
      modify_header(
         label ~ '',
         c(stat_1_1, stat_2_1, stat_3_1) ~ '**Mean (SD)**',
         c(stat_1_2, stat_2_2, stat_3_2) ~ '**Change<br>from Bsln<br>Mean (SD)**'
      ) %>% 
      modify_spanning_header(
         c(add_n_stat_1_1, stat_1_1, stat_1_2) ~ '**Placebo**',
         c(add_n_stat_2_1, stat_2_1, stat_2_2) ~ '**Xanomeline Low**',
         c(add_n_stat_3_1, stat_3_1, stat_3_2) ~ '**Xanomeline High**'
      ) %>% 
      modify_footnote(all_stat_cols() ~ NA) %>% 
      modify_table_styling(
         columns = label,
         rows =  variable == 'VALUE' & label == 'End of Treatment',
         footnote = "Last observed value while on treatment (prior to or at Week 24)"
      ) %>% 
      modify_caption(
         str_glue('**Table 14-6.01 {i}<br>Summary Statistics for Continuous Laboratory Values**')
      ) %>% 
      bstfun::bold_italicize_group_labels(bold = TRUE) %>% 
      knitr::knit_print() %>% 
      cat()
   
}
