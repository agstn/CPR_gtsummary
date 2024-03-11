#+ message = FALSE
# PACKAGES
pacman::p_load(tidyverse, haven)
pacman::p_load(gtsummary)

# THEME
theme_gtsummary_compact()

# IMPORT DATA
adqsadas_orig <- haven::read_xpt(
   'https://raw.githubusercontent.com/cdisc-org/sdtm-adam-pilot-project/master/updated-pilot-submission-package/900172/m5/datasets/cdiscpilot01/analysis/adam/datasets/adqsadas.xpt')

# DATA LOCF
adqsadas_locf <- adqsadas_orig %>%
   filter(EFFFL == 'Y' & ITTFL=='Y' & PARAMCD == 'ACTOT' & ANL01FL == 'Y') %>% 
   filter(AVISITN %in% c(0, 8, 16, 24)) %>% 
   select(USUBJID, TRTP, AVISIT, AVISITN, AVAL, BASE, CHG)

# DATA WINDOWED
adqsadas_win <- adqsadas_orig %>%
   filter(EFFFL == 'Y' & ITTFL=='Y' & PARAMCD == 'ACTOT' & ANL01FL == 'Y') %>% 
   filter(AVISITN %in% c(0, 8, 16, 24)) %>% 
   filter(AVISITN != 0 & DTYPE != 'LOCF') %>% 
   select(USUBJID, TRTP, AVISIT, AVISITN, AVAL, BASE, CHG)

# LONG
adqsadas_l <- 
   bind_rows(
      LOCF     = adqsadas_locf,
      Windowed = adqsadas_win,
      .id = 'SET' ) %>% 
   mutate(TRTP = factor(TRTP, 
                        levels = c('Placebo','Xanomeline Low Dose','Xanomeline High Dose'),
                        labels = c('Placebo', 'Low Dose','High Dose')) ) %>% 
   mutate(AVISIT_SET = case_when(
      AVISIT == 'Baseline' ~ AVISIT,
      .default = str_glue('{AVISIT} ({SET})'))) %>% 
   mutate(AVISIT_SET = factor(AVISIT_SET) %>% 
             fct_relevel('Baseline',
                         'Week 8 (Windowed)', 'Week 16 (Windowed)', 'Week 24 (Windowed)',
                         'Week 8 (LOCF)', 'Week 16 (LOCF)', 'Week 24 (LOCF)') ) %>% 
   pivot_longer(cols = c(AVAL, BASE, CHG),
                names_to = 'VAR',
                values_to = 'AVAL',
                values_drop_na = TRUE)

# NEST
adqsadas_n <- adqsadas_l %>% 
   droplevels() %>% 
   nest_by(SET, TRTP, AVISITN, AVISIT, AVISIT_SET, VAR)

# TABLE BY STAT WIDE
stats <- c('N'    = '{length}', 
           'Mean' = '{mean}',
           'Std'  = '{sd}',
           'Med.' = '{median}',
           'Min.' = '{min}',
           'Max.' = '{max}')

adqsadas_t <- adqsadas_n %>% 
   mutate(t = list(
      purrr::imap(
         stats,
         ~data %>%
            tbl_summary(include = 'AVAL', 
                        missing = 'no', 
                        digits = list(AVAL ~ 5),
                        label = list(AVAL ~ str_glue('{AVISIT_SET}')),
                        statistic = ~.x)  %>%
            modify_header(all_stat_cols() ~ stringr::str_glue('**{.y}**'))
         ) %>%
         tbl_merge(tab_spanner = FALSE) %>%
         modify_footnote(~NA))
   )

# TRANS to WIDE
adqsadas_w <- adqsadas_t %>% 
   select(-data) %>% 
   pivot_wider(names_from = VAR,
               values_from = t) %>%
   rowwise() %>% 
   mutate(t_m = ifelse( is.null(CHG) ,
                        list( tbl_merge(tbls = list(AVAL, BASE) ) ),
                        list( tbl_merge(tbls = list(AVAL, BASE, CHG)) ) ) )

# TABLES STACK
adqsadas_s <- adqsadas_w %>% 
   group_by(AVISIT_SET, TRTP) %>% 
   summarise(t_s = list( 
      tbl_stack(t_m) ) )

# PRINT TABLE
with(adqsadas_s,
     tbl_stack( tbls = t_s,
                group_header = TRTP)) %>% 
   modify_fmt_fun(
      update = list(
         starts_with('stat_0_1_') ~ function(x) style_number(as.numeric(x), digits = 0),
         starts_with('stat_0_2_') ~ function(x) style_number(as.numeric(x), digits = 1),
         starts_with('stat_0_3_') ~ function(x) style_number(as.numeric(x), digits = 2),
         starts_with('stat_0_4_') ~ function(x) style_number(as.numeric(x), digits = 1),
         starts_with('stat_0_5_') ~ function(x) style_number(as.numeric(x), digits = 0),
         starts_with('stat_0_6_') ~ function(x) style_number(as.numeric(x), digits = 0)),
      rows = !is.na(stat_0_2_1) ) %>% 
   modify_column_hide(
      columns = c(stat_0_1_2,
                  stat_0_4_2,
                  stat_0_5_2,
                  stat_0_6_2,
                  
                  stat_0_1_3) ) %>% 
   modify_header(
      label ~ '') %>% 
   modify_spanning_header(
      ends_with('_1') ~ '&nbsp;',
      ends_with('_2') ~ '**Baseline**',
      ends_with('_3') ~ '**Change from baseline**')%>% 
   modify_caption(
      '**Table 14-3.10<br>ADAS Cog (11) - Mean and Mean Change from Baseline over Time**') %>% 
   bstfun::bold_italicize_group_labels(bold = TRUE)