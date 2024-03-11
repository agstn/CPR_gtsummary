#+ message = FALSE
# PACKAGES
pacman::p_load(tidyverse, haven)
pacman::p_load(gtsummary)
pacman::p_load(ggsurvfit)

# THEME
theme_gtsummary_compact()

# READ ADSL
adtte_orig <-  haven::read_xpt(
   'https://raw.githubusercontent.com/cdisc-org/sdtm-adam-pilot-project/master/updated-pilot-submission-package/900172/m5/datasets/cdiscpilot01/analysis/adam/datasets/adtte.xpt')

# UPDATE
adtte <- adtte_orig %>% 
   filter(SAFFL == 'Y') %>% 
   filter(PARAMCD == 'TTDE') %>% 
   mutate(TRTA = factor(TRTA, 
                        levels = c('Placebo','Xanomeline Low Dose','Xanomeline High Dose'),
                        labels = c('Placebo', 'Xanomeline Low Dose','Xanomeline High Dose')))

# FIGURE
survfit2(Surv(AVAL, 1-CNSR) ~ TRTA, data = adtte) |>
   ggsurvfit(linewidth = 1) +
   add_quantile(y_value = 0.5, linetype = 'dotted', color = 'grey30', linewidth = 0.8) +
   add_censor_mark(size = 2,
                   show.legend = FALSE) +
   add_confidence_interval(show.legend = FALSE) +
   add_pvalue(caption = 'Logrank {p.value}',
              location = 'annotation',
              x = 15, y = 0.05,
              size = 3) +
   add_risktable(
      risktable_stats = c('n.risk')
   ) +
   add_risktable_strata_symbol(symbol = '\U25AC', size = 16) +
   scale_ggsurvfit() +
   labs(title = 'KM plot for Time to First Dermatologic Event',
        subtitle = 'Safety population',
        y = 'Survival Probability (%)',
        x = 'Time (Days)') +
   guides(colour = ggh4x::guide_stringlegend(ncol = 1)) +
   theme(legend.position = c(0.99, 0.99),
         legend.justification = c('right','top'),
         legend.background = element_rect(fill = 'transparent'))

# TABLE
tbl_merge(
   tbls = list(
      tbl_summary(
         data = adtte,
         include = TRTA,
         statistic = list(all_categorical() ~ '{n}')
      ) %>% 
         modify_header(all_stat_cols() ~ '**No Subjects**<br>{n}'),
      tbl_summary(
         data = adtte %>% mutate(CNSR = factor(CNSR, label = c('Event','Censored'))),
         by = CNSR,
         include = TRTA,
         statistic = list(all_categorical() ~ '{p}% ({n})'),
         percent = 'row'
      ) %>% 
         modify_header(all_stat_cols() ~ '**{level}**<br>{n} ({style_percent(p)}%)'),
      tbl_survfit(
         survfit2(Surv(AVAL, 1-CNSR) ~ TRTA, data = adtte, conf.type = 'log-log'),
         probs = 0.5,
         label_header = '**Median Survival**',
         estimate_fun  = function(x) style_number(x, digits = 1),
      ) %>% 
         modify_header(all_stat_cols() ~ '**Median Survival (95% CL)**')
   ),
   tab_spanner = FALSE) %>%
   modify_header(
      label ~ '') %>% 
   modify_footnote(
      all_stat_cols() ~ NA,
      stat_1_2 ~ 'Dermatologic events were identified as adverse events associated with skin conditions such as rash, pruritus, dermatitis. A full list of adverse event terms is presented in the final study report') %>% 
   modify_table_body(
      ~.x %>% 
         slice(-1) %>% 
         mutate(row_type = 'label'))
