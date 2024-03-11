#+ message = FALSE
# PACKAGES
pacman::p_load(tidyverse, haven)
pacman::p_load(gtreg, gtsummary)
# MMRM MODEL
pacman::p_load(emmeans, mmrm)

# THEME
theme_gtsummary_compact()

# IMPORT DATA
adqsadas_orig <- haven::read_xpt(
   'https://raw.githubusercontent.com/cdisc-org/sdtm-adam-pilot-project/master/updated-pilot-submission-package/900172/m5/datasets/cdiscpilot01/analysis/adam/datasets/adqsadas.xpt')

# DATA LOCF
adqsadas <- adqsadas_orig %>%
   filter(EFFFL == 'Y' & PARAMCD == 'ACTOT' & ANL01FL == 'Y' & DTYPE != 'LOCF' & AVISITN > 0) %>% 
   select(USUBJID, TRTP, SITEGR1, AVISIT, AVISITN, AVAL, BASE, CHG) %>% 
   mutate(USUBJID = factor(USUBJID)) %>% 
   mutate(across(c(USUBJID, SITEGR1, AVISIT), factor)) %>% 
   mutate(TRTP = factor(TRTP, 
                        levels = c('Placebo','Xanomeline Low Dose','Xanomeline High Dose'),
                        labels = c('Placebo', 'Low Dose','High Dose'))) %>% 
   mutate(AVISIT = factor(AVISIT,
                          levels = c( 'Week 8', 'Week 16', 'Week 24') ) )

# EMMEANS & BIND DATA
m <- mmrm(
   CHG ~ SITEGR1 + BASE * AVISIT + TRTP * AVISIT + us(AVISIT | USUBJID),
   data = adqsadas,
   reml = TRUE,
   control = mmrm_control(
      method = 'Kenward-Roger') )

e <- emmeans(m, ~TRTP, calc = c(n = '.wgt.'), infer = TRUE)
c <- contrast(e, method = 'revpairwise') %>% summary(infer = TRUE, adjust = 'none')

d <- bind_rows(
   e %>% data.frame() %>% rename(estimate = emmean),
   c %>% data.frame() %>% rename(TRTP = contrast)) %>% 
   select(-n, -df, -t.ratio)

# WIDE DATA
d_l <- d %>% 
   pivot_wider(values_from = -TRTP,
               names_from = TRTP) %>% 
   janitor::clean_names()

# TABLE
tbl_listing(
   data = d_l ) %>% 
   modify_column_merge('{estimate_placebo} ({se_placebo})') %>% 
   modify_column_merge('{estimate_low_dose} ({se_low_dose})') %>% 
   modify_column_merge('{estimate_high_dose} ({se_high_dose})') %>%
   modify_column_merge('{p_value_low_dose_placebo}<br>{estimate_low_dose_placebo}	({se_low_dose_placebo})<br>({lower_cl_low_dose_placebo};{upper_cl_low_dose_placebo})') %>% 
   modify_column_merge('{p_value_high_dose_placebo}<br>{estimate_high_dose_placebo}	({se_high_dose_placebo})<br>({lower_cl_high_dose_placebo};{upper_cl_high_dose_placebo})') %>%
   modify_column_merge('{p_value_high_dose_low_dose}<br>{estimate_high_dose_low_dose}	({se_high_dose_low_dose})<br>({lower_cl_high_dose_low_dose};{upper_cl_high_dose_low_dose})') %>%
   modify_column_hide(
      c('upper_cl_placebo','upper_cl_low_dose','upper_cl_high_dose',
        'lower_cl_placebo','lower_cl_low_dose','lower_cl_high_dose',
        'p_value_placebo','p_value_low_dose','p_value_high_dose')) %>% 
   modify_fmt_fun(
      list(
         starts_with( c('estimate_', 'upper_', 'lower_') ) ~ function(x) style_number(x, digits = 1),
         starts_with( 'se_') ~ function(x) style_number(x, digits = 2),
         starts_with( 'p_value_') ~ function(x) style_pvalue(x, digits = 3)),
      rows = !is.na(estimate_placebo)) %>% 
   modify_spanning_header(
      c(estimate_low_dose, estimate_high_dose) ~ '**Xanomeline**',
      c(p_value_low_dose_placebo:p_value_high_dose_low_dose) ~ '**p-value / Diff (SE) / (95% CI)**') %>% 
   modify_header(
      estimate_placebo ~ '**Placebo**',
      estimate_low_dose ~ '**Low Dose**',
      estimate_high_dose ~ '**High Dose**',
      p_value_low_dose_placebo	~ '**Low Dose - <br>Placebo**', 
      p_value_high_dose_placebo	~ '**High Dose - <br>Placebo**',
      p_value_high_dose_low_dose	~ '**High Dose - <br>Low Dose**') %>% 
   modify_column_alignment(
      columns = starts_with(c('est','p_val')), 'center') %>% 
   modify_footnote(
      update = list(
         c(p_value_low_dose_placebo,
           p_value_high_dose_placebo,
           p_value_high_dose_low_dose) ~ '
         Note: The change from baseline is calculated as the post-baseline score minus the baseline score. <br>The
         covariates included in the MMRM model are treatment, site group, time and treatment by time interaction,<br>
         baseline ADAS-Cog (11) score, and baseline ADAS-Cog (11) score by time interaction.' ))%>% 
   modify_caption(
      '**Table 14-3.11<br>ADAS Cog (11) - Repeated Measures Analysis of Change from Baseline to Week 24**')
