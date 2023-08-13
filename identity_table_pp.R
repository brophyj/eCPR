# table of per protocol analyses
# need to run models first in brms_identity.R
# per protocol
df_pp <- tibble(Trial = c("INCEPTION", "INCEPTION","ARREST", "ARREST", "PRAGUE", "PRAGUE"), Tx = c("CPR", "eCPR", "CPR", "eCPR", "CPR", "eCPR"),
                fail = c(48, 41, 15, 8, 108, 86),
                success = c(13,5, 0, 6,24,38 )) %>% 
  mutate(total = fail + success,
         prop_success = success / total) 
df1_pp <- df_pp[, c(1:4)]  %>% tidyr::pivot_wider(names_from = Tx, values_from = c(fail,success))

tab_fct <- function(m=model) {
  m_diff <- (fixef(m)[2,1] - fixef(m)[1,1]) * 100
  m_diff_sd <- (sqrt(fixef(m)[2,2]^2 + fixef(m)[1,2]^2)) * 100
  m_diff_CI <- m_diff + c(-1,1) * 3.92 * m_diff_sd 
  p0 <- (1 - pnorm(0, m_diff, m_diff_sd)) * 100
  p1 <- (1 - pnorm(1, m_diff, m_diff_sd)) * 100
  p2 <- (1 - pnorm(-1, m_diff, m_diff_sd)) * 100
  equv <- p2 - p1
  list(m_diff, m_diff_CI[1], m_diff_CI[2], p0, p1, equv)
}

m_tbl_pp <- tab_fct(m_pp)
combined_tbl_pp <- tab_fct(m_combined_pp)
arrest_tbl_pp <- tab_fct(m_arrest_pp)
prague_tbl_pp <- tab_fct(m_prague_pp)

## Table Per protocol

#risk difference table
table_rd_pp <- 
  tibble(
    prior = c("Vague", "Enthusiastic", "Skeptical", "Combined"),
    pt_est = c(m_tbl_pp[[1]], arrest_tbl_pp[[1]], prague_tbl_pp[[1]],combined_tbl_pp[[1]]),
    llimit = c(m_tbl_pp[[2]], arrest_tbl_pp[[2]], prague_tbl_pp[[2]],combined_tbl_pp[[2]]),
    ulimit = c(m_tbl_pp[[3]], arrest_tbl_pp[[3]], prague_tbl_pp[[3]],combined_tbl_pp[[3]]),
    p.gt.1 = c(m_tbl_pp[[4]], arrest_tbl_pp[[4]], prague_tbl_pp[[4]],combined_tbl_pp[[4]]),
    p.gt.1.1 = c(m_tbl_pp[[5]], arrest_tbl_pp[[5]], prague_tbl_pp[[5]],combined_tbl_pp[[5]]),
    p.eq   = c(m_tbl_pp[[6]], arrest_tbl_pp[[6]], prague_tbl_pp[[6]],combined_tbl_pp[[6]])
  ) %>%
  mutate_if(is.numeric, ~ round(.,1))

table1pp <- gt(table_rd_pp) %>%
  tab_header(
    title = "Table 2 eCPR risk differences, 95% credible intervals and probabilities with various priors",
    subtitle = "per protocol analyses"
  ) %>% 
  tab_source_note(
    source_note = "Vague: default vague prior"
  ) %>%
  tab_source_note(
    source_note = "Combined: prior eCPR data from ARREST + PRAGUE"
  ) %>%
  tab_source_note(
    source_note = md("Enthusiastic: prior eCPR data from ARREST alone")
  ) %>%
  tab_source_note(
    source_note = md("Skeptical: prior eCPR data from PRAGUE alone")
  ) %>%
  tab_source_note(
    source_note = md("(PRAGUE trial stopped for futility at 6 months, but 30 day outcomes supportive of eCPR")
  ) %>%
  tab_source_note(
    source_note = md("ROPE: range of practical equivalence = + / - 1% RD (risk difference)")
  ) %>%
  tab_spanner(
    label = "Priors",
    columns = c(prior)
  ) %>%
  tab_spanner(
    label = "RD",
    columns = c(pt_est)
  ) %>%
  tab_spanner(
    label = "Probabilities",
    columns = c(p.gt.1, p.gt.1.1, p.eq)
  ) %>%
  tab_spanner(
    label = "95% CrI",
    columns = c(llimit, ulimit)
  ) %>%
  cols_label(
    prior = html(""),
    pt_est = html("point estimate"),
    llimit = html("lower limit"),
    ulimit = html("upper limit"),
    p.gt.1 = html("p(RD) >0 "),
    p.gt.1.1 = html("p(RD) >1% "),
    p.eq   = html(" p(ROPE)")
  ) %>%
  tab_style(style = cell_text(align = 'center'), locations = cells_column_labels()) %>%
  cols_align(
    align = c("center"),
    columns = c(pt_est, llimit, ulimit, p.gt.1.1, p.eq) 
  ) %>%
  fmt_number(
    columns = c(pt_est, llimit, ulimit, p.gt.1.1),
    decimals = 1,
    drop_trailing_zeros = FALSE
  ) 
table1pp
gtsave(table1pp, "table1pp.png")


