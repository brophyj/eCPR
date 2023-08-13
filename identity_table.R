# knitr::kable(df1, caption = "Table 1 Extracted ITT trial data", col.names=c("Trial", "Fail CPR", "Fail eCPR", "Success CPR", "Success eCPR"))


# create data frame
df <- tibble(Trial = c("INCEPTION", "INCEPTION","ARREST", "ARREST", "PRAGUE", "PRAGUE"), Tx = c("CPR", "eCPR", "CPR", "eCPR", "CPR", "eCPR"),
             fail = c(52, 56, 14, 8, 108, 86),
             success = c(10,14, 1, 6,24,38 )) %>% 
  mutate(total = fail + success,
         prop_success = success / total) 
df1 <- df[, c(1:4)]  %>% tidyr::pivot_wider(names_from = Tx, values_from = c(fail,success))

# per protocol
df_pp <- tibble(Trial = c("INCEPTION", "INCEPTION","ARREST", "ARREST", "PRAGUE", "PRAGUE"), Tx = c("CPR", "eCPR", "CPR", "eCPR", "CPR", "eCPR"),
                fail = c(48, 41, 15, 8, 108, 86),
                success = c(13,5, 0, 6,24,38 )) %>% 
  mutate(total = fail + success,
         prop_success = success / total) 
df1_pp <- df_pp[, c(1:4)]  %>% tidyr::pivot_wider(names_from = Tx, values_from = c(fail,success))

#default model (m)

# two other equivalent ways to get mean risk differences and sd
# in addition to epred_draws used in the models
# mean(tidy_draws(m)$b_grp2eCPR - tidy_draws(m)$b_grp2cCPR)
# sd(tidy_draws(m)$b_grp2eCPR - tidy_draws(m)$b_grp2cCPR)


tab_fct <- function(m=model) {
  m_diff <- (fixef(m)[2,1] - fixef(m)[1,1]) * 100
  m_diff_sd <- (sqrt(fixef(m)[2,2]^2 + fixef(m)[1,2]^2)) * 100
  m_diff_CI <- m_diff + c(-1,1) * 1.96 * m_diff_sd 
  p0 <- (1 - pnorm(0, m_diff, m_diff_sd)) * 100
  p1 <- (1 - pnorm(1, m_diff, m_diff_sd)) * 100
  p2 <- (1 - pnorm(-1, m_diff, m_diff_sd)) * 100
  equv <- p2 - p1
  list(m_diff, m_diff_CI[1], m_diff_CI[2], p0, p1, equv)
}

# or <-  (14/70) / (1 - (14/70)) / ((10/62) / (1 - (10/62))) = 1.3
# sd <- sqrt((1/14)+(1/56)+(1/10)+(1/52))   = 0.457
# exp(log(or) + c(-1,1) *1.96 *sd) = 0.531 3.182

# use above function to get values for manuscript Table
m_tbl <- tab_fct(m)
combined_tbl <- tab_fct(m_combined)
arrest_tbl <- tab_fct(m_arrest)
prague_tbl <- tab_fct(m_prague)

# raw data table onnly in supplemental material
raw_data <- gt(df1) %>%
  tab_header(
    title = "Table - Extracted ITT trial data"
  ) %>%
  cols_label(
    Trial = html("Trial"),
    fail_CPR = html("Fail CPR (n)"),
    fail_eCPR = html("Fail eCPR (n)"),
    success_CPR = html("Success CPR (n)"),
    success_eCPR = html("Success eCPR (n)"),
  ) %>%
  tab_style(style = cell_text(align = 'center'), locations = cells_column_labels()) %>%
  cols_align(
    align = c("center"),
    columns = c(fail_CPR, fail_eCPR, success_CPR, success_eCPR )
  ) %>%
  tab_source_note(
    source_note = "(e)CPR = (extracorporeal) cardiopulmonary resuscitation"
  ) 

gtsave(raw_data, "raw_data.png")
## Table 1

# manuscript risk difference table
table_rd <- 
  tibble(
    prior = c("Vague", "Enthusiastic", "Skeptical", "Combined"),
    pt_est = c(m_tbl[[1]], arrest_tbl[[1]], prague_tbl[[1]],combined_tbl[[1]]),
    llimit = c(m_tbl[[2]], arrest_tbl[[2]], prague_tbl[[2]],combined_tbl[[2]]),
    ulimit = c(m_tbl[[3]], arrest_tbl[[3]], prague_tbl[[3]],combined_tbl[[3]]),
    p.gt.1 = c(m_tbl[[4]], arrest_tbl[[4]], prague_tbl[[4]],combined_tbl[[4]]),
    p.gt.1.1 = c(m_tbl[[5]], arrest_tbl[[5]], prague_tbl[[5]],combined_tbl[[5]]),
    p.eq   = c(m_tbl[[6]], arrest_tbl[[6]], prague_tbl[[6]],combined_tbl[[6]])
  ) %>%
  mutate_if(is.numeric, ~ round(.,1))

table1 <- gt(table_rd) %>%
  tab_header(
    title = "Table 1 eCPR risk differences, 95% credible intervals and probabilities with various priors",
    subtitle = "ITT analyses"
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
    source_note = md("(PRAGUE trial stopped for futility for 6 month outcomes, but 30 day outcomes supportive of eCPR")
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
table1
gtsave(table1, "table1.png")

save(table1, file = "manuscript/table1.RDS")


 