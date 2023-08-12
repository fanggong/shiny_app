ks_test <- function(dat) {
  res <- ks.test(dat, y = "pnorm")
  res <- data.table::data.table(
    Statistic = .format_statistic(res$statistic),
    Df = "#TODO",
    `Sig.(2-tailed)` = .format_statistic(res$p.value)
  )
  res
}

sw_test <- function(dat) {
  if (sum(!is.na(dat)) < 3 || sum(!is.na(dat)) > 5000) {
    res <- data.table::data.table(
      Statistic = "",
      Df = "",
      `Sig.(2-tailed)` = ""
    )
  } else {
    res <- shapiro.test(dat)
    res <- data.table::data.table(
      Statistic = .format_statistic(res$statistic),
      Df = "#TODO",
      `Sig.(2-tailed)` = .format_statistic(res$p.value)
    )
  }
  res
}


norm_test <- function(ks_res, sw_res, var_x, var_y) {
  merge(ks_res, sw_res, by = "Add", suffixes = c("_x", "_y")) %>%
    gt(rowname_col = "analyze") %>% 
    tab_header(title = paste0("Normality Test for ", var_y)) %>%
    tab_spanner(
      label = "Kolmogorov-Smirnov Test",
      columns = c("Statistic_x", "Df_x", "Sig.(2-tailed)_x")
    ) %>% 
    tab_spanner(
      label = "Shapiro-Wilk Test",
      columns = c("Statistic_y", "Df_y", "Sig.(2-tailed)_y")
    ) %>% 
    cols_label(
      Statistic_x = "Statistic",
      Statistic_y = "Statistic",
      Df_x = "Df",
      Df_y = "Df",
      `Sig.(2-tailed)_x` = "Sig.(2-tailed)",
      `Sig.(2-tailed)_y` = "Sig.(2-tailed)"
    ) %>% 
    cols_align(
      align = "center"
    ) %>%
    cols_width(
      c(2:3, 5:6) ~ px(80),
      c(4,7) ~ px(160)
    ) %>% 
    tab_options(
      heading.title.font.weight = "bolder",
      heading.title.font.size = 25,
      table.border.top.width = 0,
      table.border.bottom.width = 0,
      heading.border.bottom.width = 0,
      column_labels.font.weight = "bolder",
      column_labels.border.bottom.width = 0,
      column_labels.border.top.color = "#000",
      column_labels.border.top.width = "2px",
      table_body.border.top.color = "#000",
      table_body.border.top.width = "1px",
      table_body.hlines.width = 0,
      table_body.border.bottom.color = "#000",
      table_body.border.bottom.width = "2px",
    )
}


levene_test <- function(dat) {
  res <- car::leveneTest(dat$analyze, dat$group)
  data.table::data.table(
    Statistic = .format_statistic(res$`F value`[1]),
    `P-value` = .format_statistic(res$`Pr(>F)`[1])
  )
}


t_test <- function(dat) {
  res <- t.test(analyze ~ group, data = dat, var.equal = TRUE)
  res <- data.table::data.table(
    Statistic = .format_statistic(res$statistic),
    Df = res$parameter,
    `P-value` = .format_statistic(res$p.value),
    `Mean diff` = .format_statistic(res$estimate[1] - res$estimate[2]),
    Lower = .format_statistic(res$conf.int[1]),
    Upper = .format_statistic(res$conf.int[2])
  )
  res
}
