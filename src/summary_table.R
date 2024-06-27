###-----------------------------------------------------------------------------
### SUMMARY TABLE
###-----------------------------------------------------------------------------

## Function to get the summary table per column
get_summary_table_column <- function(cohort,
                                     colname,
                                     selected_rows,
                                     total_patients) {

  total_row <- data.table(
    category = "",
    colname = colname,
    n = dim(cohort)[1],
    prob = dim(cohort)[1] / total_patients,
    group = "total"
  )

  column_res <- lapply(
    selected_rows,
    function(x) {
      total <- dim(cohort)[1]
      res <- cohort[,
        .(
          n = .N,
          prob = .N / total
        ),
        by = `x`][order(get(x))]

      setnames(res, x, "category")

      return(res)
    }
  )


  names(column_res) <- selected_rows

  column_res <- rbindlist(column_res, idcol = "group")

  column_res$colname <- colname

  return(rbind(total_row, column_res))
}

##-----------------------------------------------------------------------------
## Build the data frame the information to show
##-----------------------------------------------------------------------------

selected_rows <- c(
  "sex",
  "age_group",
  "race",
  "dm2",
  "hypertension"
)

summary_table <- rbind(
  get_summary_table_column(
    cohort,
    "total",
    selected_rows,
    total_patients
  ),
  get_summary_table_column(
    cohort[
      covid == positive_label
    ],
    "covid",
    selected_rows,
    total_patients
  ),
  get_summary_table_column(
    cohort[
      obesity == "Y"
    ],
    "obese",
    selected_rows,
    total_patients
  ),
  get_summary_table_column(
    cohort[
      covid  == positive_label &
      obesity == "Y"
    ],
    "COVID_obese",
    selected_rows,
    total_patients
  )
)

summary_table[group == "age_group", group := "age group"]

summary_table[,
  colname := ordered(
    colname,
    c("total", "covid", "obese", "COVID_obese")
  )
]

summary_table <- summary_table %>%
  dcast(category + group ~ colname, value.var = c("n", "prob"))

summary_table[,
  group := ordered(
    group,
    c(
      "total",
      "sex",
      "age group",
      "race",
      "tobacco",
      "alcohol",
      "dm2",
      "hypertension")
  )
]


##-----------------------------------------------------------------------------
## Format and output the table
##-----------------------------------------------------------------------------

tab <- summary_table[order(group)] %>%
  mutate(group = tolower(group)) %>%
  group_by(group) %>%
  gt(rowname_col = "category")
merge_col_pattern <- "{1}<br>({2})"
tab <- tab %>%
  tab_header(
    title = md("**General demographics of patients**")
  ) %>%
  fmt_percent(
    columns = matches("prob_"),
    decimals = 1
  ) %>%
  fmt_number(
    columns = matches("n_"),
    decimals = 0
  ) %>%
  cols_merge(
    vars(n_total, prob_total),
    hide_columns = vars(prob_total),
    pattern = merge_col_pattern
  ) %>%
  cols_merge(
    vars(n_covid, prob_covid),
    hide_columns = vars(prob_covid),
    pattern = merge_col_pattern
  ) %>%
  cols_merge(
    vars(n_obese, prob_obese),
    hide_columns = vars(prob_obese),
    pattern = merge_col_pattern
  ) %>%
  cols_merge(
    vars(n_COVID_obese, prob_COVID_obese),
    hide_columns = vars(prob_COVID_obese),
    pattern = merge_col_pattern
  ) %>%
  cols_label(
    n_total = md("**Exposed**"),
    n_covid = md("**SARS-CoV-2 (+)**"),
    n_obese = md("**Obese**"),
    n_COVID_obese = md("**SARS-CoV-2 (+) + Obese**")
  ) %>%
  opt_all_caps() %>%
  tab_style(
    style = cell_text(size = px(17)),
    locations = cells_body(
      columns = matches("n_")
    )
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  ) %>%
  cols_width(
    matches("n_") ~ px(110)
  )


tab %>% gtsave("paper/obesity_age/demographics_table.html")
