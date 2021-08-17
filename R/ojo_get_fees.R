

ojo_get_fees <- function(data) {

}


ojo_tbl("case") |>
  filter(case_type == "SC") |>
  left_join(ojo_tbl("minute"), by = c("id" = "case_id"), suffix = c("", ".minute")) |>
  filter(!is.na(amount)) |>
  select(id, description, amount)


ojo_fee_filter <- function(df) {

  filter_desc_terms <- c("CASH BOND",
                         "FORFEIT",
                         "WARR(E|A)NT RETUR",
                         "JAIL COSTS",
                         "CREDIT TIME SERVED",
                         "PAID BY DIS",
                         "DECEASED",
                         "ADJUSTING ENTRY",
                         "CASE NOT PROCESSED")

  filter_code_terms <- c("AC22",
                         "AC35",
                         "AC72",
                         "SFIJC",
                         "TR")

  filter_string_desc <- paste(filter_desc_terms, collapse = "|")
  filter_string_codes <- paste("\\b", filter_code_terms, "\\b", sep = "", collapse = "|")

  fdf <- df %>%
    filter(!str_detect(min_desc, filter_string_desc),
           !str_detect(min_code, filter_string_codes),
           fee_amt < 300000,
           fee_amt > 0)

  filtered_results <- df %>%
    mutate(exclusion = case_when(fee_amt > 300000 ~ "AMOUNT TOO HIGH (> $300,000)",
                                 str_detect(min_desc, filter_string_desc) ~ str_extract(min_desc, filter_string_desc),
                                 str_detect(min_code, filter_string_codes) ~ min_code,
                                 TRUE ~ as.character(NA)))  %>%
    group_by(exclusion) %>%
    filter(!is.na(exclusion)) %>%
    summarize(rows_filtered = n(),
              amt_filtered = sum(fee_amt, na.rm = TRUE))

  message(scales::comma(nrow(df) - nrow(fdf)), " (", round((nrow(df) - nrow(fdf))/nrow(df)*100, 1), "%) rows removed by ojo_fee_filter()")

  filtered_results %>%
    adorn_totals(where = "row") %>%
    print()

  return(fdf)
}
