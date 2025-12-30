library(tabulapdf)
library(tidyverse)
library(knitr)

target <- file.path("Data/TACIR_pdf")
fc_files <- list.files(target)
fc_files <- file.path(target, fc_files)


start <- c(1, 1, 1, 1, 1, 1, 1, 1, 3, 3) ### first page of data
stop <- c(3, 4, 4, 4, 4, 3, 3, 3, 5, 6) ### last page of data
fy <- c(2017:2026)

input <- data.frame(files = fc_files, fy = fy,
                    start = start, stop = stop)

import_tacir_fc_data <- function(files, fy, start, stop) {
  df <- tabulapdf::extract_tables(file = files, 
                                  output = "matrix", 
                                  pages = start:stop,
                                  guess = TRUE) 
  
  df <- do.call(rbind, df) ##### creates one matrix per year
  
  df <- cbind(df, rep_len(fy, length.out = nrow(df))) 
}

fc <- purrr::pmap(input, import_tacir_fc_data)
fc <- do.call(rbind, fc) ##### Combine into one large matrix

fiscal_capacity <-  as.data.frame(fc)

df_names <- c("county", "rev_pp", "prop_pp", "sales_pp", "pci", 
              "res_farm_assess_ratio_pct", "adm_pop_ratio_pct", "fc_pp",
              "adm", "tot_fc", "fc_index", "prior_fy_fc_index_pct", "fy")

fiscal_capacity <- fiscal_capacity |> 
  setNames(df_names) |>
  mutate(across(rev_pp:fy, ~gsub(",", "", .x))) |>
  mutate(across(rev_pp:fy, ~gsub("\\$", "" , .x))) |>
  mutate(across(rev_pp:fy, ~gsub("%", "" , .x))) |>
  mutate(across(rev_pp:fy, ~gsub("[[:space:]]", "" , .x))) |>
  filter(!county == "County Area") |> # takes out residual col names
  filter(!county == "") |>
  mutate(across(rev_pp:fy, as.numeric)) |>
  mutate(across(res_farm_assess_ratio_pct:adm_pop_ratio_pct, ~.x/100))

fc_summary_stats <- fiscal_capacity |> 
  filter(county %in% c("Statewide", "Min", "Max")) |>
  pivot_longer(cols = rev_pp:prior_fy_fc_index_pct,
               names_to = "variable",
               values_to = "TACIR_value")

fiscal_capacity <- fiscal_capacity |>
  filter(!county %in% c("Statewide", "Min", "Max")) |>
  arrange(fy)

# write_csv(fiscal_capacity, "TACIR_fc_summary.csv")

###### Test accuracy of imported data - Max and min are accurate
###### It does not appear that "Statewide" is the average of the county
###### level data. The FY2026 summary statistics did not import.

check <- fiscal_capacity |>
  group_by(fy) |>
  summarize(across(rev_pp:prior_fy_fc_index_pct,
                   list(Min = ~min(.x, na.rm = TRUE),
                        Max = ~max(.x, na.rm = TRUE),
                        Statewide = ~mean(.x, na.rm = TRUE)),
                   .names = "{fn}_{col}")) |>
  pivot_longer(cols = -fy,
               names_to = "variable",
               values_to = "calculated_value") |>
  mutate(county = stringr::str_extract(variable, "^[^_]+")) |>
  mutate(variable = stringr::str_replace(variable, ".*?_", "")) |>
  inner_join(fc_summary_stats, by = c("fy", "variable", "county")) |>
  mutate(delta = TACIR_value - calculated_value)

############## Regression models
run_the_regs <- function(x){
  
  dat <- fiscal_capacity |>
    filter(fy == x) 
  
  reg <- lm(rev_pp ~ prop_pp + sales_pp + pci + 
              res_farm_assess_ratio_pct + adm_pop_ratio_pct,
            data = dat)
}

fy_range <- c(min(fiscal_capacity$fy):max(fiscal_capacity$fy))

regs <- lapply(fy_range, run_the_regs)

model_summaries <- lapply(regs, summary)

results <- lapply(model_summaries, function(x) {
  s <- data.frame(x$coefficients) |>
    mutate_all(round, digits = 4) |>
    mutate(display = paste0(Estimate, "/", t.value)) |>
    select(display)
  
  r <- data.frame(x$adj.r.square)
  names(r) <- "display"
  r$display <- round(r$display, digits = 4)
  r$display <- as.character(r$display)
  rownames(r) <- "adj. R square"
  
  s <- bind_rows(s, r)
  return(s)
}) |>
  reduce(bind_cols) |>
  setNames(paste0("FY", fy_range)) |>
  select(FY2017, FY2022, FY2026) 

knitr::kable(results)
