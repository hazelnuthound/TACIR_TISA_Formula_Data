library(readxl)
library(tidyverse)
library(zoo)

# More Annual Statistical Report years
# https://web.archive.org/web/20120417211140/http://www.tn.gov/education/reports_data.shtml

# Reports of TN Public Charter School Commission
# https://www.tn.gov/content/dam/tn/tn-public-charter-school-commission/documents/annual-reporting/annual-authorizer-report/2021-22%20TPCSC%20Annual%20Authorizer%20Report%20FINAL%20(updated6.15).pdf
# https://www.tn.gov/content/dam/tn/tn-public-charter-school-commission/documents/annual-reporting/annual-authorizer-report/2022-23%20TPCSC%20Annual%20Authorizer%20Report_2024.04.24.pdf
# https://www.tn.gov/content/dam/tn/tn-public-charter-school-commission/documents/annual-reporting/annual-authorizer-report/2023-24%20TPCSC%20Annual%20Authorizer%20Report_updated1.10.25_BF.pdf

# Data for Charter Schools Authorized by State Board of Education
# Charter School Report School Years 2017 - 2020
# https://www.tn.gov/content/dam/tn/education/documents/2019%20Charter%20Report%20final.pdf
# https://www.tn.gov/content/dam/tn/education/documents/Annual_Charter_Report_for_SY_2018-19_FINAL.pdf
# https://www.tn.gov/content/dam/tn/education/documents/Annual_Charter_Report_for_SY2019-20.pdf
# https://www.tn.gov/content/dam/tn/education/documents/Annual_Charter_Report_for_SY2020-21.pdf

# Archive of Charter School Reports
# https://digitalcommons.memphis.edu/govpubs-tn-dept-education-charter-schools-annual-report/2/

more_names <-  c("id", "name", "adm_k", "adm_1st", "adm_2nd", "adm_3rd",
                 "adm_4th", "adm_5th", "adm_6th", "adm_7th", "adm_8th",
                 "adm_9th", "adm_10th", "adm_11th", "adm_12th","adm_special", 
                 "adm_tot", "school_year")

less_names <- more_names[-1]


dirs <- list.dirs("Data/TN_Education_Dept", recursive = FALSE)
dirs2 <- list.dirs(dirs, recursive = FALSE)
dirs2_root <- str_remove(dirs2, "/[^/]+$")
dirs <- dirs[!dirs %in% dirs2_root]
dirs <- sort(c(dirs, dirs2))

files <- lapply(dirs, list.files)
school_years <- 2012:2024

table7 <- lapply(files, function(x) grep(pattern = "table 7", x, ignore.case = TRUE))

m <- 1:length(dirs)

adm <- purrr::map2(m, table7, function(x, y) files[[x]][y])

adm <- purrr::map2(dirs, adm, file.path)

import_adm <- function(x, sy) {
  y <- readxl::read_excel(x, skip = 4)
  y$sy <- sy
  
  if(ncol(y) == 18 ) {
    names(y) <- more_names
  } else{
    names(y) <- less_names
  }
  
  y <- y %>% mutate_all(as.character) # so bind_row will work
}

removal <- paste0(c("grand total", "special school district",
                    "average daily membership"),
                  collapse = "|")

## Gibson and Carrol county are messed up, have to add up systems, shelby county 
## has code  790 in 2013, and 792 otherwise 

adm <- purrr::map2(adm, school_years, import_adm) |>
  dplyr::bind_rows() |> 
  mutate(name = tolower(name)) |>
  mutate(id = str_pad(id, width = 3, side = "left", pad = "0")) |>
  mutate(id = ifelse(name == "shelby county", "792", id)) |>
  filter(!is.na(name)) |> 
  filter(!str_detect(name, removal)) |>
  mutate(across(adm_k:adm_tot, as.numeric))

systems <- adm |>
  select(name, id) |>
  distinct() |>
  filter(!is.na(id)) 

##### this dataframe is used to merge with TACIR data for comparison
systems_counties <- systems |>
  filter(str_detect(id, "0$")) |>
  bind_rows(data.frame(name = "shelby county", id = "790")) |>
  bind_rows(data.frame(name = "state", id = "980")) |>
  mutate(id = substr(id, 1, 2)) |>
  rename(county_id = id) |>
  mutate(name = str_replace(name, " county", "")) |>
  rename(county = name) 

adm_no_id <- adm |>
  filter(is.na(id)) |>
  select(-id) |>
  left_join(systems, by = "name")
  
adm <- adm |>
  filter(!is.na(id)) |>
  bind_rows(adm_no_id) |>
  mutate(county_id = substr(id, start = 1, stop = 2)) 
  
##### ASD (Achievement School District)
# LEAD Brick Church is in the ASD and in Davidson County
# Add to Davidson, subtract from Shelby
# School Year 2012-13 103
# School Year 2013-14 177
# School Year 2014-15 273
# School Year 2015-16 92+87+80+100 = 359
# School Year 2016-17 66+88+98+80 = 332
# School Year 2017-18 94+68+83+82 = 327
# School Year 2018-19 79+86+61+76 = 302
# School Year 2019-20 90+79+93+75 = 337
# School Year 2020-21 326
# School Year 2021-22 229
# School Year 2022-23 229
# School Year 2023-24 249

##### Charter Schools Authorized by TN State Board of Education
# School Year 2017-2018               Shelby 156
# School Year 2018-2019 Davidson 144  Shelby 305
# School Year 2019-2020 Davidson 425  Shelby 416
# School Year 2020-2021 Davidson 694  Shelby 565

##### TN Public Charter Commission Enrollment
# School Year 2021-22   Davidson 1275 Shelby 500
# School Year 2022-23   Davidson 1930 Shelby 1758
# School Year 2023-24   Davidson 2674 Shelby 2122

adm_out <- adm |>
  # allocate per enrollment
  mutate(county_id = ifelse(id == "985", "79", county_id)) |> #asd allocated to shelby county
  mutate(adm_tot = case_when(
    # Achievement School District allocation
    id == "190" & school_year == "2013" ~ adm_tot + 103,
    id == "792" & school_year == "2013" ~ adm_tot - 103,
    id == "190" & school_year == "2014" ~ adm_tot + 177,
    id == "792" & school_year == "2014" ~ adm_tot - 177,
    id == "190" & school_year == "2015" ~ adm_tot + 273,
    id == "792" & school_year == "2015" ~ adm_tot - 273,
    id == "190" & school_year == "2016" ~ adm_tot + 359,
    id == "792" & school_year == "2016" ~ adm_tot - 359,
    id == "190" & school_year == "2017" ~ adm_tot + 332,
    id == "792" & school_year == "2017" ~ adm_tot - 332,
    id == "190" & school_year == "2018" ~ adm_tot + 327,
    id == "792" & school_year == "2018" ~ adm_tot - 327,
    id == "190" & school_year == "2019" ~ adm_tot + 302,
    id == "792" & school_year == "2019" ~ adm_tot - 302,
    id == "190" & school_year == "2020" ~ adm_tot + 337,
    id == "792" & school_year == "2020" ~ adm_tot - 337,
    id == "190" & school_year == "2021" ~ adm_tot + 326,
    id == "792" & school_year == "2021" ~ adm_tot - 326,
    id == "190" & school_year == "2022" ~ adm_tot + 229,
    id == "792" & school_year == "2022" ~ adm_tot - 229,
    id == "190" & school_year == "2023" ~ adm_tot + 229,
    id == "792" & school_year == "2023" ~ adm_tot - 229,
    id == "190" & school_year == "2024" ~ adm_tot + 249,
    id == "792" & school_year == "2024" ~ adm_tot - 249,
    .default = adm_tot)) |>
  mutate(adm_tot = case_when(
    # TN State Board of Education authorized Charter School allocation
    id == "190" & school_year == "2019" ~ adm_tot + 144,
    id == "190" & school_year == "2020" ~ adm_tot + 425,
    id == "190" & school_year == "2021" ~ adm_tot + 694,
    id == "792" & school_year == "2018" ~ adm_tot + 156,
    id == "792" & school_year == "2019" ~ adm_tot + 305,
    id == "792" & school_year == "2020" ~ adm_tot + 416,
    id == "792" & school_year == "2021" ~ adm_tot + 565,
    .default = adm_tot)) |>
   mutate(adm_tot = case_when(
    # TN Public Charter Commission allocation
    id == "190" & school_year == "2022" ~ adm_tot + 1275,
    id == "190" & school_year == "2023" ~ adm_tot + 1930,
    id == "190" & school_year == "2024" ~ adm_tot + 2674,
    id == "792" & school_year == "2022" ~ adm_tot + 500,
    id == "792" & school_year == "2023" ~ adm_tot + 1758,
    id == "792" & school_year == "2024" ~ adm_tot + 2122,
    .default = adm_tot)) |>
  mutate(school_year = as.numeric(school_year)) |>
  select(name, county_id, school_year, adm_tot) |>
  group_by(county_id, school_year) |>
  summarise(adm = sum(adm_tot, na.rm = TRUE)) |>
  ungroup() |>
  arrange(county_id, school_year) |>
  group_by(county_id) |>
  mutate(adm_calculated = zoo::rollmean(adm, k = 3, align = "right", fill = NA)) |>
  mutate(fy_tacir = school_year + 2) |>
  inner_join(systems_counties, by = "county_id") |>
  select(-county) |>
  select(-c(adm)) |>
  rename(fy = fy_tacir)

compare <- fiscal_capacity |>
  mutate(county = tolower(county)) |>
  inner_join(systems_counties, by = "county") |>
  select(county, adm, fy, county_id) |> 
  inner_join(adm_join, by = c("county_id", "fy")) |>
  mutate(diff = adm - round(adm_calculated, 0)) |>
  mutate(pct_diff = (diff / round(adm)) * 100)

compare_notzero <- compare |> filter(pct_diff != 0)

