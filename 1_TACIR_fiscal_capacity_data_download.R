library(tidyverse)
library(httr)

fc_fy2026 <- "https://www.tn.gov/content/dam/tn/tacir/commission-meetings/2025june/2025June_Tab5FisCap_Memo.pdf"
fc_fy2025 <- "https://www.tn.gov/content/dam/tn/tacir/commission-meetings/20024may/2024May_Tab6FisCap_Memo.pdf"
fc_fy2024 <- "https://www.tn.gov/content/dam/tn/tacir/commission-meetings/2023june/2023June_Tab6Fiscap_Table1.pdf"
fc_fy2023 <- "https://www.tn.gov/content/dam/tn/tacir/commission-meetings/2022june/2022June_Tab9FisCap_Table1.pdf"
fc_fy2022 <- "https://www.tn.gov/content/dam/tn/tacir/commission-meetings/2021june/2021June_Tab6FisCap_Table1.pdf"
fc_fy2021 <- "https://www.tn.gov/content/dam/tn/tacir/commission-meetings/2020september/2020Sept_Tab5aFisCapTable1.pdf"
fc_fy2020 <- "https://www.tn.gov/content/dam/tn/tacir/commission-meetings/2019may/2019May_Tab6FisCapTable1.pdf"
fc_fy2019 <- "https://www.tn.gov/content/dam/tn/tacir/commission-meetings/may-2018/2018May_Tab6FiscalCapacityTable1.pdf"
fc_fy2018 <- "https://www.tn.gov/content/dam/tn/tacir/commission-meetings/may-2017/2017May_Tab7FiscapTable1.pdf"
fc_fy2017 <- "https://www.tn.gov/content/dam/tn/tacir/commission-meetings/2016-may/2016_Tab%209%202017%20Fiscap%20Table1_pages17-19.pdf"

fc_files <- mget(ls(pattern = "^fc_"))

##### Creates a directory name "Data" in the existing home directory.
if (!dir.exists("Data/TACIR_pdf")) {dir.create("Data/TACIR_pdf")}

download_TACIR_data <- function(name, link) {
  fname <- paste0("Data/TACIR_pdf/", name, ".pdf")
  httr::GET(link, write_disk(fname, overwrite = TRUE))
}

purrr::map2(names(fc_files), fc_files, download_TACIR_data)
