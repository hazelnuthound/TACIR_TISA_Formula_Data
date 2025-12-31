##### Download Tennessee Department of Revenue Retail Sales Data
##### Data released quarterly, most recent file name will change
##### with new data released
##### Script will create a directory named "Data" in the existing 
##### home directory.  The "Data" directory is used as the data
##### source in the script that processes the data into a dataframe.

library(tidyverse)
library(httr)

a <- "https://www.tn.gov/content/dam/tn/education/documents/asr/asr_1112.zip"
b <- "https://www.tn.gov/content/dam/tn/education/documents/asr/asr_1213.zip"
c <- "https://www.tn.gov/content/dam/tn/education/documents/asr/asr_1314.zip"
d <- "https://www.tn.gov/content/dam/tn/education/documents/asr/asr_1415.zip"
e <- "https://www.tn.gov/content/dam/tn/education/documents/asr/asr_1516.zip"
f <- "https://www.tn.gov/content/dam/tn/education/documents/asr/asr_1617.zip"
g <- "https://www.tn.gov/content/dam/tn/education/documents/asr/asr_1718.zip"
h <- "https://www.tn.gov/content/dam/tn/education/documents/asr/Final%20Excels%2018-19.zip"
i <- "https://www.tn.gov/content/dam/tn/education/documents/asr/2020_ExcelFiles_upd.zip"
j <- "https://www.tn.gov/content/dam/tn/education/documents/asr/FinalExcels20-21.zip"
k <- "https://www.tn.gov/content/dam/tn/education/documents/asr/ASR_2021_22_ExcelFiles.zip"
l <- "https://www.tn.gov/content/dam/tn/education/documents/asr/ASR_2022_23_ExcelFiles.zip"
m <- "https://www.tn.gov/content/dam/tn/education/documents/asr/2023-24_ASR_Excel.zip"

objs <- mget(ls(), envir = .GlobalEnv)
objs <- as.data.frame(objs)

first <- 11:23
last <- 12:24
dirname <- paste0("asr_", first, last)

##### Creates a directory name "Data" in the existing home directory.
if (!dir.exists("Data/TN_Education_Dept")) {dir.create("Data/TN_Education_Dept")}

download_data <- function(x, y) {
  tmp = tempfile()
  httr::GET(x, write_disk(tmp, overwrite = TRUE))
  unzip(tmp, exdir = paste0("Data/TN_Education_Dept/", y))
}

purrr::map2(objs, dirname, download_data)
