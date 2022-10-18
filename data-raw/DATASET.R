## code to prepare `DATASET` dataset goes here
colRm <- load('R/sysdata.rda')
usethis::use_data(colRm, overwrite = TRUE)