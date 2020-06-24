## code to prepare `example_data_1` dataset goes here

przyklad <- read.delim("~/smoother/data-raw/test3.txt", stringsAsFactors = TRUE)

przyklad$grupa <- 'x'
colnames(przyklad) <- c('dlugosc', 'int', 'grupa')

usethis::use_data(przyklad, overwrite = TRUE)
