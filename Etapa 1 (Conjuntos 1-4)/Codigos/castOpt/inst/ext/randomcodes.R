library(tidyverse)
library(castOpt)

test_matrix <- read_rds("inst/ext/test_matrix.rds")


sum <- 0

reg_beta(sum = sum, N = 4, x = c(1,1,1), data = test_matrix)
sum

