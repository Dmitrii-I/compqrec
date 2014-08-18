source("../match_ticks.R")
source("../find_first_common_tick.R")
source("../lcs.R")	

ticks1 <- read.csv("AUDCAD1_for_match_ticks.csv", header = TRUE)
ticks2 <- read.csv("AUDCAD2_for_match_ticks.csv", header = TRUE)
ticks1[, 1] <- as.POSIXct(ticks1[, 1])
ticks2[, 1] <- as.POSIXct(ticks2[, 1])

matched_ticks <- match_ticks(ticks1, ticks2)


