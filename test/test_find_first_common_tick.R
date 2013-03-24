# A script to test find_first_common_tick()


source("../find_first_common_tick.R")

ticks_1 <- read.csv("AUDCAD_1.csv")
ticks_1[, 1] <- as.POSIXct(ticks_1[, 1])

ticks_2 <- read.csv("AUDCAD_2.csv")
ticks_2[, 1] <- as.POSIXct(ticks_2[, 1])

print("Initial ticks data frame 1: ")
print(head(ticks_1))

print("Initial ticks data frame 2: ")
print(head(ticks_2))

print("Now let's find common first tick")

# sync up to first common tick
start <- find_first_common_tick(ticks_list)

print("The starting indexes are: ")
print(start)

ticks_1 <- ticks_1[start[1]:nrow(ticks_1), ]
ticks_2 <- ticks_2[start[2]:nrow(ticks_2), ]

print("The synced data frames are: ")
print(head(ticks_1))
print(head(ticks_2))

# After running source("test_find_first_common_tick.R"), you should get this:
# [1] "Initial ticks data frame 1: "
#                 timestamp     bid     ask
# 1 2012-11-12 21:55:01.664 1.04215 1.04295
# 2 2012-11-12 21:55:55.200 1.04218 1.04298
# 3 2012-11-12 21:56:15.394 1.04222 1.04302
# 4 2012-11-12 21:58:00.108 1.04225 1.04305
# 5 2012-11-12 21:58:14.213 1.04222 1.04302
# 6 2012-11-12 21:58:22.822 1.04218 1.04298
# [1] "Initial ticks data frame 2: "
#                 timestamp     bid     ask
# 1 2012-11-12 21:34:49.648 1.04232 1.04259
# 2 2012-11-12 21:34:52.773 1.04235 1.04262
# 3 2012-11-12 21:36:06.061 1.04232 1.04259
# 4 2012-11-12 21:36:06.311 1.04225 1.04252
# 5 2012-11-12 21:36:06.809 1.04228 1.04255
# 6 2012-11-12 21:36:12.184 1.04232 1.04259
# [1] "Now let's find common first tick"
# [1] "The starting indexes are: "
# [1]  1 36
# [1] "The synced data frames are: "
#                 timestamp     bid     ask
# 1 2012-11-12 21:55:01.664 1.04215 1.04295
# 2 2012-11-12 21:55:55.200 1.04218 1.04298
# 3 2012-11-12 21:56:15.394 1.04222 1.04302
# 4 2012-11-12 21:58:00.108 1.04225 1.04305
# 5 2012-11-12 21:58:14.213 1.04222 1.04302
# 6 2012-11-12 21:58:22.822 1.04218 1.04298
#                  timestamp     bid     ask
# 36 2012-11-12 21:55:01.687 1.04215 1.04295
# 37 2012-11-12 21:55:55.450 1.04218 1.04298
# 38 2012-11-12 21:56:15.670 1.04222 1.04302
# 39 2012-11-12 21:58:00.446 1.04225 1.04305
# 40 2012-11-12 21:58:14.523 1.04222 1.04302
# 41 2012-11-12 21:58:23.148 1.04218 1.04298
