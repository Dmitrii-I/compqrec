source("lcs.R")
v1 <- 1:20
v1 <- v1[-c(3, 6, 19)]

v2 <- 1:20
v2 <- v2[-c(4, 11, 18)]

df1 <- as.data.frame(matrix(rep(v1, 3), length(v1)))
df2 <- as.data.frame(matrix(rep(v2, 3), length(v2)))

print(lcs(df1, df2))
