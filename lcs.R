# Given two data frames this function returns longest common subsequence (LCS). 
# The LCS is built using the x data frame, but could have as well be 
# built using the y data frame. With the LCS we are able to match ticks.
#
# For the LCS algorithm, refer to pages 394-395 of Introduction to Algorithms,
# 3rd ed. by Cormen et al.

lcs <- function(x, y) {
	B <- matrix(0, nrow(x), nrow(y))
	C <- matrix(0, nrow(x) + 1, nrow(y) + 1)
		
	for (i in 1:nrow(x)) {
		for (j in 1:nrow(y)) {
			if (x[i, 2] == y[j, 2] & x[i, 3] == y[j, 3]) {
				C[i+1, j+1] <- C[i, j] + 1
				B[i, j] <- "upleft"
			} else if (C[i, j+1] >= C[i+1, j]) {
				C[i+1, j+1] <- C[i, j+1]
				B[i, j] <- "up"
			} else {
				C[i+1, j+1] <- C[i+1, j]
				B[i, j] <- "left"
			}
		}
	} 

	LCS <- build_lcs(B, x, nrow(x), nrow(y))
	return(LCS)
}



build_lcs <- function(B, x, i, j) {
 	lcs <- data.frame()
 	while (i != 0 & j != 0) {
 		if (B[i, j] == "upleft") { 
 			lcs <- rbind(x[i, ], lcs)
 			i <- i - 1
 			j <- j - 1
 		} else if (B[i, j] == "up") {
 			i <- i - 1
 		} else {
 			j <- j - 1
 		}
 	}
 	return(lcs)
}
# 		
# 
# build.prefixes <- function(B, x, y, i ,j) {
# 	prefixes <- list(x.prefix = x[0, ], y.prefix = y[0, ])
# 	while (i != 0 & j != 0 & B[i, j] != "upleft") {
# 		if (B[i, j] == "up") {
# 			prefixes$x.prefix <- rbind(x[i, ], prefixes$x.prefix[, ])
# 			i <- i - 1
# 		} else {
# 			prefixes$y.prefix <- rbind(y[j, ], prefixes$y.prefix[, ])
# 			j <- j - 1
# 		}
# 		}
# 	return(prefixes)
# }
# 
# 
# 
# 
# LCS.with.prefixes <- function(quotes) {
# 	LCS <- quotes[[1]][0, ]
# 	x.prefix <- quotes[[1]][0, ]
# 	y.prefix <- quotes[[2]][0, ]
# 	while(nrow(quotes[[1]]) != 0 && nrow(quotes[[2]]) != 0) {
# 		x.end <- min(10 - nrow(x.prefix), nrow(quotes[[1]]))
# 		x <- rbind(x.prefix, quotes[[1]][1:x.end, ])
# 		y.end <- min(10 - nrow(y.prefix), nrow(quotes[[2]]))
# 		y <- rbind(y.prefix, quotes[[2]][1:y.end, ])
# 		B <- matrix(0, nrow(x), nrow(y))
# 		C <- matrix(0, nrow(x) + 1, nrow(y) + 1)
# 		
# 		for (i in 1:nrow(x)) {
# 			for (j in 1:nrow(y)) {
# 				if (x[i, 2] == y[j, 2] & x[i, 3] == y[j, 3]) {
# 					C[i+1, j+1] <- C[i, j] + 1
# 					B[i, j] <- "upleft"
# 				} else if (C[i, j+1] >= C[i+1, j]) {
# 					C[i+1, j+1] <- C[i+1, j]
# 					B[i, j] <- "up"
# 				} else {
# 					C[i+1, j+1] <- C[i+1, j]
# 					B[i, j] <- "left"
# 				}
# 			}
# 		} 
# #		build.LCS <- function(B, x, i, j) {
# #			if (i == 0 | j == 0) return(x[0, ])
# #			else if (B[i, j] == "upleft") rbind(build.LCS(B, x, i-1, j-1), x[i, ])
# #			else if (B[i, j] == "up") build.LCS(B, x, i-1, j)
# #			else build.LCS(B, x, i, j-1)
# #		}
# 
# 
# 		LCS <- rbind(LCS, build.LCS(B, x, nrow(x), nrow(y)))
# 		
# #		build.prefixes <- function(B, x, y, i, j) {
# #			y.prefix <- y[0, ]
# #			if (i == 0 | j == 0 | B[i, j] == "upleft") return(list(x[0, ], y[0, ]))
# #			else if (B[i,j] == "up") {
# #				list(rbind(build.prefixes(B, x, y, i-1, j)[[1]], x[i, ]), y[0, ])
# #			} else {
# #				list(x[0 ,], rbind(build.prefixes(B, x, y, i, j-1)[[2]], y[j, ]))
# #			}
# #		}
# #
# 		x.prefix <- build.prefixes(B, x, y, nrow(x), nrow(y))[[1]]
# 		y.prefix <- build.prefixes(B, x, y, nrow(x), nrow(y))[[2]]
# 		quotes[[1]] <- quotes[[1]][-(1:x.end), ]
# 		quotes[[2]] <- quotes[[2]][-(1:y.end), ]
# 	}
# 	return(LCS)
# }
