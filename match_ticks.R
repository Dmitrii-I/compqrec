match_ticks <- function(x, y) {
	# Returns a data frame with side-by-side matched ticks from two 
	# ticks data frames: x and y. If not match was found for a ticks,
	# NA is displayed. This allows you to identify which ticks were not
	# recorded by either of the recorders. Knowing which ticks were 
	# missed allows comparing of data recording instances that produced 
	# the two ticks data frames.
	#
	# When  the data recording instances are on different servers, 
	# you are in essence comparing which server records the ticks best.
	#
	# x, y:	tick data frame
	
	# Sync data frames to first common tick; purge leading redundant ticks
	# if needed
	start <- find_first_common_tick(list(x, y))
	x <- x[start[1]:nrow(x), ]
	y <- y[start[2]:nrow(y), ]

	# index vars of data frames; start from second tick, as first ticks
	# already match
	ix <- 2L
	iy <- 2L
	i <- 1L

	# pre-allocate to max to avoid expanding vectors dynamically (costly)
	missed_x <- rep(NA, max(nrow(x), nrow(y))) 
	missed_y <- missed_x 

	window_size <- 9 # set to max number of possible 
					 # consecutive lost ticks
	
	while (ix < nrow(x) && iy <= nrow(y)) { # < and <= is not a mistake
		if (!isTRUE(all.equal(x[ix, 2:3], y[iy, 2:3], check.attributes=F))) {
			# indexes of last ticks used in comparison
			last_x <- min(ix + window_size, nrow(x)) 
			last_y <- min(iy + window_size, nrow(y))
			
			# because ix:last_x and iy:last_y may cover different 
			# nonoverlapping time periods, we need to adjust last_x and last_y
			# so that overlapping periods are covered. Overlapping is needed
			# to find matching ticks 
			last <- max(x[last_x, 1], y[last_y, 1])
			last_x <- rev(which(x[, 1] <= last))[1]
			last_y <- rev(which(y[, 1] <= last))[1]
			
			# Find the longest common subsequence
			lcs <- lcs(x[ix:last_x, ], y[iy:last_y, ])


			# Using lcs to match the ticks
			if (nrow(lcs) != 0) {
				if (isTRUE(all.equal(x[ix, 2:3], lcs[1, 2:3], check.attributes=F))) {
					missed_x[i] <- ix - 0.99999 / ix 
					iy <- iy + 1
				} else if (isTRUE(all.equal(y[iy, 2:3], lcs[1, 2:3], check.attributes=F))) {
					missed_y[i] <- iy - 0.99999 / iy 
					ix <- ix + 1
				} else { # if both  quotes are unequal to each other and the first LCS item
					if (x[ix, 1] >= y[iy, 1]) {
						missed_x[i] <- ix - 0.99999 / ix
						missed_y[i] <- (iy + 1) - 0.99999 / iy
						ix <- ix + 1
						iy <- iy + 1
					} else {
						missed_x[i] <- (ix + 1) - 0.99999 / ix 
						missed_y[i] <- iy - 0.99999 / iy
						ix <- ix + 1
						iy <- iy + 1
					}
				}
			} else { # if LCS is empty
				if (x[ix, 1] >= y[iy, 1]) {
					missed_x[i] <- ix - 0.99999 / ix
					missed_y[i] <- (iy + 1) - 0.99999 / iy
					ix <- ix + 1
					iy <- iy + 1
				} else {
					missed_x[i] <- (ix + 1) - 0.99999 / ix 
					missed_y[i] <- iy - 0.99999 / iy
					ix <- ix + 1
					iy <- iy + 1
				}
			}
			i <- i + 1  
		} else {
			ix <- ix + 1
			iy <- iy + 1
		}
	}
	missed_x <- missed_x[complete.cases(missed_x)]
	missed_y <- missed_y[complete.cases(missed_y)]

	x.NAs <- as.data.frame(matrix(NA, length(missed_x), 3))
	names(x.NAs) <- names(x) # equal names needed to rbind
	x <- rbind(x, x.NAs)
	x <- x[order(c(1:(nrow(x) - length(missed_x)), missed_x)), ]

	y.NAs <- as.data.frame(matrix(NA, length(missed_y), 3))
	names(y.NAs) <- names(y) # equal names needed to rbind
	y <- rbind(y, y.NAs)
	y <- y[order(c(1:(nrow(y) - length(missed_y)), missed_y)), ]
 
	n <- min(nrow(x), nrow(y)) - 1
	return(cbind(x[1:n, ], y[1:n, ]))
}
