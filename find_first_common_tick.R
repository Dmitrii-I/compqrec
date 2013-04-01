find_first_common_tick <- function(ticks_list) {
	# Returns a vector of 2 elements. The elements are the indexes of first
	# common tick (fcq) in the two ticks data frames. 
	#
	# ticks_list: a list of two data frames with ticks of same instrument
	#
	# A first common tick is found if the three consecutive rows in 
	# each data frame have same bid and ask prices.
	#
	# The function assumes there is ALWAYS a first common tick. If not
	# you will get runtime error. 
	#
	# Example:
	# You have collected ticks of same instrument on two different servers. 
	# One server however started to record ticks earlier than the other. To 
	# analyze the ticks you want to sync them up to first common starting tick.
	# Then simply do this:
	# start <- find_first_common_tick(list(ticks_df_1, ticks_df_2)) 	
	# ticks_df_1 <- ticks_df_1[start[1]:nrow(ticks_df_1), ]
	# ticks_df_2 <- ticks_df_2[start[2]:nrow(ticks_df_2), ]

	x <- ticks_list[[1]] # shorter names for easier code reading
	y <- ticks_list[[2]]  

	ix <- 1 # index of the first common tick in the first data frame
	iy <- 1 # and in the second

	if (!isTRUE(all.equal(x[1:3, 2:3], y[1:3, 2:3], F))) {
		# perform first rough sync based on timestamp. 
		# Saves computation time later for highly out of sync tick files
		common_starting_time <- max(x[1, 1], y[1, 1]) 
		ix <- min(which(x[, 1] >= common_starting_time)) 
		iy <- min(which(y[, 1] >= common_starting_time))
	 
		# now do a tick by tick step-forward comparison
		while (!isTRUE(all.equal(x[ix:(ix+2), 2:3], y[iy:(iy+2), 2:3], F))) {
			common_starting_time <- min(x[ix, 1], y[iy, 1])
			ix <- min(which(x[, 1] > common_starting_time))
			iy <- min(which(y[, 1] > common_starting_time))
		}
	}

	return(c(ix, iy))
}
