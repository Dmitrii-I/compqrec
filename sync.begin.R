sync.begin <- function(quotes) {
	# Returns a list of quote files that have same first quote (First Common
	# Quote). Irrelevant leading quotes are purged.

	# Perform a rough sync first and return if the first 3 quotes don match
	if (!isTRUE(all.equal(quotes[[1]][1:3, ], quotes[[2]][1:3, ], F))) {
	first <- max(quotes[[1]][1, 1], quotes[[2]][1, 1])
	quotes[[1]] <- quotes[[1]][quotes[[1]][, 1] >= first, ]
	quotes[[2]] <- quotes[[2]][quotes[[2]][, 1] >= first, ]
	}

	# Perform a more fine grained sync. As soon as first 3 quotes of both
	# data frames are equal, we are done. The choice for 3 
	# first quotes is arbitrary.
	while (!isTRUE(all.equal(quotes[[1]][1:3, ], quotes[[2]][1:3, ], F))) {
		start <- min(quotes[[1]][1, 1], quotes[[2]][1, 1])
		quotes[[1]] <-quotes[[1]][quotes[[1]][, 1] > start, ]
		quotes[[2]] <-quotes[[2]][quotes[[2]][, 1] > start, ]
	}
	return(quotes)
}
