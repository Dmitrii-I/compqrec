missed.quotes <- function(quotes) {
	quotes <- sync.begin(quotes)
	x <- quotes[[1]]
	y <- quotes[[2]]
	i.x <- 2L
	i.y <- 2L
	i <- 1L

	# pre-allocate more than needed to avoid expanding vectors dynamically
	x.miss.ind <- rep(NA, max(sapply(quotes, nrow))) 
	y.miss.ind <- x.miss.ind 


	while (i.x < nrow(x) && i.y <= nrow(y)) {
		if (!isTRUE(all.equal(x[i.x, 2:3], y[i.y, 2:3], F))) {
			n.x <- min(i.x + 9, nrow(x))
			n.y <- min(i.y + 9, nrow(y))
			last <- max(x[n.x, 1], y[n.y, 1])
			xx <- x[i.x:nrow(x), ][(x[i.x:nrow(x), 1] <= last) & (x[i.x:nrow(x), 1] >= x[i.x, 1]), ]	
			yy <- y[i.y:nrow(y), ][(y[i.y:nrow(y), 1] <= last) & (y[i.y:nrow(y), 1] >= y[i.y, 1]), ]	
			lcs <- LCS(list(xx, yy))

			if (nrow(lcs) != 0) {
				if (isTRUE(all.equal(x[i.x, 2:3], lcs[1, 2:3], F))) {
					x.miss.ind[i] <- i.x - 0.99999 / i.x 
					i.y <- i.y + 1
				} else if (isTRUE(all.equal(y[i.y, 2:3], lcs[1, 2:3], F))) {
					y.miss.ind[i] <- i.y - 0.99999 / i.y 
					i.x <- i.x + 1
				} else { # if both  quotes are unequal to each other and the first LCS item
					if (x[i.x, 1] >= y[i.y, 1]) {
						x.miss.ind[i] <- i.x - 0.99999 / i.x
						y.miss.ind[i] <- (i.y + 1) - 0.99999 / i.y
						i.x <- i.x + 1
						i.y <- i.y + 1
					} else {
						x.miss.ind[i] <- (i.x + 1) - 0.99999 / i.x 
						y.miss.ind[i] <- i.y - 0.99999 / i.y
						i.x <- i.x + 1
						i.y <- i.y + 1
					}
				}
			} else { # if LCS is empty
				if (x[i.x, 1] >= y[i.y, 1]) {
					x.miss.ind[i] <- i.x - 0.99999 / i.x
					y.miss.ind[i] <- (i.y + 1) - 0.99999 / i.y
					i.x <- i.x + 1
					i.y <- i.y + 1
				} else {
					x.miss.ind[i] <- (i.x + 1) - 0.99999 / i.x 
					y.miss.ind[i] <- i.y - 0.99999 / i.y
					i.x <- i.x + 1
					i.y <- i.y + 1
				}
			}
			i <- i + 1  
		} else {
			i.x <- i.x + 1
			i.y <- i.y + 1
		}
	}
	x.miss.ind <- x.miss.ind[complete.cases(x.miss.ind)]
	y.miss.ind <- y.miss.ind[complete.cases(y.miss.ind)]

	x.NAs <- as.data.frame(matrix(NA, length(x.miss.ind), 3))
	names(x.NAs) <- names(x) # equal names needed to rbind
	x <- rbind(x, x.NAs)
	x <- x[order(c(1:(nrow(x) - length(x.miss.ind)), x.miss.ind)), ]

	y.NAs <- as.data.frame(matrix(NA, length(y.miss.ind), 3))
	names(y.NAs) <- names(y) # equal names needed to rbind
	y <- rbind(y, y.NAs)
	y <- y[order(c(1:(nrow(y) - length(y.miss.ind)), y.miss.ind)), ]
 
	n <- min(nrow(x), nrow(y)) - 1
	return(list(x[1:n, ], y[1:n, ]))
}
