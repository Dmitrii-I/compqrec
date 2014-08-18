visualize_missing <- function(df) {
	x <- is.na(df)	
	x <- t(x)[, nrow(x):1]
	image(x, col=c("green","red"))
}

