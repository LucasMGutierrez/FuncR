conf.interval2 <- function(x, s, n, conf.level=0.95) {
	alfa = 1 - conf.level
	if (n >= 30) {
		z = qnorm(1 - alfa/2)
	}
	else {
		z = qt(1 - alfa/2, n-1)
	}

	return(c(x - z * s / sqrt(n), x + z * s / sqrt(n)))
}

conf.interval <- function(x, conf.level=0.95) {
	return(conf.interval2(mean(x), sd(x), length(x), conf.level))
}

cv <- function(x) {
	return (sd(x) / mean(x))
}

new.function <- function(x) {
	return (x + 1)
}

pull.request <- function(x) {
	return (x - 1)
}

new.feature <- function(x) {
	return (x * 1)
}

a <- 1
