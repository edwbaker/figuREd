#' Wave moving through a gas
#'
#' @importFrom graphics par
#' @importFrom stats rnorm runif
#' @export
wave_particle <- function() {
  n <- 8000
  y <- runif(n)

  x <- c()
  for (i in 1:4) {
    x_n <- rnorm(n/4, mean=i, sd=0.4)
    x <- c(x, x_n + i-1)
  }

  par(mar=c(1,1,1,1))

  plot(cbind(x,y), cex=0.1, yaxt="n", xaxt="n", xlab="", ylab="")
}
