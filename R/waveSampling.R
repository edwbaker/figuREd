#' Show a wave being sampled
#'
#' @param wave A Wave object
#' @param plot_every Sample rate to show
#' @importFrom graphics abline segments
#' @export
waveSampling <- function(wave, plot_every){
  n <- floor(length(wave@left)/plot_every)

  x <- seq(from=plot_every, by=plot_every, length.out=n)
  y <- wave@left[x]

  plot(wave@left, type="l", xlab="Time", xaxt="n", ylab="Amplitude")
  abline(h=0)
  segments(x,0,x,y)
}