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

#' Show a sampled wave
#'
#' @param wave A Wave object
#' @param plot_every Sample rate to show
#' @param sample.depth Number of sampling levels to use
#' @param show_wave Logical. Whether to plot the waveform.
#' @importFrom graphics barplot lines
#' @export
waveSampled <- function(wave, plot_every, sample.depth, show_wave=T){
  sample.depth <- sample.depth/2
  n <- floor(length(wave@left)/plot_every)

  x <- seq(from=plot_every, by=plot_every, length.out=n)
  y <- wave@left[x]

  adc <- (-sample.depth:sample.depth)/sample.depth

  y <- adc[findInterval(y, adc)]

  barplot(
    y,
    xlab="Time",
    xaxt="n",
    width=plot_every,
    space=0,
    ylab="Amplitude",
    ylim=c(-1,1))
  if (show_wave) {
    lines(1:length(wave@left), wave@left)
  }
  abline(h=0)
}
