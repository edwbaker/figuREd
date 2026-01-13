#' Storage costs
#'
#' Graph of storage costs over time, using data taken from https://jcmit.net/diskprice.htm.
#' @importFrom rvest read_html html_nodes html_table
#' @importFrom graphics axis
#'
#' @export

storageCosts <- function() {

  drive_url <- "https://web.archive.org/web/20250117083324/https://jcmit.net/diskprice.htm"

  page <- read_html(drive_url)

  drives <- page |> html_nodes("table") |> html_table(fill = TRUE)
  drives <- drives[[2]]

  #Fix naming
  cn <- drives[1,]
  cn[1] <- "date"
  cn[2] <- "US$/MB"
  drives <- drives[3:nrow(drives),]
  colnames(drives) <- cn

  #allow for empty US$/MB
  drives <- drives[which(drives$`US$/MB`!=""),]

  plot(drives$date, drives$`US$/MB`, log="y", cex=.3, yaxt = "n", xlab="Year", ylab="US$ / MB")
  axis(2, at=c(0.01,0.1,1,10,100), labels = c("0.01","0.1","1","10","100"))
}
