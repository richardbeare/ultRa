## Stuff to plot data in ssff files


plot1 <- function(trackdata)
{
  d <- trackdata$data
  X <- t(d[,seq(from=1, by=2, length=ncol(d)/2)])
  Y <- t(d[,seq(from=2, by=2, length=ncol(d)/2)])
  Time <- rep(as.numeric(rownames(d)), rep(ncol(d)/2, nrow(d)))
  samp <- rep(1:nrow(d), rep(ncol(d)/2, nrow(d)))
  df <- data.frame(time=Time, X=as.vector(X), Y=as.vector(Y), sample=samp)
  return(df)
}