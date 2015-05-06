## Stuff to plot data in ssff files


plot1 <- function(trackdata)
{
  d <- trackdata$data
  lastX <- ncol(d)/3
  lasty <- 2*ncol(d)/3
  
  X <- t(d[,1:lastx])
  Y <- t(d[,(lastx+1):lasty])
  cert <- t(d[,(lasty+1):ncol(d)])
  Time <- rep(as.numeric(rownames(d)), rep(ncol(d)/2, nrow(d)))
  samp <- rep(1:nrow(d), rep(ncol(d)/2, nrow(d)))
  df <- data.frame(time=Time, X=as.vector(X), Y=as.vector(Y), sample=samp, certainty=cert)
  return(df)
}