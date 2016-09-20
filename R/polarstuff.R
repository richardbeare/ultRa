## Stuff with polar coordinates
## Will need to beware of mixing up bite plane angles for individuals
#' @export
mkPolar <- function(df, x="X", y="Y", biteplaneangle=0, targetOrigin=c(64,0))
{
  ## subtract the target origin, convert to polar, then subtract the biteplane angle
  xx <- df[,x]
  yy <- df[,y]
  
  xx <- xx - targetOrigin[1]
  yy <- yy - targetOrigin[2]
  
  r <- sqrt(xx^2 + yy^2)
  theta <- atan2(yy, xx)
  df$r <- r
  df$theta <- theta - biteplaneangle
  return(df)
}
#' @export
mkCartesian <- function(df, r="r", theta="theta", x="X", y="Y")
{
  rr <- df[,r]
  th <- df[,theta]
  xx <- rr*cos(th)
  yy <- rr*sin(th)
  df[,x] <- xx
  df[,y] <- yy
  return(df)
}

#' Transform a typical ssanova back to
#' cartesian coordinates for plotting
#' @export 
#' 
mkCartesianAll <- function(df)
{
  dfM <- mkCartesian(df, r="r", theta="theta")
  dfSE <- mkCartesian(df, r="SE", theta="theta", y="SE")
  dfCI <- mkCartesian(df, r="CI", theta="theta", y="CI")
  res <- dfM
  res$SE <- dfSE$SE
  res$CI <- dfCI$CI
  return(res)
}