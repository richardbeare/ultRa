#library(emu)
#library(ultRa)
#library(ggplot2)
#install.packages(c("tidyr", "dplyr", "purrr"))
#library(tidyr)
#library(dplyr)
#f<-load("trial_data_04032016.Rdata")

# Now we fit the ssanova to each token

#' Function for fitting the ssanova and returning a prediction
#' with confidence intervals.
#' @param DF dataframe with tongue spline
fit.ssanova <- function(DF)
{
  # Have a special fitting function to deal with problem values
  DF1 <- DF[DF$certainty > 0, ]
  mod <- ssanova(Y ~ X, data=DF1, weight=DF1$certainty)
  r<-range(DF1$X)
  jj<-data.frame(X=seq(from=r[1], to=r[2], length.out=100))
  p <- predict(mod, newdata=jj, se.fit=TRUE)
  jj$Y <- p$fit
  jj$SE <- p$se.fit
  jj$CI <- jj$SE * 1.96
  jj$orig.tag <- unique(DF1$orig.tag)[1]
  return(list(model=mod, prediction=jj))
}
#

#' Fit ssanova to groups of tongue splines
#' @param tonguedat - the tongue data, usually with emu.track and a cut
#' option
#' @param labels - the labels used to define grouped splines
#' @param individual - return all labels (including repeats), if true 
#' possibly good for debugging.
#' @return a data frame with smoothed values and CI suitable for
#' plotting with ggplot2
#' @export
#' @examples
#' \dontrun{
#' plotoptions <- list(
#'  scale_x_continuous(breaks=seq(from=-50,to=50,by=20)),
#'   scale_y_continuous(breaks=seq(from=-40, to=30,by=20))
#'   )
#'   
#' gg <- tongue.ssanova(tongue.data.cut_0.5, labs.segs)
#' ggplot(gg, aes(x=X, y=Y, group=orig.tag, colour=orig.tag)) + 
#' geom_path() + geom_smooth(aes(ymin=Y-CI, ymax=Y+CI), stat='identity') +
#'  coord_fixed() + theme_bw()
#'  
#' g0 <- tongue.ssanova(tongue.data.cut_0.0, labs.segs)
#' g1 <- tongue.ssanova(tongue.data.cut_0.5, labs.segs)
#' g2 <- tongue.ssanova(tongue.data.cut_1.0, labs.segs)
#' 
#' gall <- combineCuts(g0, g1, g2, cutnames=c("start", "middle", "end"))
#' ggplot(gall, aes(x=X, y=Y, group=interaction(orig.tag,timepoint), colour=orig.tag)) + 
#' geom_path() + geom_smooth(aes(ymin=Y-CI, ymax=Y+CI), stat='identity') +
#'  coord_fixed() + theme_bw() + plotoptions
#'  
#'  
#' ggplot(gall, aes(x=X, y=Y, group=interaction(orig.tag,timepoint), colour=timepoint)) + 
#' geom_path() + geom_smooth(aes(ymin=Y-CI, ymax=Y+CI), stat='identity') +
#'  facet_wrap(~orig.tag) + coord_fixed() + theme_bw() + plotoptions 
#'  

#' }
tongue.ssanova <- function(tonguedat, labels, individual=FALSE)
{
  g<-splineDatForGG(tonguedat, labels)
  
  ## Try to use nested data frames for the fitting and plotting
  if (individual) {
    g1 <- nest(group_by_(g, "unique.tag"))
  } else {
    g1 <- nest(group_by_(g, "orig.tag"))
  }
  
  # put the models into the frame
  g1 <- mutate(g1, model=lapply(data, fit.ssanova))
  # shuffle so that model and prediction are separate
  g2 <- mutate(g1, fit=lapply(model, "[[", "prediction"), 
               model=lapply(model, "[[", "model"))
  
  # Now we unnest the fits
  gg <- unnest_(g2, "fit")
  
  return(gg)
}

#' combine different ssanova fits
#' @param ... data frames (from tongue.ssanova)
#' @param cutnames - the names to be used (e.g c("start", "middle", "end"))
#' @param cutcolname - the name of the column that will be used to specify the cut point.
#' @return A combined data frame with an extra column
#' @seealso tongue.ssanova
#' @export
combineCuts <- function(..., cutnames, cutcolname="timepoint")
{
  fr <- list(...)
  counts <- sapply(fr, nrow)
  res <- do.call(rbind, fr)
  res[[cutcolname]] <- rep(cutnames, counts)
  return(res)
}