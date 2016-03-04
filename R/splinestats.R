## Functions using smoothing splines
## probably just a few helper frames to set up the data.
##
## s <- subset(g, orig.tag=="h A d" & sample==4)
##
## h<-singlefit(s)
## ggplot(s, aes(x=X, y=Y)) + geom_path(aes(group=unique.tag, colour=unique.tag), alpha=0.4) + 
## geom_smooth(data=h, aes( ymin=Y-SE, ymax=Y+SE), stat='identity')
##
## Looks like the general approach will be to somehow select one
## trace from each repeat of each token, and then use these functions to compare tokens
## eventually we'll need functions that sensibly select the trace of interest.
## For testing purposes, lets write something that will take the middle trace

# g <- splineDatForGG(tongue.marija, paste(labsminus1.marija,labs.marija,labsplus1.marija))
# gm <- getMiddleTongue(g)
# gm <- subset(gm, certainty > 0)
# gm1 <- subset(gm, orig.tag=="h A d")
# gm2 <- subset(gm, orig.tag=="h e: d")
# h1<-singlefit(gm1)
# h2 <- singlefit(gm2)
#
# hh <- rbind(h1, h2)
#
# ggplot(hh, aes(x=X, y=Y,group=orig.tag, colour=orig.tag))  + geom_smooth(aes( ymin=Y-1.96*SE, ymax=Y+1.96*SE), stat='identity')
#
getMiddleTongue <- function(s)
{
  # can write simple variants of this 
  # return the middle from each 
  middlepertag <- ddply(g, "unique.tag", 
                        function(D)
                        {
                          middle <- round(median(D$sample))
                          return(D[D$sample==middle,])
                        }
  )
  return(middlepertag)
}



singlefit <- function(s.sub)
{
  ## s.sub is a single set of spline data to fit.
  mod <- ssanova(Y ~ X, data=s.sub, weight=s.sub$certainty)
  
  r<-range(s.sub$X)
  jj<-data.frame(X=seq(from=r[1], to=r[2], length.out=100))
  p <- predict(mod, newdata=jj, se.fit=TRUE)
  jj$Y <- p$fit
  jj$SE <- p$se.fit
  jj$orig.tag <- unique(s.sub$orig.tag)[1]
  ## set up something for plotting
  
  return(jj)
}

## ssanova doesn't like weights of zero - causes NaN in the projection.
## j1 <- subset(gm, orig.tag %in% c("h A d", "h e: d") & (certainty > 0))
## mc <- combinedfit(j1)
## x<- seq(from=min(j1$X), to=max(j1$X), length.out=50)
## newdat <- data.frame(X=c(x,x), orig.tag=c(rep("h A d", length(x)), rep("h e: d", length(x))))
## ff <- predict(mc, newdata=newdat, se=TRUE)
## newdat$Y <- ff$fit
## newdat$CI <- ff$se.fit * 1.96
## ggplot(newdat, aes(x=X, y=Y, group=orig.tag, colour=orig.tag)) + geom_smooth(aes(ymin=Y-CI, ymax=Y+CI), stat='identity')
## project(mc, include=c("X", "orig.tag"))
##
## Compare repeats of the same token to see what the projection should report - combine 1 and 3, 2 and 4
## j2 <- subset(gm, orig.tag %in% c("h A d") & (certainty > 0))
## j2$test <- "g1"
## j2$test[j2$unique.tag %in% c("h A d 1", "h A d 3")] <- "g2"
## j2$test <- factor(j2$test)
## mm <- ssanova(Y ~ X*test, data=j2, weight=certainty)
## project(mm, include=c("X", "test"))
##
## x<- seq(from=min(j2$X), to=max(j2$X), length.out=50)
## newdat <- data.frame(X=c(x,x), test=c(rep("g1", length(x)), rep("g2", length(x))))
## pp <- predict(mm, newdata=newdat, se=TRUE)
## newdat$Y <- pp$fit
## newdat$CI <- pp$se.fit * 1.96
## ggplot(newdat, aes(x=X, y=Y, group=test, colour=test)) + geom_smooth(aes(ymin=Y-CI, ymax=Y+CI), stat='identity')
## ratio for two repeats of h A d is 0.002
## for h A d vs h e: d is 0.04
combinedfit <- function(s.sub)
{
  ## something with multiple tokens
  s.sub$orig.tag <- factor(s.sub$orig.tag)
  mod <- ssanova(Y ~ X + orig.tag + X:orig.tag, data=s.sub, weight=s.sub$certainty)
  return(mod)
}