#' Stuff to plot data in ssff files
#' @export
#' @param trackdata the track from emu.track 
#' @details Rearranges the trackdata into a sensible for for plotting
#' Not really needed for general use. Only works on a single token
#' @return A dataframe with X, Y, certainty, time and sample columns
extractTongue <- function(trackdata)
{
  d <- trackdata$data
  lastx <- ncol(d)/3
  lasty <- 2*ncol(d)/3
  
  X <- t(d[,1:lastx])
  Y <- t(d[,(lastx+1):lasty])
  cert <- t(d[,(lasty+1):ncol(d)])
  tt <- as.numeric(rownames(d))
  tt <- tt - tt[1]
  Time <- rep(tt, rep(ncol(d)/3, nrow(d)))
  samp <- rep(1:nrow(d), rep(ncol(d)/3, nrow(d)))
  df <- data.frame(time=Time, X=as.vector(X), Y=as.vector(Y), sample=samp, certainty=as.vector(cert))
  return(df)
}

#' Extract splines for all tokens in a trackdata file.
#' @param trackdata the trackdata from emu.track
#' @param tags the labels that you want to use. Probably paste(labsminus1, labs, labsplus1)
#' @return a big dataframe for visualization
#' @export
#' @examples 
#' \dontrun{
#' labs <- paste(labsminus1.marija,labs.marija,labsplus1.marija)
#' g<-splineDatForGG(tongue.marija, labs)
#' 
#' ggplot(g, aes(x=X,y=Y,group=interaction(factor(sample),unique.tag), colour=time)) + geom_line() + 
#' geom_point(aes(size=certainty)) + facet_wrap(~unique.tag)
#' ggplot(subset(g, orig.tag=="h a: d"), aes(x=X,y=Y,group=interaction(factor(sample),unique.tag), colour=time)) + geom_path() + 
#' geom_point(aes(size=certainty)) + facet_wrap(~unique.tag)
#' }
splineDatForGG <- function (trackdata, tags) 
{
  if (nrow(trackdata) != length(tags)) {
    stop("Length of trackdata and labels doesn't match")
  }
  #l <- rle(tags)
  #ll <- as.character(unlist(lapply(l$lengths, function(y) 1:y)))
  ll <- 1:length(tags)
  tags.mod <- paste(tags, ll)
  tags.mod <- factor(1:length(tags.mod), labels=tags.mod)
  ## is there a neater version with 
  trajectories <- lapply(1:length(tags), function(idx) {
    a <- extractTongue(trackdata[idx, ])
    a$unique.tag <- tags.mod[idx]
    a$orig.tag <- tags[idx]
    return(a)
  })
  names(trajectories) <- tags.mod
  trajectories <- do.call(rbind, trajectories)
  return(trajectories)
}

#' Interactive visualization of splines by token
#' @param splineDat output from splineDatForGG
#' @return Nothing - a shiny app should appear in a browser
#' @export
#' @examples 
#' \dontrun{
#' labs <- paste(labsminus1.marija,labs.marija,labsplus1.marija)
#' g<-splineDatForGG(tongue.marija, labs)
#' shinyTongue(g)
#'}
shinyTongue <- function(splineDat)
{
  library(ggplot2)
  require(shiny)
  chk <- unique(splineDat$orig.tag)
  names(chk) <- chk
  shinyApp(
    ui=fluidPage(titlePanel("Tongue spline plots"),
    
    sidebarLayout(
      sidebarPanel(checkboxGroupInput("promptchoice", "Prompts to display",
                                      choices=chk, selected=chk[1], inline=TRUE)),
      mainPanel(plotOutput("hist"))
    )
    ),
    server=function(input, output) {

      output$hist <- renderPlot({
        gg <- subset(splineDat, orig.tag %in% input$promptchoice)
        if (nrow(gg)>0)
          ggplot(gg, aes(x=X,y=Y,group=interaction(factor(sample),unique.tag), colour=time)) + 
            geom_path(aes(alpha=certainty))+ facet_wrap(~unique.tag)
      }
        ,
        height=1000
      )
    }
  )
}

