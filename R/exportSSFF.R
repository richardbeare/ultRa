#' Create modified names for AAA files. Removes punctuation that offends tcltk
#' @param files the output of list.files
#' @export
#' @return list of modified names
#' @examples
#' \dontrun{
#' f<-list.files(path="c:/Tabain/English_Ultrasound/", full.names=TRUE)
#' f1 <- gsub("_Track1", "", f)
#' f2 <- fixNamesForEMU(f1)
#' need.to.do <- basename(f) != basename(f2)
#' file.rename(from=f[need.to.do], to=f2[need.to.do])
#' }
fixNamesForEMU <- function(files)
{
  b <- basename(files)
  d <- dirname(files)

  b1 <- gsub("'", "", b)
  b2 <- gsub("\\[.+\\]", "", b1)
  b3 <- gsub("__+", "_", b2)
  return(file.path(d,b3))
}


#' Convert the text file containing spline data to ssff
#' @param txtfile the exported AAA spline file
#' @param targdir where the output will be written
#' @param missingAsZero Change NA to 0 so emu doesn't complain
#' @param tracksuf suffix appended to each filename to match the soundfiles already exported
#' @param fixPrompts attempts to use regular expressions to fix problems caused by tabs in the prompt list
#' @export
#' @return the list of prompts, invisibly
#' @examples
#' \dontrun{
#' exportSplinesSSFF("c:/AAA/Richard_Spline_Export_02_05_2015.txt", targdir="c:/Tabain/English_Ultrasound/")
#' }
exportSplinesSSFF <- function(txtfile=NULL, targdir="./", missingAsZero=TRUE, tracksuf="", filesuf=".spln", fixPrompts=TRUE, splinetype="Tongue")
    {
        if (is.null(txtfile)) {
            txtfile <- file.choose()
        }
        splinepts <- 42
        raw <- readLines(txtfile)
        hd <- strsplit(raw[1], "\\t")[[1]]

        expected <- c("Client Surname, given names", "Prompt","Date and time of recording", "Time of sample in recording", 
                      paste0("X,Y values of spline \"",splinetype,"\" relative to \"bite plane\" (rotated)"),
                      paste0("Confidence values of spline \"", splinetype, "\""))
        expected2 <- c("Client Surname, given names", "Prompt","Date and time of recording", "Time of sample in recording", 
                       paste0("X,Y values of spline \"", splinetype, "\""),
                      paste0("Confidence values of spline \"", splinetype, "\""))
        
        if (any(hd != expected)) {
          if (any(hd != expected2)) {
            stop("Header not what was expected")
          }
        }

        raw <- raw[-1]
        if (fixPrompts) {
          ## Sometimes there are tabs in the prompt list, which messes up the columns. 
          ## try to fix this with regular expressions. We're looking for multiple tabs appearing before
          ## some non tabs. Lots of tabs in a row are possible when there are no splines, but they will
          ## be at the end, I hope. Need to match the comma in the name and colon in time field
          raw <- gsub("([^\t]+,.+)(\t{2,})(.+:.+)", "\\1\t\\3",raw)
        }
        raw <- strsplit(raw, "\\t")
        speaker <- raw[[1]][1]
        ## Should be 42 spline points - 84 numbers - followed by 42 conf scores
        prompts <- sapply(raw, "[", 2)
        datetime <- sapply(raw, "[", 3)
        promptAndtime <- paste(prompts, datetime, sep="__")
        times <- as.numeric(sapply(raw, "[", 4))
        rest <- lapply(raw, function(p){
          g<-as.numeric(p[-(1:4)])
          if (length(g)==125) {
            g <- c(g, NA)
          }
          return(g)
        })


        if (missingAsZero) {
          rest <- lapply(rest, function(M){
            M[is.na(M)] <- 0
            return(M)
          })
        }
        rest <- unlist(rest)
        dim(rest) <- c(3*splinepts, length(prompts))

        splines <- rest[1:(2*splinepts),]
        ## Reorder so that it is all X followed by all Y
        splines <- splines[c(seq(from=1, to=2*splinepts, by=2), seq(from=2, to=2*splinepts, by=2)),]
        conf <- rest[(2*splinepts + 1):(3*splinepts),]
        splines <- rbind(splines,conf)
        ## Figure out the rows for each prompt
        ## split returns indexes in order
        promptrows <- split(1:length(promptAndtime), promptAndtime)

        ## Check for irregular spacing - need to produce a list of rows to ignore
        samplerate <- median(diff(times))
        timesperprompt <- lapply(promptrows, function(G){return(times[G])})
        difftimes <- lapply(timesperprompt, function(J)return(abs(diff(J) - samplerate)))

        diffwarning <- lapply(difftimes, function(d)d>0.001)

        if (any(unlist(diffwarning))) {
          ## Just warn for now, as we don't have enough examples to check with
          prob<-sapply(diffwarning, function(G){return(any(G>0.001))})
          msg <- paste("Possible sample rate problem in ", paste(names(diffwarning)[prob], collapse=" : "))
          warning(msg)
        }

        ## Now we figure out the output names
        ## Format is lastname_firstname_word
        clientname <- gsub(", ", "_", speaker)

        unique.prompts <- names(promptrows)
        only.prompt <- gsub("(.+)__.+", "\\1", unique.prompts)
        l <- rle(only.prompt)
        ll <- as.character(unlist(lapply(l$lengths, function(y)1:y)))
        outnames <- paste0(clientname, "_", only.prompt, "_", ll, tracksuf)
        outnames <- gsub(" +", "_", outnames)
        outnames.spline <- file.path(targdir, paste0(outnames, filesuf))
        outnames.spline <- fixNamesForEMU(outnames.spline)
        outnames.conf <- file.path(targdir, paste0(outnames, ".spconf"))
        outnames.conf <- fixNamesForEMU(outnames.conf)
        for (i in 1:length(promptrows)) {
          these.rows <- promptrows[[i]]
          starttime <- timesperprompt[[i]][1]
          sp <- as.vector(splines[, these.rows])
          storage.mode(sp) <- "double"
          ssff.header <- c("SSFF -- (c) SHLRC",
                           "Machine IBM-PC",
                           paste("Start_Time", formatC(starttime,digits=5)),
                           paste("Record_Freq", as.numeric(1/ samplerate)),
                           paste("Column XY DOUBLE ", as.character(3*splinepts)),
                           "-----------------")

          writeLines(ssff.header, outnames.spline[i])
          connection <- file(description=outnames.spline[i], open="a+b")
          writeBin(sp, con=connection)
          close(connection)
        }

        return(invisible(outnames.spline))
    }
#' Convert the text file containing spline data to ssff. This exports all the frames, 
#' which isn't really necessary
#' @param txtfile the exported AAA spline file
#' @param targdir where the output will be written
#' @param missingAsZero Change NA to 0 so emu doesn't complain
#' @param tracksuf suffix appended to each filename to match the soundfiles already exported
#' @export
#' @examples
#' \dontrun{
#' exportFiducialSSFF("c:/AAA/Richard_BitePlane_Export_02_05_2015.txt", targdir="c:/Tabain/English_Ultrasound/")
#' }
exportFiducialSSFF <- function(txtfile=NULL, targdir="./", missingAsZero=TRUE, 
                               tracksuf="", filesuf=".spln", fixPrompts=TRUE)
{
  if (is.null(txtfile)) {
    txtfile <- file.choose()
  }
  splinepts <- 2
  raw <- readLines(txtfile)
  hd <- strsplit(raw[1], "\\t")[[1]]

  expected <- c("Client Surname, given names", "Prompt","Date and time of recording", "Time of sample in recording", 
                "X,Y values of spline \"bite plane\"")
   
  if (any(hd != expected)) {
      stop("Header not what was expected")
  }
  
  raw <- raw[-1]

  raw <- strsplit(raw, "\\t")
  speaker <- raw[[1]][1]
  ## Should be 2 spline points - 4 numbers
  prompts <- sapply(raw, "[", 2)
  datetime <- sapply(raw, "[", 3)
  promptAndtime <- paste(prompts, datetime, sep="__")
  times <- as.numeric(sapply(raw, "[", 4))
  rest <- lapply(raw, function(p){
    g<-as.numeric(p[-(1:4)])
    return(g)
  })
  
  if (missingAsZero) {
    rest <- lapply(rest, function(M){
      M[is.na(M)] <- 0
      return(M)
    })
  }
  rest <- unlist(rest)
  dim(rest) <- c(2*splinepts, length(prompts))
  
  splines <- rest
  ## Reorder so that it is all X followed by all Y
  splines <- splines[c(seq(from=1, to=2*splinepts, by=2), seq(from=2, to=2*splinepts, by=2)),]

  ## Figure out the rows for each prompt
  ## split returns indexes in order
  promptrows <- split(1:length(promptAndtime), promptAndtime)
  ## Check for irregular spacing - need to produce a list of rows to ignore
  samplerate <- median(diff(times))
  timesperprompt <- lapply(promptrows, function(G){return(times[G])})
  
  ## Now we figure out the output names
  ## Format is lastname_firstname_word
  clientname <- gsub(", ", "_", speaker)
  
  unique.prompts <- names(promptrows)
  only.prompt <- gsub("(.+)__.+", "\\1", unique.prompts)
  l <- rle(only.prompt)
  ll <- as.character(unlist(lapply(l$lengths, function(y)1:y)))
  outnames <- paste0(clientname, "_", only.prompt, "_", ll, tracksuf)
  outnames <- gsub(" +", "_", outnames)
  outnames.spline <- file.path(targdir, paste0(outnames, filesuf))
  outnames.spline <- fixNamesForEMU(outnames.spline)
  outnames.conf <- file.path(targdir, paste0(outnames, ".spconf"))
  outnames.conf <- fixNamesForEMU(outnames.conf)
  for (i in 1:length(promptrows)) {
    these.rows <- promptrows[[i]]
    starttime <- timesperprompt[[i]][1]
    sp <- as.vector(splines[, these.rows])
    storage.mode(sp) <- "double"
    ssff.header <- c("SSFF -- (c) SHLRC",
                     "Machine IBM-PC",
                     paste("Start_Time", formatC(starttime,digits=5)),
                     paste("Record_Freq", as.numeric(1/ samplerate)),
                     paste("Column XY DOUBLE ", as.character(2*splinepts)),
                     "-----------------")
    
    writeLines(ssff.header, outnames.spline[i])
    connection <- file(description=outnames.spline[i], open="a+b")
    writeBin(sp, con=connection)
    close(connection)
  }
  
  return(invisible(outnames.spline))
}

