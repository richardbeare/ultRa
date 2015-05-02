#' Convert the text file containing spline data to ssff
#' @param txtfile the exported AAA spline file
#' @param targdir where the output will be written
#' @param missingAsZero Change NA to 0 so emu doesn't complain
#' @param tracksuf suffix appended to each filename to match the soundfiles already exported
#' @export
#' @return the list of prompts, invisibly
exportSplinesSSFF <- function(txtfile=NULL, targdir="./", missingAsZero=TRUE, tracksuf="_Track1")
    {
        if (is.null(txtfile)) {
            txtfile <- file.choose()
        }
        splinepts <- 42
        raw <- readLines(txtfile)
        hd <- strsplit(raw[1], "\\t")[[1]]

        expected <- c("Client Surname, given names", "Prompt","Date and time of recording", "Time of sample in recording", "X,Y values of spline \"Tongue\" relative to \"bite plane\" (rotated)",
                      "Confidence values of spline \"Tongue\"")

        if (any(hd != expected)) {
            stop("Header not what was expected")
        }

        raw <- raw[-1]
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
        conf <- rest[(2*splinepts + 1):(3*splinepts),]
        
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
        outnames.spline <- file.path(targdir, paste0(outnames, ".spln"))
        outnames.conf <- file.path(targdir, paste0(outnames, ".spconf"))
        for (i in 1:length(promptrows)) {
          these.rows <- promptrows[[i]]
          #browser()
          sp <- as.vector(splines[, these.rows])
          storage.mode(sp) <- "double"
          ssff.header <- c("SSFF -- (c) SHLRC",
                           "Machine IBM-PC",
                           paste("Start_Time", as.character()),
                           paste("Record_Freq", as.numeric(1/ samplerate)),
                           "Column XY DOUBLE 84")
                           
          writeLines(ssff.header, outnames.spline[i])
          connection <- file(description=outnames.spline[i], open="a+b")
          writeBin(sp, con=connection)
          close(connection)
        }
        
        return(invisible(outnames.spline))
    }
        
