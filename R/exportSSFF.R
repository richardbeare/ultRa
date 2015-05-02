#' Convert the text file containing spline data to ssff
#' 
exportSplinesSSFF <- function(txtfile=NULL, missingAsZero=TRUE)
    {
        if (is.null(txtfile)) {
            txtfile <- file.choose()
        }
        splinepts <- 42
        raw <- readLines(txtfile)
        hd <- strsplit(raw[1], "\\t")[[1]]

        expected <- c("Client Surname", "Prompt","Date and time of recording", "Time of sample in recording", "X,Y values of spline \"Tongue\" relative to \"bite plane\" (rotated)",
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
        promptrows <- split(1:length(prompts), prompts)
        
        ## Check for irregular spacing - need to produce a list of rows to ignore
        samplerate <- median(diff(times))
        timesperprompt <- lapply(promptrows, function(G){return(times[G])})
        difftimes <- lapply(timesperprompt, function(J)return(abs(diff(J) - samplerate)))
        
        diffwarning <- lapply(difftimes, function(d)d>0.001)
        if (any(unlist(diffwarning))) {
          ## Just warn for now, as we don't have enough examples to check with
          warning("Possible sample rate problem - check more closely")
        }
        
        ## Now we figure out the output names
        ## Format is lastname_firstname_word
        return(prompts)
    }
        
