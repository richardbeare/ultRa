#' Convert the text file containing spline data to ssff
#' 
exportSplinesSSFF <- function(txtfile=NULL)
    {
        if (is.null(txtfile)) {
            txtfile <- file.choose()
        }

        raw <- readLines(txtfile)
        hd <- strsplit(raw[1], "\\t")[[1]]
        expected <- c("Client Surname", "Prompt","Time of sample in recording", "X,Y values of spline \"Tongue\" relative to \"bite plane\" (rotated)",
                      "Confidence values of spline \"Tongue\"")

        if (any(hd != expected)) {
            stop("Header not what was expected")
        }

        raw <- raw[-1]
        raw <- strsplit(raw, "\\t")

        ## Should be 42 spline points - 84 numbers - followed by 42 conf scores
        prompts <- sapply(raw, "[", 2)
        times <- as.numeric(sapply(raw, "[", 3))
        rest <- lapply(raw, function(p)as.numeric(p[-(1:3)]))
        browser()
        return(raw)
    }
        
