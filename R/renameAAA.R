#' Rename the files exported by AAA based on the contents of the .txt - lastname_firstname_word
#' @param name of a file in the exported folder. If not provided then a file chooser will be used
#' @return The original names and matching output
#' @description This function renames files exported by AAA. Input is the path to the folder
#' containing exported files, or select one of the files in the folder with a chooser dialog.
#' Renaming proceeds without warning. Likely to be fragile if there is lots of punctuation in the
#' word list.
#' @export
renameAAA <- function(txtfile=NULL)
    {
        if (is.null(txtfile)) {
            txtfile <- file.choose()
        }
        ff <- file.info(txtfile)
        if (!ff$isdir)
            txtfile <- dirname(txtfile)

        ## Now for the real work
        all.txt.files <- list.files(path=txtfile, pattern="File[[:digit:]]{3}\\.txt", full.names=TRUE)
        all.prefixes <- gsub("\\.txt$", "", all.txt.files)
        ## the description files don't have a final EOL
        dat <- lapply(all.txt.files, readLines, warn=FALSE)
        speaker <- sapply(dat, "[", 3)
        dates <- sapply(dat, "[", 2)
        words <- sapply(dat, "[", 1)
        ## Remove tabs from prompt list
        words <- gsub("\t+$", "", words)

        ## need to confirm %d%m - check leading zeros
        dates <- as.POSIXct(dates, format="%d/%m/%G %r")
        speaker <- gsub("[,[:blank:]]+", "_", speaker)
        words <- gsub("[,[:blank:]]+", "_", words)
        newnames <- paste0(speaker, "_", words);
        names(newnames) <- all.prefixes
        names(dates) <- all.prefixes
        ## Now list the different files we need to rename
        original <- list.files(path=txtfile, pattern="File[[:digit:]]{3}.*\\..*", full.names=TRUE)
        prefixes <- gsub("(File[[:digit:]]{3}).+", "\\1", basename(original))
        suffixes <- gsub("(File[[:digit:]]{3})(.+)", "\\2", basename(original))
        newprefix <- newnames[file.path(dirname(original), prefixes)]
        newdates <- dates[file.path(dirname(original), prefixes)]
        ## deal with duplicate out names
        tmpname <- paste(newprefix,suffixes)
        oo <- order(tmpname,newdates)
        original <- original[oo]
        suffixes <- suffixes[oo]
        newprefix <- newprefix[oo]
        tmpname <- tmpname[oo]
        l <- rle(tmpname)

        ll <- as.character(unlist(lapply(l$lengths, function(y)1:y)))
        
        outname <- file.path(dirname(original), paste0(newprefix,"_", ll, "_", suffixes))
        outname <- gsub("_+", "_", outname)
        outname <- gsub("_\\.", ".", outname)

        
        file.rename(from=original, to=outname)
        return(cbind(original, outname))
    }
