#' Rename the files exported by AAA based on the contents of the .txt - lastname_firstname_word
#' @param name of a file in the exported folder. If not provided then a file chooser will be used
#' @return The original names and matching output
#' @description This function renames files exported by AAA. Input is the path to the folder
#' containing exported files, or select one of the files in the folder with a chooser dialog.
#' Renaming proceeds without warning. Likely to be fragile if there is lots of punctuation in the
#' word list.
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

        speaker <- gsub("[,[:blank:]]+", "_", speaker)
        words <- gsub("[,[:blank:]]+", "_", words)
        newnames <- paste0(speaker, "_", words);
        names(newnames) <- all.prefixes
        ## Now list the different files we need to rename
        original <- list.files(path=txtfile, pattern="File[[:digit:]]{3}.*\\..*", full.names=TRUE)
        prefixes <- gsub("(File[[:digit:]]{3}).+", "\\1", basename(original))
        suffixes <- gsub("(File[[:digit:]]{3})(.+)", "\\2", basename(original))
        newprefix <- newnames[file.path(dirname(original), prefixes)]
        outname <- file.path(dirname(original), paste0(newprefix, "_", suffixes))
        outname <- gsub("_+", "_", outname)
        outname <- gsub("_\\.", ".", outname)
        file.rename(from=original, to=outname)
        return(cbind(original, outname))
    }
