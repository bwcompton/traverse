'check.file' <- function(filename, path, what, verbose, ext = NULL, require = FALSE, result = FALSE) {

    # check.file
    # Add path to file name, check whether it exists, give understandable error if missing, and print chatter message
    # Arguments:
    #   filename    name of file
    #   path        path to prepend to filename if it doesn't start with \ or <drive>:
    #   what        what to call the missing thing in chatter or error message (e.g., 'Shapefile')
    #   verbose     if TRUE, chatter name of file
    #   ext         append file extension if it's not already there
    #   require     if TRUE don't allow filename to be blank or null; if false, ignore empty filenames
    #   result      if TRUE, filename is a result, so path must exist, but file doesn't have to
    # B. Compton, 2 Apr 2021
    # 25 Apr 2021: combine with add.path and extend to do all filename pre-processing



    # deal with empty filename
    if(require) {
        if(is.null(filename))
            stop(what, ' not supplied.')
        if(nchar(filename) == 0)
            stop(what, ' not supplied.')
    }
    else {
        if(is.null(filename))
            return(NULL)
        if(nchar(filename) == 0)
            return(NULL)
    }

    # add path if needed
    filename <- add.path(path, filename)

    # add extension if needed
    if(!is.null(ext))
        if(1 != length(grep(paste('[.]', ext, sep = ''), filename)))
            filename <- paste(filename, '.', ext, sep = '')

    # check to see if file exists (or path, if it's a result name)
    if(result) {
        t <- strsplit(filename, '/')[[1]]
        if(!file.exists(paste(t[-length(t)], collapse = '/')))
            stop('Path for result ', what, ' ', filename, ' does not exist.')
    }
    else
        if(!file.exists(filename))
            stop(what, ' ', filename, ' does not exist.')

    # chatter
    if(verbose)
        cat(what, ' = ', filename, '\n', sep = '')

    filename
}
