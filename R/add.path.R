'add.path' <- function(path, file) {

    # add.path
    # Adds path to a filename if it doesn't already have a path
    # Filename already has a path if it starts with / or <drive letter>:
    # Arguments:
    #   path    path to prepend (optionally ending with /)
    #   file    filename
    # B. Compton, 2 Apr 2021
    # 23 Aug 2021: behave properly if path = ''



    if(!is.null(file) && nchar(file) > 0 && !is.null(path) && nchar(path) > 0) {
        s <- ifelse(substr(path, nchar(path), nchar(path)) != '/', '/', '')
        if(1 != length(grep('^/|^[a-zA-Z]:', file)))
            file <- paste(path, s, file, sep = '')
    }
    file
}
