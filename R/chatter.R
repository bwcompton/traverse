'chatter' <- function(verbose, ...) {
   
   # chatter
   # Display text if verbose is TRUE
   # Arguments:
   #     verbose  switch - actually display it?
   #     ...      text to display
   # B. Compton, 25 Apr 2021
   
   
   
   if(verbose)
      cat(..., '\n', sep = '')
}