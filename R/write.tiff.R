'write.tiff' <- function(x, template, name, what = '', verbose = FALSE, timing = FALSE) {

   # write.tiff
   # write matrix as a geoTIFF, which bells and whistles
   # Arguments:
   #   x          matrix to write
   #   template   raster object that x conforms to (e.g., input raster)
   #   name       path and name of result
   #   what       what to call it in chatter (start with lowercase)
   #   verbose    whether to chatter
   #   timing     whether to report timing (only if chatter = TRUE)
   # B. Compton, 29 May 2021



   chatter(verbose, 'Writing ', what, ' grid...')
   t <- proc.time()[3]

   template[] <- x
   writeRaster(template, name, format = 'GTiff', overwrite = TRUE)

   what <- paste(toupper(substring(what, 1, 1)), substring(what, 2), sep = '')
   chatter(verbose, what, ' grid written to ', name)

   chatter(timing & verbose, '  Elapsed time = ', proc.time()[3] - t, ' s')
}
