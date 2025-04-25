'findcell' <- function(x, h) {
   # Convert x,y point from map units to row, column in grid window
   # B. Compton, 27 Feb 2012 (from FINDCELL in CAPS)



   rev(c(1,0) + c(-1,1) * (c(0,h$nrow) - floor((x - c(h$xll, h$yll)) / h$cellsize)))
}
