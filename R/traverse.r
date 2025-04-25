#' CAPS Traversability Metric
#'
#' Estimates local connecedness with resistant kernels
#'
#' @param bandwidth standard deviation of the kernel (m).
#' @param land name of landcover raster with integer classes for each landcover type.
#' @param result name of result raster to create.
#' @param resist file with n x 2 matrix of classes in landcover and resistance for each
#' class. Resist should be tab-delimited, with column names 'class' and
#' 'resistance', and an optional column designated in focal, below. Other
#' columns may be included but will be ignored. Semicolons indicate
#' comments. Resistance values must be >= 1. Omitted classes get a resistance of 1.
#' @param window Area of the landscape to run for. Supply a polygon shapefile.
#' @param path base path prepended to input and result names/paths. Inputs that
#' include a complete path (starting with / or a drive letter) don't use path. This
#' option helps keep the inputs cleaner, and makes for easy switching to different
#' sets of inputs and results.
#' @param focal designates focal cells to build kernels for. If focal is a number,
#' all cells with resistance <= focal are treated as focal cells.
#' Alternatively, focal can be the name of a column in the resistance
#' table; this column should have 1's for classes to treat as focal,
#' and 0's for others. The default is focal = 1. If running for example
#' cells, focal is ignored.
#' @param focalresist if TRUE, subtract focal cell's resistance when building kernel. Default =
#' FALSE.
#' @param search search distance, indicating the maximum spread of a kernel as a multiple
#' of bandwidth. Search is a precision vs. speed tradeoff. Values from 2 to
#' 4 are reasonable. Default = 3.
#' @param example a vector of x,y pairs (in map units) indicating cells to run examples for.
#' If included, return the kernel for each example (summed where they overlap).
#' Example values are multiplied by 1E6 so they're not absurdly tiny. Normally,
#' example = NULL, and traversability is run normally for each cell. If example
#' is included, focal is ignored.
#'
#' @details
#' CAPS (Conservation Assessment and Prioritization System) traversability metric (also
#' known as old-style connectedness and local connectedness).  Builds a resistant kernel
#' for each focal cell in landcover using resistances supplied in a table.  In normal
#' mode, each focal cell gets the sum of the volume of a resistant kernel built on that
#' cell, divided by the sum of a standard kernel built on that cell.  This gives a measure
#' of local connectedness for each focal (usually undeveloped) cell in the landscape.
#' Alternatively, example points may be specified, and resistant kernels are built for
#' each example cell; these kernels are summed where they overlap.
#'
#' Traverse is based on resistant kernels (Compton et. al 2007), which have
#' been used in a number of conservation applications since 2003,
#' including estimating local and regional connectivity (McGarigal et al. 2018) and
#' building terrestrial and aquatic conservation cores in Designing Sustainable
#' Landscapes/Nature's Network (McGarigal et al. 2017); they have also been used in
#' TNC's Resilient Sites for Terrestrial Conservation and Massachusetts Natural
#' Heritage's Living Waters and BioMap 2.
#'
#' @section Notes:
#' 1. Resistance values must range from 1 to infinity. The spread value starts in
#' each focal cell (all edge cells of each seed) at bandwidth / cell size. At each
#' cell, the cell's resistance x multiplier is subtracted from the spread value. For
#' example, a bandwidth of 5000 m when the cell size is 30 m gives a spread value of
#' 166.67. The spread will stop once it has passed through cells with a cumulative
#' resistance * multiplier of 166.67. Resistances greater than or equal to this value
#' will stop the spread at a single cell, thus these cells act as complete barriers.
#' 2. Raster inputs may be either Arc grids or geoTIFFs (other formats will likely
#' work).
#'
#' @section References:
#' Compton, B.W., K. McGarigal, S.A. Cushman, and L.R. Gamble. 2007. A resistant-kernel
#' model of connectivity for amphibians that breed in vernal pools. Conservation
#' Biology 21:788-799. \doi{10.1111/j.1523-1739.2007.00674.x}.
#'
#' McGarigal, K., B.W. Compton, E.B. Plunkett, W.V. DeLuca, J. Grand, E. Ene, and
#' S.D. Jackson. 2018. A landscape index of ecological integrity to inform landscape
#' conservation. Landscape Ecology 33:1029-1048. \doi{10.1007/s10980-018-0653-9}.
#'
#' McGarigal K., B.W. Compton, E.B. Plunkett, W.V. DeLuca, and J. Grand. 2017.
#' Designing sustainable landscapes: landscape conservation design. Report to the
#' North Atlantic Conservation Cooperative, US Fish and Wildlife Service, Northeast
#' Region.
#' \url{http://landeco.umass.edu/web/lcc/dsl/technical/DSL_documentation_landscape_design.pdf}
#' @section Author:
#' Bradley W. Compton <bcompton@@umass.edu>
#' @export
#  C++ code that does resistant kernels. Package source is at https://github.com/ethanplunkett/gridprocess
#' @import gridprocess
#  GIS processing for raster data
#' @import raster
#' @import rgdal
#' @import rgeos
#' @importFrom stats dnorm
#' @importFrom utils read.table
#' @examples
#' ### Set up temporary directory for examples
#' require(traverse)
#' dp <- paste(shortPathName(system.file('exampledata', package='traverse')), '/.', sep = '')
#' dir <- tempdir()
#' if(!file.exists(dir)) dir.create(dir)
#' file.copy(dp, dir, recursive=TRUE)
#' cat('Example data and results will be in', dir)
#'
#' ### 1. example kernels (creates examples.tif)
#' traverse(1000, resist = 'resist.txt', land = 'land.tif', window = 'clip', result = 'examples.tif',
#' path = dir, example = c(110377, 893424, 114009, 898109, 118869, 895284))
#' ### 2. a low-bandwidth example of traversability (creates traverse.tif; takes a few minutes to run)
#' traverse(200, resist = 'resist.txt', land = 'land.tif', window = 'clip', result = 'traverse.tif',
#' path = dir)
# B. Compton, 27 Feb 2012-23 Aug 2021


'traverse' <- function(bandwidth, land, resist, result, window = NULL, path = '',
                       focal = 1, focalresist = FALSE, search = 3, example = NULL) {

   if(!is.null(path))
      cat('path = ', path, '\n', sep = '')
   land <- check.file(land, path, 'Landcover', verbose = TRUE, require = TRUE)
   resist <- check.file(resist, path, 'Resistance table', verbose = TRUE, require = TRUE)
   window <- check.file(window, path, 'Window', verbose = TRUE, ext = 'shp', require = FALSE)
   result <- check.file(result, path, 'Result', verbose = TRUE, ext = 'tif', result = TRUE, require = TRUE)


   # read raster data (and maybe clip window)
   xy <- raster(land)
   buf <- ceiling(bandwidth * search)		# buffer in map units
   if(is.null(window)) {						# if window is null, use whole grid
      header <- list(xll = extent(xy)[1], yll = extent(xy)[3], nrow = nrow(xy), ncol = ncol(xy), cellsize = res(xy)[1])
   } else {
      clip <- suppressWarnings(readOGR(dsn = window, verbose = FALSE))
      q <- extent(clip)
      q <- q + c(-buf, buf, -buf, buf)
      xy <- crop(xy, q)
      header <- list(xll = extent(xy)[1], yll = extent(xy)[3], nrow = nrow(xy), ncol = ncol(xy), cellsize = res(xy)[1])
   }
   cell.buffer <- ceiling(buf / header$cellsize) # buffer in cells
   win <- c(header$xll, header$yll, header$xll + (header$ncol - .5) * header$cellsize, header$yll + (header$nrow - .5) * header$cellsize)

   w <- c(findcell(win[c(1,4)], header),findcell(win[c(3,2)], header))	      # window in terms of cells
   w[c(3,4)] <- w[c(3,4)] - (w[c(1,2)] - 1)
   x <- as.matrix(xy)

   # get resistance table (omitted classes get resistance = 1)
   r <- read.table(resist, header = TRUE, sep = '\t', comment.char = ';')
   r <- r[order(r$class),]
   cost <- matrix(c(1,r$resistance)[1 + match(x, r$class, nomatch = 0)], dim(x)[1], dim(x)[2])
   # deal with examples or not
   if(is.null(example)) {						# if regular run,
      type <- 'cell'								#	we'll sum kernel into f cell
      if(is.numeric(focal))					#   if focal is numeric, we'll run for all cells with cost <= focal
         f <- matrix(as.numeric(cost <= focal), dim(x)[1], dim(x)[2])
      else									#   else, look up focal cells in resistance matrix, in column referred to by focal
         f <- matrix(c(1,r[,focal])[1 + match(x, r$class, nomatch = 0)], dim(x)[1], dim(x)[2])
      f[1:cell.buffer,] <- 0						#	don't do any cells in buffer
      f[(1 + dim(x)[1] - cell.buffer):dim(x)[1],] <- 0
      f[,1:cell.buffer] <- 0
      f[,(1 + dim(x)[2] - cell.buffer):dim(x)[2]] <- 0
   } else {										# else, example,
      type <- 'kernel'						# 	we'll return summed kernels
      e <- matrix(example, length(example)/2, 2, byrow = TRUE)	#   convert example points to cells
      e <- t(apply(e, 1, FUN = findcell, h = header)) - matrix(rep(w[c(1,2)],3), dim(e)[1], dim(e)[2], byrow = TRUE) + 1
      f <- matrix(0, dim(x)[1], dim(x)[2])	#	and run only for example points
      f[e] <- 1
   }
   cat('Running traversability for ',formatC(sum(f), format='d', big.mark = ','),(if(!is.null(example)) ' example'),' cell',(if(sum(f) != 1) 's'),'...\n', sep = '')
   z <- rkern(bandwidth, f, cost, search, style = type, focalresist = focalresist, selected = 1e6, cellsize = header$cellsize)

   write.tiff(z, xy, result, 'results', verbose = FALSE, timing = FALSE)
   cat('\nTraversability has been run for ',land,'; results written to ',result,'\n', sep = '')
}
