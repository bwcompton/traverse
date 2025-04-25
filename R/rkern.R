'rkern' <- function(h, focal, cost, search = 3, missing = 1e6, style = 'kernel',
                    focalresist = FALSE, selected = 1, cellsize, slope = 1) {

   # This function builds resistant kernels from input data represented as R matrices.
   #	h			spread value (s.d. or bandwidth of kernel), in m
   #	focal		matrix corresponding to cost, with focal cells marked by nonzero values,
   #				and background cells marked by 0.  All focal cells are treated as kernel
   #				seeds
   #	cost		input landscape, consisting of a resistant surface with a specified
   #				cost at each cell.  Costs may range from 1 (minimal resistance) to a
   #				very large number (representing absolute barriers).  Costs represent
   #				the cost of crossing a single cell
   #	search		search distance, in s.d.
   #	missing		resistance value to use for missing data cells
   #	style
   #		= 'kernel'	give resistant kernel (sum across all kernels for each cell)
   #		= 'cell'	give sum of resistances of selected cells for each focal
   #					cell
   #	focalresist	if TRUE, subtract focal cell's resistance when building kernel.  Default =
   #				FALSE (old behavior).
   #	selected	maximum resistance value to consider selected (only when style =
   #				'cell')
   #	cellsize	size of cells, in m
   #	slope		optional slope distance grid, gives multiplier on distance for
   #				each cell.
   # B. Compton, 4 Jan 2008
   # 27 Feb 2012: rearrange rawspread call to conform to Ethan's version; print % done
   # 15 May 2014: change raw.spread to rawspread thanks to R's changing fashions
   # 2-3 Jun 2014: add focalresist



   focal[is.na(focal)] <- 0
   cost[is.na(cost)] <- missing
   if(is.null(dim(slope)))
      slope <- matrix(1,dim(focal)[1], dim(focal)[2])
   b <- ceiling(h * search / cellsize)		# Buffer in cells
   r <- (1:(b * 2 + 1)) - (b + 1)			# Maximum kernel radius
   q <- (length(r) + 1) / 2
   t <- (h * search) - focalresist * cellsize
   q <- rawspread(matrix(cellsize, length(r), length(r)), t, q, q)

   s <- (q!= 0) * dnorm((search * h - q) / h)
   e <- (s != 0) & (s >= s[ceiling(dim(s)[1] / 2), 1])	# mask to prevent rectangular artifacts
   s <- sum(w <- e * s)		# scaling factor: kernel in non-resistant landscape = 1
   z <- matrix(0, dim(focal)[1], dim(focal)[2])

   a <- matrix(1:dim(focal)[1], dim(focal)[1], dim(focal)[2])
   b <- matrix(1:dim(focal)[2], dim(focal)[1], dim(focal)[2], byrow = TRUE)
   f <- cbind(a[focal > 0], b[focal > 0])	# list of focal cells

   if(dim(f)[1] != 0)
      for(i in 1:dim(f)[1]) {					# for each focal cell,
         cat('\r   ',round(i / dim(f)[1] * 100, 2),'%   ', sep = '')
         k <- f[i,1] + r
         k <- k[e1 <- (k >= 1) & (k <= dim(focal)[1])]
         l <- f[i,2] + r
         l <- l[e2 <- (l >= 1) & (l <= dim(focal)[2])]
         x <- cost[k,l] * cellsize * matrix(pmax(1, slope[k,l]), length(k), length(l))
         t <- max(.0001, (h * search) - focalresist * cost[f[i,1],f[i,2]] * cellsize)	# if focalresist, subtract cost of focal cell from spread value
         q <- rawspread(x, t, (1:length(k))[k == f[i,1]],
                        (1:length(l))[l == f[i,2]])
         u <- s
         if(any(c(e1,e2) == 0)) 				# if near edge,
            u <- sum(w[e1 == 1, e2 == 1])	#	recalculate scaling factor
         t <- dnorm(((h * search) - q) / h)
         q <- e[e1, e2] * (q != 0) * t / u

         if(style == 'kernel')				# if summing kernels,
            z[k,l] <- z[k,l] + q * 1e6		#	and make it bigger for display
         else								# else, sum kernel for cell
            z[f[i,1],f[i,2]] <- sum(q * (cost[k,l] <= selected))
      }
   z
}
