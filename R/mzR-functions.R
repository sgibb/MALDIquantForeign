## Copyright 2015 Sebastian Gibb
## <mail@sebastiangibb.de>
##
## This file is part of MALDIquantForeign for R and related languages.
##
## MALDIquantForeign is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## MALDIquantForeign is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with MALDIquantForeign. If not, see <http://www.gnu.org/licenses/>

.openMSfile <- function(file, verbose) {
  if (!requireNamespace("mzR", quietly=TRUE)) {
    stop("For \'backend=\"mzR\"\' the mzR package from ",
         "http://bioconductor.org/packages/mzR/ is needed. ",
         "Use \'backend=\"default\"\' instead.")
  }

  s <- openMSfile(file=file, verbose=verbose)
  lapply(seq_along(s), function(i) {
    m <- header(s, scan=i)
    m$file <- file
    p <- peaks(s, scan=i)
    .createMassObject(mass=p[, 1L], intensity=p[, 2L], metaData=m,
                      centroided=centroided,
                      massRange=massRange, minIntensity=minIntensity,
                      verbose=verbose)
  })
}
