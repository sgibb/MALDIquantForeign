## Copyright 2012 Sebastian Gibb
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

#' @keywords internal
.importMzMl <- function(file, verbose=FALSE, ...) {

  if (!require(readMzXmlData)) {
    stop("Could not load package ", sQuote("readMzXmlData"), ".")
  }
  
  if (verbose) {
    message("Reading spectrum from ", sQuote(file), " ...")
  }
  
  if (!file.exists(file)) {
    stop("File ", sQuote(file), " doesn't exists!")
  }

  ## read file
  s <- .parseMzMl(file=file, verbose=verbose)

  spectra <- lapply(s$spectra, function(x, globalS=s) {
    m <- modifyList(s$metaData, x$metaData)
    m$file <- file
    return(createMassSpectrum(mass=x$mass,
                              intensity=x$intensity,
                              metaData=m))
  })

  return(spectra)
}

#' @keywords internal
.import.mzml <- function(...) {
  return(.importMzMl(...))
}

