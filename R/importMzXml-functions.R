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
.importMzXml <- function(file, verbose=FALSE, ...) {

  if (!require(readMzXmlData)) {
    stop("Could not load package ", sQuote("readMzXmlData"), ".")
  }
  
  s <- readMzXmlData:::.readMzXmlFile(mzXmlFile=file, 
                                      verbose=verbose, ...)
  return(lapply(s, function(x)createMassSpectrum(mass=x$spectrum$mass,
                                                 intensity=x$spectrum$intensity,
                                                 metaData=x$metaData)))
}

