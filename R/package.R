## Copyright 2012-2013 Sebastian Gibb
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

#' Import/Export routines for MALDIquant
#'
#' This package allows to read and write different file formats of mass
#' spectrometry data into/from MALDIquant objects.
#'
#' \tabular{ll}{
#' Package: \tab MALDIquantForeign \cr
#' Version: \tab 0.3.1\cr
#' Date: \tab 2013-03-02\cr
#' License: \tab GPL (>= 3)\cr
#' URL: \tab http://strimmerlab.org/software/maldiquant/\cr
#' }
#'
#' @docType package
#' @name MALDIquantForeign-package
#' @author Sebastian Gibb \email{mail@@sebastiangibb.de}
#' @references \url{http://strimmerlab.org/software/maldiquant/}
#' @keywords package
#'
#' @import MALDIquant
#' @import readMzXmlData
#' @importFrom base64enc base64encode
#' @importFrom digest digest
#' @importFrom downloader download
#' @importFrom readBrukerFlexData readBrukerFlexFile
#' @importFrom XML xmlEventParse xmlParse xmlValue xpathApply xpathSApply
#'
NULL

