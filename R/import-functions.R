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

#' Import files
#'
#' This function provides a general interface to import different file formats
#' into \code{\link[MALDIquant]{MassSpectrum-class}} objects.
#'
#' @details
#' Specific import functions:
#' \tabular{ll}{
#'  txt \tab \code{\link[MALDIquantForeign]{importTxt}} \cr
#'  tab \tab \code{\link[MALDIquantForeign]{importTab}} \cr
#'  csv \tab \code{\link[MALDIquantForeign]{importCsv}} \cr
#'  fid \tab \code{\link[MALDIquantForeign]{importBrukerFlex}} \cr
#'  mzXML \tab \code{\link[MALDIquantForeign]{importMzXml}} \cr
#'  mzML \tab \code{\link[MALDIquantForeign]{importMzMl}} \cr
#' }
#'
#' \code{pattern}: Sometimes unusual file extensions are used (e.g.
#' \code{"*.xml"} for mzXML files). In this case a specific 
#' \code{pattern} could be defined to import files with an unusual file 
#' extension (e.g. \code{pattern="^.*\\.xml$"} to read all \code{*.xml} 
#' files in a directory; see \code{\link[base]{regexp}} for details).
#'
#' @param path \code{character}, path to directory or file which should be read
#'  in.
#' @param type \code{character}, file format. If \code{type} is set to 
#'  \dQuote{auto} the file extension (if \code{path} is a directory the
#'  most represented file extension) is used.
#' @param pattern \code{character}, a regular expression to find files in a
#'  directory (see details).
#' @param verbose \code{logical}, verbose output?
#' @param \ldots arguments to be passed to specific import functions.
#'
#' @return a \code{list} of \code{\link[MALDIquant]{MassSpectrum-class}}
#'  objects.
#' @seealso
#' \code{\link[MALDIquant]{MassSpectrum-class}} 
#' @author Sebastian Gibb
#' @references \url{http://strimmerlab.org/software/maldiquant/}
#' @examples
#'
#' library("MALDIquant")
#' library("MALDIquantForeign")
#'
#' ## get example directory
#' exampleDirectory <- system.file("tests/data", package="MALDIquantForeign")
#'
#' ## import mzXML files
#' s <- import(exampleDirectory, type="mzXML")
#'
#' ## import tab delimited file with different file extension (default: *.tab)
#' s <- import(exampleDirectory, type="tab", pattern="^.*\\.txt") 
#'
#' ## import single mzML file
#' s <- import(file.path(exampleDirectory, "tiny1.mzML1.1.mzML"))
#'
#' @rdname import-functions
#' @export
import <- function(path, type="auto", pattern, verbose=FALSE, ...) {

  e <- file.exists(path)

  if (!all(e)) {
    stop(sQuote(path[!e]), " doesn't exist!")
  }

  i <- pmatch(tolower(type), c("auto", importFormats$type), nomatch=0,
              duplicates.ok=FALSE)-1

  if (i == -1) {
    stop("File type ", sQuote(type), " is not supported!")
  } else if (i == 0) {
    ## auto
    return(.importAuto(path=path, pattern=pattern, verbose=verbose, ...))
  } else {
    ## specific type
    if (missing(pattern)) {
      pattern <- importFormats$pattern[i]
    }
    handler <- importFormats$handler[i]
    return(unlist(lapply(.files(path=path, pattern=pattern), handler, ...)))
  }
}

#' Import text files
#'
#' This function imports different text file formats
#' into \code{\link[MALDIquant]{MassSpectrum-class}} objects.
#'
#' \code{importTab}, \code{importTxt} and \code{importCsv} use
#' \code{\link[utils]{read.table}} with different defaults.
#'
#' @param path \code{character}, path to directory or file which should be read
#'  in.
#' @param \ldots arguments to be passed to \code{\link[utils]{read.table}}.
#'
#' @return a \code{list} of \code{\link[MALDIquant]{MassSpectrum-class}}
#'  objects.
#' @seealso
#' \code{\link[MALDIquant]{MassSpectrum-class}},
#' \code{\link[utils]{read.table}}
#' @author Sebastian Gibb
#' @references \url{http://strimmerlab.org/software/maldiquant/}
#' @examples
#'
#' library("MALDIquant")
#' library("MALDIquantForeign")
#'
#' ## get example directory
#' exampleDirectory <- system.file("tests/data", package="MALDIquantForeign")
#'
#' ## import txt files
#' s <- importTxt(exampleDirectory) 
#'
#' ## import csv files
#' s <- importCsv(exampleDirectory)
#'
#' @rdname importTab-functions
#' @export
importTxt <- function(path, ...) {
  return(import(path=path, type="txt", ...))
}

#' @rdname importTab-functions
#' @export
importTab <- function(path, ...) {
  return(import(path=path, type="tab", ...))
}

#' @rdname importTab-functions
#' @export
importCsv <- function(path, ...) {
  return(import(path=path, type="csv", ...))
}

#' Import Bruker Daltonics *flex files
#'
#' This function imports files in Bruker Daltonics *flex-series file format
#' into \code{\link[MALDIquant]{MassSpectrum-class}} objects.
#'
#' @param path \code{character}, path to directory or file which should be read
#'  in.
#' @param \ldots arguments to be passed to
#' \code{\link[readBrukerFlexData]{readBrukerFlexFile}}.
#'
#' @return a \code{list} of \code{\link[MALDIquant]{MassSpectrum-class}}
#'  objects.
#' @seealso
#' \code{\link[MALDIquant]{MassSpectrum-class}},
#' \code{\link[readBrukerFlexData]{readBrukerFlexFile}}
#' @author Sebastian Gibb
#' @references \url{http://strimmerlab.org/software/maldiquant/}
#' @examples
#'
#' library("MALDIquant")
#' library("MALDIquantForeign")
#'
#' ## get example directory
#' exampleDirectory <- system.file("tests/data", package="MALDIquantForeign")
#'
#' s <- importBrukerFlex(exampleDirectory) 
#'
#' @rdname importBrukerFlex-functions
#' @export
importBrukerFlex <- function(path, ...) {
  return(import(path=path, type="fid", ...))
}

#' Import mzXML files
#'
#' This function imports files in mzXML file format
#' into \code{\link[MALDIquant]{MassSpectrum-class}} objects.
#'
#' @param path \code{character}, path to directory or file which should be read
#'  in.
#' @param \ldots arguments to be passed to
#' \code{\link[readMzXmlData]{readMzXmlFile}}.
#'
#' @return a \code{list} of \code{\link[MALDIquant]{MassSpectrum-class}}
#'  objects.
#' @seealso
#' \code{\link[MALDIquant]{MassSpectrum-class}},
#' \code{\link[readMzXmlData]{readMzXmlFile}}
#' @author Sebastian Gibb
#' @references \url{http://strimmerlab.org/software/maldiquant/}, \cr
#' Definition of \code{mzXML} format:
#' \url{http://tools.proteomecenter.org/mzXMLschema.php}
#' @examples
#'
#' library("MALDIquant")
#' library("MALDIquantForeign")
#'
#' ## get example directory
#' exampleDirectory <- system.file("tests/data", package="MALDIquantForeign")
#'
#' ## import
#' s <- importMzXml(exampleDirectory) 
#'
#' @rdname importMzXml-functions
#' @export
importMzXml <- function(path, ...) {
  return(import(path=path, type="mzxml", ...))
}

#' Import mzML files
#'
#' This function imports files in mzML file format
#' into \code{\link[MALDIquant]{MassSpectrum-class}} objects.
#'
#' @param path \code{character}, path to directory or file which should be read
#'  in.
#' @param verbose \code{logical}, verbose output?
#'
#' @return a \code{list} of \code{\link[MALDIquant]{MassSpectrum-class}}
#'  objects.
#' @seealso
#' \code{\link[MALDIquant]{MassSpectrum-class}},
#' \code{\link[readMzXmlData]{readMzXmlFile}}
#' @author Sebastian Gibb
#' @references \url{http://strimmerlab.org/software/maldiquant/}, \cr
#' Definition of \code{mzML} format:
#' \url{http://www.psidev.info/mzml_1_0_0\%20}
#' @examples
#'
#' library("MALDIquant")
#' library("MALDIquantForeign")
#'
#' ## get example directory
#' exampleDirectory <- system.file("tests/data", package="MALDIquantForeign")
#'
#' ## import
#' s <- importMzMl(exampleDirectory) 
#'
#' @rdname importMzMl-functions
#' @export
importMzMl <- function(path, verbose=FALSE) {
  return(import(path=path, type="mzml", verbose=verbose))
}

