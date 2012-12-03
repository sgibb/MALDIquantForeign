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
    return(.import.auto(path=path, verbose=verbose, ...))
  } else {
    ## specific type
    if (missing(pattern)) {
      pattern <- importFormats$pattern[i]
    }
    handler <- importFormats$handler[i]
    return(unlist(lapply(.files(path=path, pattern=pattern), handler, ...)))
  }
}

importTxt <- function(path, ...) {
  return(import(path=path, type="txt", ...))
}

importTab <- function(path, ...) {
  return(import(path=path, type="tab", ...))
}

importCsv <- function(path, ...) {
  return(import(path=path, type="csv", ...))
}

importBrukerFlex <- function(path, ...) {
  return(import(path=path, type="fid", ...))
}

importMzXml <- function(path, ...) {
  return(import(path=path, type="mzxml", ...))
}

importMzMl <- function(path, ...) {
  return(import(path=path, type="mzml", ...))
}

