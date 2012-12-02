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
.importTab <- function(file, verbose=FALSE, ...) {
  
  ## load ms file
  s <- read.table(file=file, ...)
  
  return(list(createMassSpectrum(mass=s[, 1], intensity=s[, 2],
                                 metaData=list(file=file))))
}

#' @keywords internal
.import.tab <- function(...) {
  return(.importTab(...))
}

#' @keywords internal
.importCsv <- function(file, verbose=FALSE, sep=.autoSep(file),
                       header=.autoHeader(file, sep), ...) {
  
  return(.importTab(file=file, verbose=verbose, sep=sep, header=header, ...))
}

#' @keywords internal
.import.csv <- function(...) {
  return(.importCsv(...))
}

#' @keywords internal
.autoHeader <- function(file, sep="\t") {
  l <- readLines(file, n=1)
  l <- gsub(pattern='[\\\\"]*', replacement="", x=l)
  l <- strsplit(l, split=sep)[[1]][1]
  return(!is.numeric(type.convert(l, as.is=TRUE)))
}

#' @keywords internal
.autoSep <- function(file, sep=c(",", ";", "\t")) {
  l <- readLines(file, n=1)
  pattern <- paste(".+", sep, ".+", sep="")
  i <- vapply(pattern, function(x) {
    g <- gregexpr(pattern=x, text=l)[[1]]
    return(all(g > 0) & length(g) == 1)
  }, logical(1))

  if (any(i)) {
    return(sep[i])
  } else {
    return(sep[1])
  }
}

