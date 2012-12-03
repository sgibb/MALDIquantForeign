## Copyright 2012 Sebastian Gibb
## <mail@sebastiangibb.de>
##
## This is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## It is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## See <http://www.gnu.org/licenses/>

#' This function removes spaces and punctuations from filenames.
#' 
#' @return filename
#'
#' @keywords internal
#' @rdname cleanFilename
.cleanFilename <- function(x) {
  return(gsub(pattern="([[:punct:]]|[[:space:]])+", replacement="_", x=x))
}

#' Determine file extension
#'
#' @param x \code{character}, filename.
#' 
#' @return \code{character}, file extension.
#'
#' @seealso \code{\link[MALDIquant]{MassSpectrum-class}}
#' @keywords internal
#' @rdname fileExtension
#' @examples
#' library("MALDIquantForeign")
#' files <- c("/home/foo/bar.txt", "foobar.pdf")
#' MALDIquantForeign:::.fileExtension(files)
#'
.fileExtension <- function(x) {
  pos <- regexpr(pattern="\\.([[:alnum:]]+)$|(/|\\\\)+[^.\\\\/]+$", text=x)
  return(ifelse(pos > -1L, substring(x, pos+1L), x))
}

#' @keywords internal
.withoutFileExtension <- function(x) {
  return(sub(pattern="\\.[[:alnum:]]+\\.?(gz|bz2|xz)?$|(/|\\\\)+[^.\\\\/]+$",
             replacement="", x=x))
}

#' @keywords internal
.cutFilenames <- function(x) {
  l <- strsplit(x, split=.Platform$file.sep, fixed=TRUE)

  nCol <- unlist(lapply(l, length))
  mCol <- max(nCol)

  m <- matrix(NA, nrow=length(x), ncol=mCol)

  for (i in seq(along=l)) {
    cols <- 1:nCol[i]
    m[i, cols] <- l[[i]] 
  }

  isIdentical <- apply(m, 2, function(co)all(co[1] == co))
  isIdentical[is.na(isIdentical)] <- FALSE

  m <- as.matrix(m[, !isIdentical])

  if (length(m)) {
    filenames <- apply(m, 1, function(r) {
      do.call(file.path, as.list(na.omit(r)))
    })
  } else {
    filenames <- basename(x)
  }

  return(filenames)
}

.uniqueBaseFilenames <- function(x, fileExtension="csv",
                                 sep="_") {
  filenames <- .cutFilenames(.withoutFileExtension(x))
  filenames <- .cleanFilename(filenames)
  filenames <- make.unique(filenames, sep=sep)
  return(paste(filenames, fileExtension, sep="."))
}

