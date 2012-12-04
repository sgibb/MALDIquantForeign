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
setMethod(f="export",
  signature=signature(x="AbstractMassObject"),
  definition=function(x, file, type, force=FALSE, ...) {

  if (file.exists(file) && !force) {
    stop("File already exists! Use ", sQuote("force=TRUE"), " to overwrite it.")
  }

  if (missing(type) || pmatch(tolower(type), "auto", nomatch=0,
                              duplicates.ok=FALSE)) {
    type <- .fileExtension(file)
  }

  i <- pmatch(tolower(type), exportFormats$type, nomatch=0, duplicates.ok=FALSE)

  if (i <= 0) {
    stop("File type ", sQuote(type), " is not supported!")
  } else {
    handler <- exportFormats$handler[i]
    return(do.call(handler, list(x=x, file=file, ...)))
  }
})

#' @export
setMethod(f="export",
  signature=signature(x="list"),
  definition=function(x, path, type, force=FALSE, ...) {

  stopifnot(isMassObjectList(x))

  if (!file.exists(path) && force) {
    dir.create(path, showWarnings=FALSE, recursive=TRUE)  
  }

  if (!file.info(path)$isdir) {
    stop(sQuote(path), " is no directory!")
  }

  ## stop if directory isn't writeable
  if (file.access(path, 2) != 0) {
    stop("No permissions to write into ", sQuote(path), "!")
  }

  i <- pmatch(tolower(type), exportFormats$type, nomatch=0, duplicates.ok=FALSE)

  if (i <= 0) {
    stop("File type ", sQuote(type), " is not supported!")
  } else {
    filenames <- .composeFilename(x, fileExtension=exportFormats$extension[i])
    filenames <- file.path(path, filenames)
     
    for (i in seq(along=x)) {
      if (!missing(peaks)) {
        export(x=x[[i]], file=filenames[i], peaks=peaks[[i]], type=type, 
               force=force, ...)
      } else {
        export(x=x[[i]], file=filenames[i], type=type, force=force, ...)
      }
    }
  }
  invisible()
})

## tab
#' @export
setMethod(f="exportTab",
          signature=signature(x="AbstractMassObject"),
          definition=function(x, file, force=FALSE, ...) {
  export(x, file=file, type="tab", force=force, ...)
})

#' @export
setMethod(f="exportTab",
          signature=signature(x="list"),
          definition=function(x, path, force=FALSE, ...) {
  export(x, path=path, type="tab", force=force, ...)
})

## csv
#' @export
setMethod(f="exportCsv",
          signature=signature(x="AbstractMassObject"),
          definition=function(x, file, force=FALSE, ...) {
  export(x, file=file, type="csv", force=force, ...)
})

#' @export
setMethod(f="exportCsv",
          signature=signature(x="list"),
          definition=function(x, path, force=FALSE, ...) {
  export(x, path=path, type="tab", force=force, ...)
})

## msd 
#' @export
setMethod(f="exportMsd",
          signature=signature(x="AbstractMassObject"),
          definition=function(x, file, force=FALSE, peaks, ...) {
  stopifnot(isMassSpectrum(x))

  if (!missing(peaks)) {
    stopifnot(isMassPeaks(peaks))
  }

  export(x, file=file, type="msd", force=force, peaks=peaks,  ...)
})

#' @export
setMethod(f="exportMsd",
          signature=signature(x="list"),
          definition=function(x, path, force=FALSE, peaks, ...) {
  stopifnot(isMassSpectrumList(x))

  if (!missing(peaks)) {
    stopifnot(isMassPeaksList(peaks))
  }

  export(x, path=path, type="msd", force=force, peaks=peaks, ...)
})

