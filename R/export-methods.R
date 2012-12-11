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

#' Export files
#'
#' This function provides a general interface to export
#' \code{\link[MALDIquant]{AbstractMassObject-class}} objects (e.g. 
#' \code{\link[MALDIquant]{MassSpectrum-class}},
#' \code{\link[MALDIquant]{MassPeaks-class}})
#' into different file formats.
#'
#' @details
#' Specific export functions:
#' \tabular{ll}{
#'  tab \tab \code{\link[MALDIquantForeign]{exportTab}} \cr
#'  csv \tab \code{\link[MALDIquantForeign]{exportCsv}} \cr
#'  msd \tab \code{\link[MALDIquantForeign]{exportMsd}} \cr
#' }
#'
#' @param x a \code{\link[MALDIquant]{AbstractMassObject-class}} object or a 
#'  \code{list} of \code{\link[MALDIquant]{AbstractMassObject-class}} objects.
#' @param file \code{character}, file name.
#' @param path \code{character}, path to directory in which the \code{list} of
#'  \code{\link[MALDIquant]{AbstractMassObject-class}} would be exported.
#' @param type \code{character}, file format. If \code{type} is set to 
#'  \dQuote{auto} the file extension is used.
#' @param force \code{logical}, If \code{TRUE} the \code{file} would be
#'  overwritten or \code{path} would be created.
#' @param \ldots arguments to be passed to specific export functions.
#'
#' @seealso
#' \code{\link[MALDIquant]{MassPeaks-class}},
#' \code{\link[MALDIquant]{MassSpectrum-class}} 
#' @author Sebastian Gibb
#' @references \url{http://strimmerlab.org/software/maldiquant/}
#' @examples
#'
#' \dontrun{
#' library("MALDIquant")
#' library("MALDIquantForeign")
#'
#' s <- list(createMassSpectrum(mass=1:5, intensity=1:5),
#'           createMassSpectrum(mass=1:5, intensity=1:5))
#'
#' ## export a single spectrum
#' export(s[[1]], file="spectrum.csv") 
#' ## identical to exportCsv(s[[1]], file="spectrum.csv")
#'
#' ## export a list of spectra
#' export(s, path="spectra", type="csv") 
#' ## identical to exportCsv(s, path="spectra")
#' }
#'
#' @aliases export,AbstractMassObject-method export,list-method
#' @rdname export-methods
#' @export
setMethod(f="export",
  signature=signature(x="AbstractMassObject"),
  definition=function(x, file, type="auto", force=FALSE, ...) {

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

#' @rdname export-methods
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
     
    optArgs <- list(...)
    peaks <- list()

    if (hasArg(peaks)) {
      peaks <- optArgs$peaks 
      optArgs$peaks <- NULL
    }

    for (i in seq(along=x)) {
      arguments <- list(x=x[[i]], file=filenames[i], type=type, force=force)
      arguments <- modifyList(arguments, optArgs)

      if (length(peaks)) {
        arguments$peaks <- peaks[[i]]
      }
      do.call(export, arguments)
    }
  }
  invisible()
})

#' Export to text files
#'
#' This function exports 
#' \code{\link[MALDIquant]{AbstractMassObject-class}} objects (e.g. 
#' \code{\link[MALDIquant]{MassSpectrum-class}},
#' \code{\link[MALDIquant]{MassPeaks-class}})
#' into different text file formats.
#'
#' @details
#' \code{exportTab} and \code{exportCsv} use \code{\link[utils]{write.table}}
#' with different defaults (\code{sep="\t"} in \code{exportTab} and
#' \code{sep=","} in \code{exportCsv}).
#'
#' @param x a \code{\link[MALDIquant]{AbstractMassObject-class}} object or a 
#'  \code{list} of \code{\link[MALDIquant]{AbstractMassObject-class}} objects.
#' @param file \code{character}, file name.
#' @param path \code{character}, path to directory in which the \code{list} of
#'  \code{\link[MALDIquant]{AbstractMassObject-class}} would be exported.
#' @param force \code{logical}, If \code{TRUE} the \code{file} would be
#'  overwritten or \code{path} would be created.
#' @param \ldots arguments to be passed to \code{\link[utils]{write.table}}.
#'
#' @seealso
#' \code{\link[MALDIquant]{MassPeaks-class}},
#' \code{\link[MALDIquant]{MassSpectrum-class}},
#' \code{\link[utils]{write.table}}
#' @author Sebastian Gibb
#' @references \url{http://strimmerlab.org/software/maldiquant/}
#' @examples
#'
#' \dontrun{
#' library("MALDIquant")
#' library("MALDIquantForeign")
#'
#' s <- list(createMassSpectrum(mass=1:5, intensity=1:5),
#'           createMassSpectrum(mass=1:5, intensity=1:5))
#'
#' ## export a single spectrum
#' exportTab(s[[1]], file="spectrum.tab") 
#'
#' ## export a list of spectra and use ; as separator
#' exportCsv(s, path="spectra", sep=";", force=TRUE) 
#' }
#'
#' @aliases exportTab,AbstractMassObject-method exportTab,list-method
#' exportCsv,AbstractMassObject-method exportCsv,list-method
#' @rdname exportTab-methods
#' @export
setMethod(f="exportTab",
          signature=signature(x="AbstractMassObject"),
          definition=function(x, file, force=FALSE, ...) {
  export(x, file=file, type="tab", force=force, ...)
})

#' @rdname exportTab-methods
#' @export
setMethod(f="exportTab",
          signature=signature(x="list"),
          definition=function(x, path, force=FALSE, ...) {
  export(x, path=path, type="tab", force=force, ...)
})

#' @rdname exportTab-methods
#' @export
setMethod(f="exportCsv",
          signature=signature(x="AbstractMassObject"),
          definition=function(x, file, force=FALSE, ...) {
  export(x, file=file, type="csv", force=force, ...)
})

#' @rdname exportTab-methods
#' @export
setMethod(f="exportCsv",
          signature=signature(x="list"),
          definition=function(x, path, force=FALSE, ...) {
  export(x, path=path, type="csv", force=force, ...)
})

#' Export to MSD files
#'
#' This function exports 
#' \code{\link[MALDIquant]{AbstractMassObject-class}} objects (e.g. 
#' \code{\link[MALDIquant]{MassSpectrum-class}},
#' \code{\link[MALDIquant]{MassPeaks-class}})
#' into mMass MSD files.
#'
#' @param x a \code{\link[MALDIquant]{MassSpectrum-class}} object or a 
#'  \code{list} of \code{\link[MALDIquant]{MassSpectrum-class}} objects.
#' @param file \code{character}, file name.
#' @param path \code{character}, path to directory in which the \code{list} of
#'  \code{\link[MALDIquant]{AbstractMassObject-class}} would be exported.
#' @param peaks a \code{\link[MALDIquant]{MassPeaks-class}} object or a 
#'  \code{list} of \code{\link[MALDIquant]{MassPeaks-class}} objects.
#' @param force \code{logical}, If \code{TRUE} the \code{file} would be
#'  overwritten or \code{path} would be created.
#' @param \ldots arguments to be passed to \code{\link[utils]{write.table}}.
#'
#' @seealso
#' \code{\link[MALDIquant]{MassPeaks-class}},
#' \code{\link[MALDIquant]{MassSpectrum-class}}
#'
#' @author Sebastian Gibb
#' @references \url{http://strimmerlab.org/software/maldiquant/}, \cr
#' mMass homepage \url{http://mmass.org/}
#' @examples
#'
#' \dontrun{
#' library("MALDIquant")
#' library("MALDIquantForeign")
#'
#' s <- list(createMassSpectrum(mass=1:5, intensity=1:5),
#'           createMassSpectrum(mass=1:5, intensity=1:5))
#' p <- list(createMassPeaks(mass=4:5, intensity=4:5, snr=1:2),
#'           createMassPeaks(mass=4:5, intensity=4:5, snr=1:2))
#'
#' ## export a single spectrum
#' exportMsd(s[[1]], file="spectrum.msd") 
#'
#' ## export a single spectrum with corresponding peaks
#' exportMsd(s[[1]], file="spectrum.msd", peaks=p[[1]]) 
#'
#' ## export a list of spectra with corresponding peaks
#' exportMsd(s, path="spectra", peaks=p, force=TRUE) 
#' }
#'
#' @aliases exportMsd,MassSpectrum-method exportMsd,list-method
#' @rdname exportMsd-methods
#' @export
setMethod(f="exportMsd",
          signature=signature(x="MassSpectrum"),
          definition=function(x, file, force=FALSE, peaks, ...) {
  stopifnot(isMassSpectrum(x))

  if (!missing(peaks)) {
    stopifnot(isMassPeaks(peaks))
    export(x, file=file, type="msd", force=force, peaks=peaks,  ...)
  } else {
    export(x, file=file, type="msd", force=force, ...)
  }
})

#' @rdname exportMsd-methods
#' @export
setMethod(f="exportMsd",
          signature=signature(x="list"),
          definition=function(x, path, force=FALSE, peaks, ...) {
  stopifnot(isMassSpectrumList(x))

  if (!missing(peaks)) {
    stopifnot(isMassPeaksList(peaks))
    export(x, path=path, type="msd", force=force, peaks=peaks,  ...)
  } else {
    export(x, path=path, type="msd", force=force, ...)
  }
})

