## Copyright 2013 Sebastian Gibb
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
.importImzMl <- function(file, verbose=FALSE) {

  if (verbose) {
    message("Reading spectrum from ", sQuote(file), " ...")
  }

  if (!file.exists(file)) {
    stop("File ", sQuote(file), " doesn't exists!")
  }

  ## read file
  s <- .parseMzMl(file=file, verbose=verbose)

  ibdFileName <- paste(.withoutFileExtension(file), ".ibd", sep="")

  ## test SHA-1
  sha1<- digest::digest(ibdFileName, algo="sha1", file=TRUE)

  if (tolower(sha1) != tolower(s$ims$sha1)) {
    stop("SHA1 mismatch!")
  }

  ibd <- file(ibdFileName, open="rb")
  on.exit(close(ibd))

  ## test UUID
  uuid <- paste(readBin(ibd, raw(), n=16, size=1, signed=TRUE, endian="little"),
                collapse="", sep="")

  if (tolower(uuid) != tolower(s$ims$uuid)) {
    stop("UUID mismatch!")
  }

  n <- length(s$ims$ibd)
  spectra <- vector(mode="list", length=n)

  .readValues <- function(file, x, column) {
    n <- x[column, "length"]
    e <- x[column, "encodedLength"]
    return(readBin(file, double(), n=n, size=e/n, signed=TRUE, endian="little"))
  }

  ## read mass and intensity values
  if (s$ims$type == "processed") {
    for (i in seq(along=spectra)) {
      if (verbose) {
        message("Reading binary data for spectrum ", i, "/", n, "...")
      }
      m <- modifyList(s$metaData, s$spectra[[i]]$metaData)
      m$file <- file
      mass <- .readValues(ibd, s$ims$ibd[[i]], "mass")
      intensity <- .readValues(ibd, s$ims$ibd[[i]], "intensity")
      spectra[[i]] <- createMassSpectrum(mass=mass, intensity=intensity, metaData=m)
    }
  } else {
    mass <- .readValues(ibd, s$ims$ibd[[1]], "mass")

    for (i in seq(along=spectra)) {
      if (verbose) {
        message("Reading binary data for spectrum ", i, "/", n, "...")
      }
      m <- modifyList(s$metaData, s$spectra[[i]]$metaData)
      m$file <- file
      intensity <- .readValues(ibd, s$ims$ibd[[i]], "intensity")
      spectra[[i]] <- createMassSpectrum(mass=mass, intensity=intensity, metaData=m)
    }
  }

  return(spectra)
}

