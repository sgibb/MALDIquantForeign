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
.isUrl <- function(x) {
  return(grepl(pattern="^https?://|^ftp://", x=x))
}

#' @keywords internal
.download <- function(url, destfile, verbose=FALSE, ...) {
  if (missing(destfile)) {
    pattern <- paste0(.withoutFileExtension(basename(url)), "_")
    fileext <- paste0(".", .fileExtension(url))
    tmpdir <- file.path(tempdir(), "MALDIquantForeign_download")

    if (!file.exists(tmpdir)) {
      dir.create(tmpdir, showWarnings=FALSE, recursive=TRUE)
    }

    destfile <- file.path(tmpdir, tempfile(pattern=pattern, tmpdir="",
                                           fileext=fileext))
  }

  if (verbose) {
    message("Downloading ", url , " to ", destfile, ".")
  }

  for (i in seq(along=url)) {
    r <- downloader::download(url=url[i], destfile=destfile[i], quiet=!verbose,
                              mode="wb", ...)
    if (r != 0) {
      warning("Download of ", url[i], " failed!")
    }
  }

  return(destfile)
}

#' @keywords internal
.cleanupDownloadedTmpFiles <- function() {
  unlink(file.path(tempdir(), "MALDIquantForeign_download"), recursive=TRUE)
}

