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

#' Supported file formats
#'
#' This function prints all file formats supported by
#' \code{\link{MALDIquantForeign}}.
#'
#' \subsection{Import}{
#'
#' \tabular{ll}{
#'  txt \tab \code{\link[MALDIquantForeign]{importTxt}} \cr
#'  tab \tab \code{\link[MALDIquantForeign]{importTab}} \cr
#'  csv \tab \code{\link[MALDIquantForeign]{importCsv}} \cr
#'  fid \tab \code{\link[MALDIquantForeign]{importBrukerFlex}} \cr
#'  mzXML \tab \code{\link[MALDIquantForeign]{importMzXml}} \cr
#'  mzML \tab \code{\link[MALDIquantForeign]{importMzMl}} \cr
#' }
#' }
#'
#' \subsection{Export}{
#'
#' \tabular{ll}{
#'  tab & \code{\link[MALDIquantForeign]{exportTab}} \\
#'  csv & \code{\link[MALDIquantForeign]{exportCsv}} \\
#'  msd & \code{\link[MALDIquantForeign]{exportMsd}} \\
#' }
#' }
#'
#' @return a \code{list} with two named elements (\code{import} and
#' \code{export}) containing a \code{character} vector of supported file types.
#'
#' @seealso
#'  \code{\link[MALDIquantForeign]{export}},
#'  \code{\link[MALDIquantForeign]{import}}
#' @author Sebastian Gibb
#' @references \url{http://strimmerlab.org/software/maldiquant/}
#' @examples
#' library("MALDIquantForeign")
#' 
#' supportedFileFormats()
#'
#' @rdname supportedFileFormats-functions
#' @export
supportedFileFormats <- function() {
  return(list(import=importFormats$type,
              export=exportFormats$type))
}

#' @keywords internal
importFormats <- data.frame(type=c("txt", "tab", "csv", "fid", "mzxml", "mzml"),
                            pattern=c("^.*\\.txt$", "^.*\\.tab$",
                                      "^.*\\.csv$", "^fid$", 
                                      "^.*\\.mz[Xx][Mm][Ll]$",
                                      "^.*\\.mz[Mm][Ll]$"),
                            handler=c(rep(".import.tab", 2),
                                      ".import.csv", ".import.fid",
                                      ".import.mzxml", ".import.mzml"),
                            stringsAsFactors=FALSE)

#' @keywords internal
exportFormats <- data.frame(type=c("tab", "csv", "msd"),
                            extension=c("tab", "csv", "msd"),
                            handler=c(".export.tab", ".export.csv", 
                                      ".export.msd"),
                            stringsAsFactors=FALSE)

