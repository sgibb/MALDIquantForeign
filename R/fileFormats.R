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
importFormats <- data.frame(type=c("ascii", "txt", "text", "tab",
                                   "csv", "fid", "mzxml", "mzml"),
                            pattern=c(rep("^.*\\.txt$", 3), "^.*\\.tab$",
                                      "^.*\\.csv$", "^fid$", 
                                      "^.*\\.mz[Xx][Mm][Ll]$",
                                      "^.*\\.mz[Mm][Ll]$"),
                            handler=c(rep(".import.tab", 4),
                                      ".import.csv", ".import.fid",
                                      ".import.mzxml", ".import.mzml"),
                            stringsAsFactors=FALSE)

#' @keywords internal
exportFormats <- data.frame(type=c("tab", "csv", "msd"),
                            extension=c("tab", "csv", "msd"),
                            handler=c(".export.tab", ".export.csv", 
                                      ".export.msd"),
                            stringsAsFactors=FALSE)

#' @export
supportedFormats <- function() {
  return(list(import=importFormats$type,
              export=exportFormats$type))
}

