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
.list.files <- function(path, pattern, recursive=TRUE, ignore.case=FALSE) {
  return(normalizePath(list.files(path=path, pattern=pattern,
                                  recursive=recursive, ignore.case=ignore.case,
                                  full.names=TRUE)))
}

#' @keywords internal
.files <- function(path, pattern, ...) {
  isDir <- file.info(path)$isdir

  files <- normalizePath(path[!isDir])
  files <- c(files, .list.files(path=path[isDir], pattern=pattern, ...))
  return(files)
}
