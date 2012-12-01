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
.importAuto <- function(path, verbose=FALSE, ...) {
  pattern <- unique(importFormats$pattern)

  files <- lapply(pattern, .files, path=path) 
  n <- lapply(files, length)
  m <- which.max(n)[1]

  return(import(path=files[[m]], type=tolower(.fileExtension(files[[m]][1])),
         pattern=pattern[m], ...))
}

#' @keywords internal
.import.auto <- function(...) {
  return(.importAuto(...))
}

