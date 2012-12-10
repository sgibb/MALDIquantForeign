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

context("exportTab-functions")

m <- createMassSpectrum(mass=1:5, intensity=6:10)
r <- data.frame(V1=1:5, V2=6:10)

test_that("exportTab", {
  temp <- tempfile()
  MALDIquantForeign:::.exportTab(m, file=temp)
  ## didn't work on win-builder.r-project.org
  ## (but on local linux and windows install (both R 2.15.2)
  #expect_equivalent(tools::md5sum(temp), tools::md5sum(file.path("data", 
  #                                                               "ascii.txt")))
  expect_equal(read.table(temp), r)
})

test_that("exportCsv", {
  temp <- tempfile()
  MALDIquantForeign:::.exportCsv(m, file=temp)
  ## didn't work on win-builder.r-project.org
  ## (but on local linux and windows install (both R 2.15.2)
  #expect_equivalent(tools::md5sum(temp), tools::md5sum(file.path("data",
  #                                                               "csv1.csv")))
  colnames(r) <- c("mass", "intensity")
  expect_equal(read.csv(temp), r)
})

