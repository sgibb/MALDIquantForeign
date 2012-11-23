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

test_that("export.tab", {
  temp <- tempfile()
  MALDIquantForeign:::.export.tab(m, file=temp)
  expect_equivalent(tools::md5sum(temp), tools::md5sum("data/ascii.txt"))
})

test_that("export.csv", {
  temp <- tempfile()
  MALDIquantForeign:::.export.csv(m, file=temp)
  expect_equivalent(tools::md5sum(temp), tools::md5sum("data/csv1.csv"))
})

