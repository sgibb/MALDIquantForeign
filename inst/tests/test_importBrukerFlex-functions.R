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

context("importBrukerFlex-functions")

test_that("importBrukerFlex", {
  expect_error(MALDIquantForeign:::.importBrukerFlex("tmp.tmp"))

  s <- MALDIquantForeign:::.importBrukerFlex("data/brukerflex/0_A1/1/1SLin/fid")

  expect_equal(trunc(mass(s[[1]])), 226:230)
  expect_equal(intensity(s[[1]]), 1:5)
  expect_equal(basename(metaData(s[[1]])$file), "fid")
  expect_true(metaData(s[[1]])$laserShots == 100)
  expect_true(all(metaData(s[[1]])$comments == paste("TESTSAMPLE", 1:4, sep="")))
})

