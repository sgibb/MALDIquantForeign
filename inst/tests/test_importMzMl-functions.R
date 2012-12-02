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

context("importMzMl-functions")

test_that("importMzMl", {
  expect_error(MALDIquantForeign:::.importMzMl("tmp.tmp"))

  s <- MALDIquantForeign:::.importMzMl("data/tiny1.mzML1.1.mzML")

  expect_equal(mass(s[[1]]), 1:5)
  expect_equal(intensity(s[[1]]), 6:10)
  expect_equal(basename(metaData(s[[1]])$file), "tiny1.mzML1.1.mzML")

  expect_equal(mass(s[[2]]), 1:5)
  expect_equal(intensity(s[[2]]), 10:6)
  expect_equal(basename(metaData(s[[2]])$file), "tiny1.mzML1.1.mzML")
})

test_that("importMzMl compressed", {
  expect_error(MALDIquantForeign:::.importMzMl("tmp.tmp"))

  s <- MALDIquantForeign:::.importMzMl("data/tiny1-compressed.mzML1.1.mzML")

  expect_equal(mass(s[[1]]), 1:5)
  expect_equal(intensity(s[[1]]), 6:10)
  expect_equal(basename(metaData(s[[1]])$file), "tiny1-compressed.mzML1.1.mzML")

  expect_equal(mass(s[[2]]), 1:5)
  expect_equal(intensity(s[[2]]), 10:6)
  expect_equal(basename(metaData(s[[2]])$file), "tiny1-compressed.mzML1.1.mzML")
})

