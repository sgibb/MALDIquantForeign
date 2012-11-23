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

context("importTab-functions")

test_that("import.tab", {
  expect_warning(expect_error(MALDIquantForeign:::.import.tab("tmp.tmp")),
                 "cannot open file")

  s <- MALDIquantForeign:::.import.tab("data/ascii.txt")

  expect_equal(mass(s[[1]]), 1:5)
  expect_equal(intensity(s[[1]]), 6:10)
  expect_equal(basename(metaData(s[[1]])$file), "ascii.txt")
})

test_that("import.csv", {
  expect_warning(expect_error(MALDIquantForeign:::.import.csv("tmp.tmp")),
                 "cannot open file")

  s <- MALDIquantForeign:::.import.csv("data/csv1.csv", sep=",", header=TRUE)

  expect_equal(mass(s[[1]]), 1:5)
  expect_equal(intensity(s[[1]]), 6:10)
  expect_equal(basename(metaData(s[[1]])$file), "csv1.csv")

  ## auto header
  s <- MALDIquantForeign:::.import.csv("data/csv1.csv")

  expect_equal(mass(s[[1]]), 1:5)
  expect_equal(intensity(s[[1]]), 6:10)
  expect_equal(basename(metaData(s[[1]])$file), "csv1.csv")

  s <- MALDIquantForeign:::.import.csv("data/csv2.csv", sep=";", header=FALSE)

  expect_equal(mass(s[[1]]), 1:5)
  expect_equal(intensity(s[[1]]), 6:10)
  expect_equal(basename(metaData(s[[1]])$file), "csv2.csv")

  ## auto header
  s <- MALDIquantForeign:::.import.csv("data/csv2.csv", sep=";")

  expect_equal(mass(s[[1]]), 1:5)
  expect_equal(intensity(s[[1]]), 6:10)
  expect_equal(basename(metaData(s[[1]])$file), "csv2.csv")
})

test_that("autoHeader", {
  s <- "10, 30"
  expect_false(MALDIquantForeign:::.autoHeader(textConnection(s), sep=","))
  s <- "\"foo\", \"bar\""
  expect_true(MALDIquantForeign:::.autoHeader(textConnection(s), sep=","))
  s <- "foo; bar"
  expect_true(MALDIquantForeign:::.autoHeader(textConnection(s), sep=";"))
})
