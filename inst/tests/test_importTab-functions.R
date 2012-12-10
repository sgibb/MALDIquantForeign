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

test_that("importTab", {
  ## suppress warnings to avoid creation of Rplots.pdf
  expect_error(suppressWarnings(MALDIquantForeign:::.importTab("tmp.tmp")))

  s <- MALDIquantForeign:::.importTab(file.path("data", "ascii.txt"))

  expect_equal(mass(s[[1]]), 1:5)
  expect_equal(intensity(s[[1]]), 6:10)
  expect_equal(basename(metaData(s[[1]])$file), "ascii.txt")
})

test_that("importCsv", {
  ## suppress warnings to avoid creation of Rplots.pdf
  expect_error(suppressWarnings(MALDIquantForeign:::.importCsv("tmp.tmp")))

  s <- MALDIquantForeign:::.importCsv(file.path("data", "csv1.csv"),
                                      sep=",", header=TRUE)

  expect_equal(mass(s[[1]]), 1:5)
  expect_equal(intensity(s[[1]]), 6:10)
  expect_equal(basename(metaData(s[[1]])$file), "csv1.csv")

  ## auto header
  s <- MALDIquantForeign:::.importCsv(file.path("data", "csv1.csv"))

  expect_equal(mass(s[[1]]), 1:5)
  expect_equal(intensity(s[[1]]), 6:10)
  expect_equal(basename(metaData(s[[1]])$file), "csv1.csv")

  s <- MALDIquantForeign:::.importCsv(file.path("data", "csv2.csv"),
                                      sep=";", header=FALSE)

  expect_equal(mass(s[[1]]), 1:5)
  expect_equal(intensity(s[[1]]), 6:10)
  expect_equal(basename(metaData(s[[1]])$file), "csv2.csv")

  ## auto header
  s <- MALDIquantForeign:::.importCsv(file.path("data", "csv2.csv"), sep=";")

  expect_equal(mass(s[[1]]), 1:5)
  expect_equal(intensity(s[[1]]), 6:10)
  expect_equal(basename(metaData(s[[1]])$file), "csv2.csv")
})

x <- c("10, 30", "\"foo\", \"bar\"", "foo; bar", "foo\tbar", "1\t 2")
sep <- c(",", ",", ";", "\t", "\t")

test_that("autoHeader", {
  result <- c(FALSE, TRUE, TRUE, TRUE, FALSE)

  for (i in seq(along=x)) {
    expect_identical(MALDIquantForeign:::.autoHeader(textConnection(x[i]),
                                                     sep=sep[i]), result[i])
  }
})

test_that("autoSep", {
  for (i in seq(along=x)) {
    expect_identical(MALDIquantForeign:::.autoSep(textConnection(x[i])), sep[i])
  }
})
