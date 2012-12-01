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

context("fileExtension-functions")

test_that("file extension is returned", {
  expect_true(MALDIquantForeign:::.fileExtension("~/foo.txt") == "txt")
  expect_true(all(
    MALDIquantForeign:::.fileExtension(c("/etc/a.conf", "b.pdf")) ==
    c("conf", "pdf")))
})

test_that("file name is returned", {
  expect_true(MALDIquantForeign:::.fileExtension("~/foo") == "foo")
})
