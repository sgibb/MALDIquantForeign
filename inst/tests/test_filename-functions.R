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

context("filename-functions")

test_that(".cleanFilename", {
  expect_equal(MALDIquantForeign:::.cleanFilename(
                "/home/a:/\"foo&bar\"/g.\\23!/ foo-bar?.txt"),
               "_home_a_foo_bar_g_23_foo_bar_txt")
})

test_that("file extension is returned", {
  expect_true(MALDIquantForeign:::.fileExtension("~/foo.txt") == "txt")
  expect_true(all(
    MALDIquantForeign:::.fileExtension(c("/etc/a.conf", "b.pdf")) ==
    c("conf", "pdf")))
})

test_that("file name is returned", {
  expect_true(MALDIquantForeign:::.fileExtension("~/foo") == "foo")
})

test_that("path without extension is returned", {
  s <- c("~/foo", "/home/user/xyz.tar.gz", "/tmp/bar.txt")
  r <- c("~", "/home/user/xyz", "/tmp/bar")
  expect_true(all(MALDIquantForeign:::.withoutFileExtension(s) == r))
})

test_that(".cutFilenames", {
  s <- c("/home/user/foo.bar", "/home/user/xyz.tar.gz")
  r <- c("foo.bar", "xyz.tar.gz")
  expect_true(all(MALDIquantForeign:::.cutFilenames(s) == r))

  s <- c("/home/user/foo.bar", "/home/user/foo.bar")
  r <- c("foo.bar", "foo.bar")
  expect_true(all(MALDIquantForeign:::.cutFilenames(s) == r))
})
