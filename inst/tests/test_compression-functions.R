context("compression")

z <- c("bz2", "bzip2", "gz", "lzma", "xz")
e <- c("zip", z, paste("tar", z, sep="."), "tar", "txt", "mzML")
f <- paste(letters[1:13], e, sep=".")

test_that(".isCompressed", {
  expect_identical(MALDIquantForeign:::.isCompressed(f),
                   c(rep(TRUE, 11), rep(FALSE, 3)))
})

test_that(".isTar", {
  expect_identical(MALDIquantForeign:::.isTar(f),
                   c(rep(FALSE, 6), rep(TRUE, 6), rep(FALSE, 2)))
})

test_that(".isZip", {
  expect_identical(MALDIquantForeign:::.isZip(f), c(TRUE, rep(FALSE, 13)))
})

test_that(".isPackedOrCompressed", {
  expect_identical(MALDIquantForeign:::.isPackedOrCompressed(f),
                   c(rep(TRUE, 12), rep(FALSE, 2)))
})

test_that(".uncompress supports single file compression by gunzip", {
  u <- MALDIquantForeign:::.uncompress(file.path("data", "compressed",
                                                 "csv1.csv.gz"))
  f <- file.path("data", "csv1.csv")
  expect_identical(readLines(u), readLines(f))
})

test_that(".uncompress supports tar compression by untar", {
  u <- list.files(MALDIquantForeign:::.uncompress(
                    file.path("data", "compressed", "csv.tar.bz2")),
                  recursive=TRUE, pattern="^.*\\.csv$", full.names=TRUE)[1]
  f <- file.path("data", "csv1.csv")
  expect_identical(readLines(u), readLines(f))
})

test_that(".uncompress supports zip compression by unzip", {
  u <- list.files(MALDIquantForeign:::.uncompress(
                    file.path("data", "compressed", "csv.zip")),
                  recursive=TRUE, pattern="^.*\\.csv$", full.names=TRUE)[1]
  f <- file.path("data", "csv1.csv")
  expect_identical(readLines(u), readLines(f))
})

test_that(".cleanupUncompressingTmFiles works", {
  n <- list.files(file.path(tempdir(), "MALDIquantForeign_uncompress"),
                  recursive=TRUE)
  expect_true(length(n) > 0)
  MALDIquantForeign:::.cleanupUncompressingTmpFiles()
  expect_false(file.exists(file.path(tempdir(),
                                     "MALDIquantForeign_uncompress")))
})

test_that("typical auto import", {
  f <- normalizePath(file.path("data", "compressed", "csv1.csv.gz"))
  s <- createMassSpectrum(mass=1:5, intensity=6:10)
  i <- import(f)[[1]]
  metaData(i) <- list()
  expect_identical(i, s)
  expect_false(file.exists(file.path(tempdir(),
                                     "MALDIquantForeign_uncompress")))
})

