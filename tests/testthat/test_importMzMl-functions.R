context("importMzMl")

test_that("importMzMl", {
  expect_error(MALDIquantForeign:::.importMzMl("tmp.tmp"))

  path <- normalizePath(file.path("data", "tiny1.mzML1.1.mzML"))
  s <- MALDIquantForeign:::.importMzMl(path)

  expect_equal(s, import(path))
  expect_equal(s, importMzMl(path))
  expect_equal(s, import(path, type="mzML"))

  expect_equal(mass(s[[1]]), 1:5)
  expect_equal(intensity(s[[1]]), 6:10)
  expect_equal(basename(metaData(s[[1]])$file), "tiny1.mzML1.1.mzML")

  expect_equal(mass(s[[2]]), 1:5)
  expect_equal(intensity(s[[2]]), 10:6)
  expect_equal(basename(metaData(s[[2]])$file), "tiny1.mzML1.1.mzML")
})

test_that("importMzMl compressed", {
  expect_error(MALDIquantForeign:::.importMzMl("tmp.tmp"))

  path <- normalizePath(file.path("data", "tiny1-compressed.mzML1.1.mzML"))
  s <- MALDIquantForeign:::.importMzMl(path)

  expect_equal(s, import(path))
  expect_equal(s, importMzMl(path))
  expect_equal(s, import(path, type="mzML"))

  expect_equal(mass(s[[1]]), 1:5)
  expect_equal(intensity(s[[1]]), 6:10)
  expect_equal(basename(metaData(s[[1]])$file), "tiny1-compressed.mzML1.1.mzML")

  expect_equal(mass(s[[2]]), 1:5)
  expect_equal(intensity(s[[2]]), 10:6)
  expect_equal(basename(metaData(s[[2]])$file), "tiny1-compressed.mzML1.1.mzML")
})

