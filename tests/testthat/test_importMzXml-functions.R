context("importMzXml")

test_that("importMzXml", {
  expect_error(MALDIquantForeign:::.importMzXml("tmp.tmp"))

  path <- file.path("data", "tiny1.mzXML3.0.mzXML")
  s <- MALDIquantForeign:::.importMzXml(path)

  expect_equal(s, import(path))
  expect_equal(s, importMzXml(path))
  expect_equal(s, import(path, type="mzXML"))

  expect_equal(mass(s[[1]]), 1:5)
  expect_equal(intensity(s[[1]]), 6:10)
  expect_equal(basename(metaData(s[[1]])$file), "tiny1.mzXML3.0.mzXML")
})

test_that("importMzXml compressed", {
  expect_error(MALDIquantForeign:::.importMzXml("tmp.tmp"))

  path <- file.path("data", "tiny1-compressed.mzXML3.0.mzXML")
  s <- MALDIquantForeign:::.importMzXml(path)

  expect_equal(s, import(path))
  expect_equal(s, importMzXml(path))
  expect_equal(s, import(path, type="mzXML"))

  expect_equal(mass(s[[1]]), 1:5)
  expect_equal(intensity(s[[1]]), 6:10)
  expect_equal(basename(metaData(s[[1]])$file),
               "tiny1-compressed.mzXML3.0.mzXML")
})

