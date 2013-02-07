context("importTab")

test_that("importTab", {
  ## suppress warnings to avoid creation of Rplots.pdf
  expect_error(suppressWarnings(MALDIquantForeign:::.importTab("tmp.tmp")))

  path <- normalizePath(file.path("data", "ascii.txt"))
  s <- MALDIquantForeign:::.importTab(path)

  expect_equal(s, import(path))
  expect_equal(s, importTxt(path))
  expect_equal(s, import(path, type="txt"))

  expect_equal(mass(s[[1]]), 1:5)
  expect_equal(intensity(s[[1]]), 6:10)
  expect_equal(basename(metaData(s[[1]])$file), "ascii.txt")
})

test_that("importCsv", {
  ## suppress warnings to avoid creation of Rplots.pdf
  expect_error(suppressWarnings(MALDIquantForeign:::.importCsv("tmp.tmp")))

  path <- normalizePath(file.path("data", "csv1.csv"))
  s <- MALDIquantForeign:::.importCsv(path, sep=",", header=TRUE)

  expect_equal(s, import(path, sep=",", header=TRUE))
  expect_equal(s, importCsv(path, sep=",", header=TRUE))
  expect_equal(s, import(path, type="csv", sep=",", header=TRUE))

  expect_equal(mass(s[[1]]), 1:5)
  expect_equal(intensity(s[[1]]), 6:10)
  expect_equal(basename(metaData(s[[1]])$file), "csv1.csv")

  ## auto header
  s <- MALDIquantForeign:::.importCsv(path)

  expect_equal(s, import(path))
  expect_equal(s, importCsv(path))
  expect_equal(s, import(path, type="csv"))

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
