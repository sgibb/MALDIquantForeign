context("fileFormats")

test_that("supportedFileFormats", {
  r <- list(import = c("txt", "tab", "csv",
                       "fid",
                       "ciphergen",
                       "mzxml", "mzml", "imzml",
                       "analyze",
                       "cdf"),
            export = c("tab", "csv",
                       "msd",
                       "mzml"))
  expect_identical(supportedFileFormats(), r)
})
