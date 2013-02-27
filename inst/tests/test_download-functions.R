context("download")

url <- c("http://www.tld.com/", "https://www.tld.com/archive.zip",
         "ftp://ftp.tld.com", "/data/archive.zip", "/root", "/dev/data.csv")

test_that(".isUrl", {
  expect_identical(MALDIquantForeign:::.isUrl(url),
                   c(rep(TRUE, 3), rep(FALSE, 3)))
})

