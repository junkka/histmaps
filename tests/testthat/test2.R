context("County map")


test_that("County sp", {
  res1 <- hist_boundaries(1800, "county")
  expect_is(res1, "SpatialPolygonsDataFrame")
  expect_equal(length(res1), 24)

  res2 <- hist_boundaries(1900, "county")
  expect_false(length(res1) == length(res2))
  expect_equal(length(res2), 25)

  expect_error(hist_boundaries(1991, "county"))
  expect_error(hist_boundaries(1400, "county"))

})

test_that("County df", {
  res1 <- hist_boundaries(1800, "county", "df")
  expect_is(res1, "data.frame")
  expect_equal(length(unique(res1$geomid)), 24)

  res2 <- hist_boundaries(1900, "county", "df")
  expect_is(res2, "data.frame")
  expect_equal(length(unique(res2$geomid)), 25)
  expect_false(nrow(res1) == nrow(res2))
  expect_true(ncol(res1) == ncol(res2))
})

test_that("County range map", {
  expect_error(hist_boundaries(c(1800, 1900), "county"))
})