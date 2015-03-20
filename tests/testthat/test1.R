context("parish map")

test_that("get parish sp", {
  res <- hist_boundaries(1800, "par", "sp")
  expect_is(res, "SpatialPolygonsDataFrame")
  expect_equal(length(res), 2431)

  res <- hist_boundaries(1900, "par", "sp")
  expect_equal(length(res), 2493)

  expect_error(hist_boundaries(2000))
  expect_error(hist_boundaries(1400))
})

test_that("get parish df", {
  res <- hist_boundaries(1800, "par", "df")
  expect_is(res, "data.frame")
  expect_equal(length(unique(res$geomid)), 2431)
  expect_equal(colnames(res)[1:2], c("long", "lat"))

  res <- hist_boundaries(1900, "par", "df")
  expect_is(res, "data.frame")
  expect_equal(length(unique(res$geomid)), 2493)
  
})

test_that("get parish meta", {
  res <- hist_boundaries(1800, "par", "meta")
  expect_is(res, "data.frame")
  expect_equal(length(res$geomid), 2431)
  expect_false(all(colnames(res)[1:2] == c("long", "lat")))

  res <- hist_boundaries(1900, "par", "meta")
  expect_is(res, "data.frame")
  expect_equal(length(res$geomid), 2493)
})


test_that("get parish period", {

  expect_error(hist_boundaries(c(1900, 1890)))

  res <- hist_boundaries(c(1800, 1900))
  
  expect_is(res, "list")
  expect_is(res[[1]], "SpatialPolygonsDataFrame")
  expect_is(res[[2]], "data.frame")

  expect_equal(length(res$map), length(unique(res$lookup$geomid)))
  expect_true(all(res$map@data$geomid %in% unique(res$lookup$geomid)))
  expect_true(all(unique(res$lookup$geomid) %in% res$map@data$geomid))
})