context("test-checkbounds")

test_that("points not recorded warning works", {

    fname <- "test_pic"
    bounds <- list(x = 1, y = 1)
    expect_warning(checkBounds(bounds = bounds, fname = fname)
                   , paste0("Bounding box not recorded for ", fname))

})

test_that("more than 26 points warning works", {

    fname <- "test_pic"
    bounds <- list(x = c(1:27), y = c(1:27))
    expect_warning(checkBounds(bounds = bounds, fname = fname)
                   , paste0("Only first 26 points used for ", fname))

})

