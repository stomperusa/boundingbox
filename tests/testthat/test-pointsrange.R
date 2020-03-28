context("test-pointsrange")

test_that("selected points adjusted to be in bounds of image", {
    points_process <- data.frame(
        file_name = as.character("name"),
        a_x = -1,
        a_y = 230,
        b_x = 230,
        b_y = -1,
        c_x = 230,
        c_y = 230,
        d_x = -1,
        d_y = -1
    )

    size_x <- size_y  <- 224

    box <- boxPoints(points_process = points_process, size_x = size_x, size_y = size_y, point_count = 4)

    expect_true(all(
        box$x_left > 0,
        box$y_top > 0,
        box$x_right < size_x,
        box$y_bottom < size_y))
})
