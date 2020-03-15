
createBox <- function(points_process, image, file_path_output, fname, lab, color, size_x, size_y,
                      point_count){
    #ADJUST THE CAPTURED POINTS AND FIND THE COORDINATES OF THE BOUNDING BOX
    # Find any points outside of range 0 to x_size or y_size and adjust to be inbounds
    points_process[-1][points_process[-1] <= 0] <- 1

    points_process[substr(colnames(points_process), 2,3) == "_x" & points_process >= size_x] <- (size_x - 1)
    points_process[substr(colnames(points_process), 2,3) == "_y" & points_process >= size_y] <- (size_y - 1)

    # flip the y coordinates as when working with images the y origin is at the top
    points_process[, substr(colnames(points_process), 2,3) == "_y"] <-
        size_y - points_process[, substr(colnames(points_process), 2,3) == "_y"]


    # round the values and convert to integers
    points_process[-1] <- sapply(points_process[-1],function(x) as.integer(round(x)) )

    #order is [x_left, y_top, x_right, y_bottom]
    x_columns <- seq.int(from=2, by=2, length.out = point_count)
    y_columns <- x_columns + 1

    points_process$x_left <- apply(points_process[ , x_columns], 1, min)
    points_process$y_top <-apply(points_process[ , y_columns], 1, min)
    points_process$x_right <- apply(points_process[ , x_columns], 1, max)
    points_process$y_bottom <- apply(points_process[ , y_columns], 1, max)
    points_boxed <- points_process[,c("file_name", "x_left", "y_top", "x_right", "y_bottom")]

    #DRAW THE BOUNDING BOX OVER THE IMAGE AND SAVE AS OUTPUT
    grDevices::jpeg(filename= paste0(file_path_output, "out_",fname))
    graphics::plot(image)
    graphics::lines(c(points_process$x_left, points_process$x_left),
          c(size_y - points_process$y_top, size_y - points_process$y_bottom),lwd = 2,  col=color)
    graphics::lines(c(points_process$x_right, points_process$x_right),
          c(size_y - points_process$y_top, size_y - points_process$y_bottom),lwd = 2,  col=color)
    graphics::lines(c(points_process$x_left, points_process$x_right),
          c(size_y - points_process$y_top, size_y - points_process$y_top),lwd = 2,  col=color)
    graphics::lines(c(points_process$x_left, points_process$x_right),
          c(size_y - points_process$y_bottom, size_y - points_process$y_bottom),lwd = 2,  col=color)
    graphics::text(x = (points_process$x_left + points_process$x_right)/2, y = (size_y - points_process$y_top + 5),
         label = lab, col = color)
    grDevices::dev.off()

    return(points_boxed)
}
