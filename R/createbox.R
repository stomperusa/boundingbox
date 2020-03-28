
createBox <- function(points_boxed, image, file_path_output, fname, lab, color, size_x, size_y){

    #DRAW THE BOUNDING BOX OVER THE IMAGE AND SAVE AS OUTPUT
    if(tools::file_ext(fname)=="png"){
        grDevices::png(filename= paste0(file_path_output, "out_",fname))
        } else {
        grDevices::jpeg(filename= paste0(file_path_output, "out_",fname))
        }

    graphics::plot(image)
    graphics::lines(c(points_boxed$x_left, points_boxed$x_left),
          c(size_y - points_boxed$y_top, size_y - points_boxed$y_bottom),lwd = 2,  col=color)
    graphics::lines(c(points_boxed$x_right, points_boxed$x_right),
          c(size_y - points_boxed$y_top, size_y - points_boxed$y_bottom),lwd = 2,  col=color)
    graphics::lines(c(points_boxed$x_left, points_boxed$x_right),
          c(size_y - points_boxed$y_top, size_y - points_boxed$y_top),lwd = 2,  col=color)
    graphics::lines(c(points_boxed$x_left, points_boxed$x_right),
          c(size_y - points_boxed$y_bottom, size_y - points_boxed$y_bottom),lwd = 2,  col=color)
    graphics::text(x = (points_boxed$x_left + points_boxed$x_right)/2, y = (size_y - points_boxed$y_top + 5),
         label = lab, col = color)
    grDevices::dev.off()

}
