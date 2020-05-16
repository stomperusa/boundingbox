
capturePoints <- function(n, file_path_input, names, resize_x , resize_y, color){

    #PREPARE THE IMAGE
    fname <- names[n]
    img_path <- file.path(file_path_input,fname)
    img <- imager::load.image(img_path)
    size_x <- ifelse(is.na(resize_x), dim(img)[1], resize_x)
    size_y <- ifelse(is.na(resize_y), dim(img)[2], resize_y)
    image <- grDevices::as.raster(imager::resize(img, size_x = size_x, size_y = size_y))

    #CAPTURE  POINTS OVER THE IMAGE
    # show image and capture point
    points <- showPlot(image = image, fname = fname, color = gplots::col2hex(color))
    capture <- list(image = image, points = points, fname  = fname, size_x = size_x, size_y = size_y)
    return(capture)
}
