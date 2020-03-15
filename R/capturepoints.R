
capturePoints <- function(n, file_path_input, names, size_x , size_y, color){

    #PREPARE THE IMAGE
    fname <- names[n]
    img_path <- paste0(file_path_input,fname)
    img <- imager::load.image(img_path)
    size_x <- ifelse(is.na(size_x), dim(img)[1], size_x)
    size_y <- ifelse(is.na(size_y), dim(img)[1], size_y)
    image <- grDevices::as.raster(imager::resize(img, size_x = size_x, size_y = size_y))

    #CAPTURE  POINTS OVER THE IMAGE
    # show image and capture point
    points <- showPlot(image = image, fname = fname, color = color)
    capture <- list(image = image, points = points, fname  = fname, size_x = size_x, size_y = size_y)
    return(capture)
}
