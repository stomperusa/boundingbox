recaptureImage <- function(file_path_input, fname, size_x, size_y){

    #PREPARE THE IMAGE
    #img_path <- paste0(file_path_input,"out_", fname)
    img_path <- paste0(file_path_input, fname)
    img <- imager::load.image(img_path)
    image <- grDevices::as.raster(imager::resize(img, size_x = size_x, size_y = size_y))
    return(image)
}
