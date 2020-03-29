# show image and capture point
showPlot <- function(image, fname, color){
    grDevices::x11()
    graphics::plot(image)
    bounds <- graphics::locator(type = "p", col = color)
    points <- checkBounds(bounds = bounds, fname = fname)
    grDevices::dev.off()
    return(points)
}
