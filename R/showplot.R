# show image and capture point
showPlot <- function(image, fname, color){
    grDevices::x11()
    graphics::plot(image)
    points <- selectBounds(color = color, fname = fname)
    grDevices::dev.off()
    return(points)
}
