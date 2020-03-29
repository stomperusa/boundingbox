#Function to capture selected points
checkBounds <- function(bounds, fname) {
    if (length(bounds$x) < 2) warning(paste0("Bounding box not recorded for ", fname))
    if (length(bounds$x) > 26) warning(paste0("Only first 26 points used for ", fname))
    points <- utils::head(as.data.frame(bounds),26)
    return(points)
}
