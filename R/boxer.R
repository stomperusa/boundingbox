boxer <- function(names, file_path_input, file_path_output, color = "red",
                  size_x = NA, size_y = NA, classifier = NA, show_classifier = F, ...){

    #Empty data frame used to capture bounding box coorindates
    points_master <- data.frame()
    lab <- ifelse(show_classifier == TRUE, classifier, "")

     #Cycle through each of the named image files
    for(n in 1:length(names)){
        image_points <- list()

        capture <- capturePoints(n = n, file_path_input, names = names, size_x, size_y, color = color)
        image <- capture$image
        fname <- capture$fname
        size_x <- capture$size_x
        size_y <- capture$size_y
        points <- capture$points
        point_count <- nrow(points)
        if (point_count < 2) {next()}

        # capture selected points in the image point list
        points$rows <- letters[1:point_count]
        points_reduced <- list()

        for (let in points$rows){
            for (var in c("x", "y")) {
                df <- data.frame(points[points$rows == let, var])
                colnames(df) <- paste(let, var, sep = "_")
                points_reduced[paste(let, var, sep = "_") ] <- df
            }
        }

        points <- as.data.frame(points_reduced)
        points$file_name<- fname
        points_process <- points[,c(ncol(points),1:(ncol(points)-1))]

        points_boxed <-createBox(points_process = points_process, image = image, file_path_output =file_path_output,
                  fname = fname, lab = lab, color = color, point_count = point_count, size_x = size_x,
                  size_y = size_y)

        #UPDATE DATA FRAME WITH THE BOUNDING BOX COORDINATES FOR EACH IMAGE
        points_master <- rbind(points_master, points_boxed)
    }
    points_master$classifier <- classifier
    return(points_master)
}
