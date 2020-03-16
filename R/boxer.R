#' Create a bounding box in an image based on selected points
#'
#' \code{boxer} allows the user to identify an area in an image around which to
#' generate a bounding box.
#'
#' Stream a series of JPEG or PNG images from a directory that each appears on
#' the screen, and indicate between 2 and 26 points that define an area around
#' which a bounding box is generated based on the max/min x and y values of the
#' selected coordinates. There are two outputs. One is a dataframe with the name
#' of the original image, the coordinates of the bounding box ("x_left",
#' "y_top", "x_right", "y_bottom") and the classifier. Another is, for each
#' image, an image file "out_<original_file_name>" annotated with the bounding
#' box, the classifier printed above the bounding box if show_classifer is set
#' to TRUE, and resized if either the size_x or size_y parameter is given.
#'
#'
#'
#' @param names A list that contains the names of the image files to cycle
#'   through.
#' @param file_path_input The directory that contains the image files in JPEG
#'   format.
#' @param file_path_output The directory to which the annotated images should be
#'   saved.
#' @param color The color of the bounding box. Default is "red".
#' @param size_x Number of columns to resize the x-axis. Default is NA and will
#'   return size of original image.
#' @param size_y Number of columns to resize the y-axis. Default is NA and will
#'   return size of original image.
#' @param classifier Character string to add a classifier along with the output
#'   coordinates. Default is NA.
#' @param show_classifier Logical to indicate to print the classifier along with
#'   the bounding box. Default is F.
#'
#' @return A dataframe with the bounding box coordinates for each image, and a
#'   new image file with the bounding box annotation for each of the images
#'   processed.
#'
#' @examples
#' \dontrun{
#' boxer(names = c("dog.275.jpg", "dog.544.jpg", "dog.119.jpg"),
#' file_path_input = "/dog_pics/input/", file_path_output = "/dog_pics/output/",
#' color = "blue", size_x = 224, size_y = 224, classifier = "dog",
#' show_classifier = T)
#' }
#'
#'@export
boxer <- function(names, file_path_input, file_path_output, color = "red",
                  size_x = NA, size_y = NA, classifier = NA, show_classifier = F){

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
