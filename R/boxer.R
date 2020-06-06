#'Create a bounding box in an image based on selected points
#'
#'\code{boxer} allows the user to identify an area in an image around which to
#'generate a bounding box and set a common classifier for all images.
#'
#'Stream a series of images (JPEG, PNG, BMP) from a directory. As each image
#'appears, indicate between 2 and 26 points around which boxer will calculate a
#'bounding box. The name of the original file, the bounding box coordinates, and
#'optional image resize values, classifier and box color are returned in a
#'dataframe. Boxer also offers the option to call the outBox function that
#'will output each image with its bounding box. If outputting the images, the
#'show_classifier parameter controls whether or not to show the class as a label
#'above the bounding box.
#'
#'When an image appears, use the left mouse button to select the series of
#'points, and the right mouse button to signal completion and to move to the
#'next image. At least two points must be selected to record a bounding box. If
#'more than 26 points are selected, only the first 26 will be used. To skip
#'creating a bounding box for the current image, select the right mouse button
#'without selecting any points. Messages noting the images skipped will appear
#'at the end.
#'
#'The dataframe of bounding box coordinates will only be returned when all of
#'the images have been cycled through, unless you set the batch parameter.  The
#'batch parameter allows for the images to be processed in batches with the
#'option to terminate and generate the output file at the end of each batch. The
#'batch parameter can be set for the number of images to cycle through before
#'the user is prompted to truncate the stream. Selecting "y" at the prompt will
#'truncate the stream and return the bounding box coordinates for all of the
#'images up to that point. Selecting any other key will continue to stream
#'through the next batch of images.
#'
#'If using the classifier and color parameters, boxer assumes all of the images
#'are classified the same. To be able to set the classifier per bounding box, or
#'to be able to generate multiple bounding boxes per image, use the boxer2
#'function.
#'
#'@param names A vector that contains the names of JPG, PNG, or BMP image files.
#'@param file_path_input The directory that contains the image files.
#'@param color The name of the color for selected points  and the bounding box.
#'  Default is "red".
#'@param resize_x Number of pixels to resize the x-axis. Default is NA to keep
#'  original size.
#'@param resize_y Number of pixels to resize the y-axis. Default is NA to keep
#'  original size.
#'@param classifier Character string to add a classifier. Default is NA.
#'@param batch Number of images before prompt to truncate stream. Default is
#'  length(names).
#'@param outbox Logical to run outBox and output images. Default is F.
#'@param file_path_output The directory where to save annotated images if
#'  outbox is T.
#'@param show_classifier Logical to include the classifier above the bounding
#'  box if outbox is T. Default is F.
#'
#'@return A dataframe with the bounding box coordinates and classifier for each
#'  image. Note the y-coordinate extends downward, not upward. Will also output
#'  new image file with the bounding box annotation for each of the images
#'  processed if outbox is set to T. The name of each output image file will be
#'  the same as the corresponding input file prefixed with "out_".
#'
#' @examples
#' \dontrun{
#' boxer_results <- boxer(names = c("S2.jpg"),
#' file_path_input = system.file("extdata", package = "boundingbox"),
#' resize_x = 224, resize_y = 224, classifier = "dog", outbox = FALSE)
#'}
#'
#'@export
boxer <- function(names, file_path_input, color = "red", resize_x = NA, resize_y = NA,
                  classifier = NA,  batch = length(names),
                  outbox = FALSE, file_path_output = NA, show_classifier = FALSE) {

    # run checks to validate parameters
    checks <- c(1:5)
    paramCheck(names = names, file_path_input = file_path_input, color = color,
               outbox = outbox, file_path_output = file_path_output, checks = checks)


    # empty data frame used to capture bounding box coorindates
    points_master <- data.frame()


     # cycle through each of the named image files
    for(n in 1:length(names)){

        # offer User to breakout out of the loop
        if((n - 1 != 0) & ((n - 1) %% batch == 0)){
            response <- as.numeric(readline(prompt = "Enter 1 if you want to truncate the stream or any other key to continue."))
            if(response == 1){
                break
            }
        }


        image_points <- list()

        capture <- capturePoints(n = n, file_path_input, names = names, resize_x = resize_x, resize_y = resize_y, color = color)
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

        # generate box coordinates
        points_boxed <-boxPoints(points_process = points_process, point_count = point_count, size_x = size_x,
                  size_y = size_y)


        # update dataframe with image coordinates
        points_master <- rbind(points_master, points_boxed)


    }
    points_master$classifier <- as.character(classifier)
    points_master$color <- color

    # generate output images
    if(outbox == TRUE) {outBox(points_master = points_master, file_path_input = file_path_input,
                                file_path_output = file_path_output, show_classifier = show_classifier)}

    return(points_master)
}
