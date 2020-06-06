#'Create multiple bounding boxes in an image based on sets of selected points
#'
#'\code{boxer2} allows the user to identify multiple areas in an image around
#'which to generate bounding boxes and/or the ability to set a classifier per
#'bounding box.
#'
#'Input a dataframe that captures reference information regarding the
#'classifiers. This includes a numeric reference, class (or name), and color.
#'Then stream a series of images (JPEG, PNG, BMP) from a directory. As each
#'image appears, indicate between 2 and 26 points around which boxer2 will
#'calculate a bounding box. Follow the prompt to provide the numeric reference
#'for the classifier. The next prompt allows to add another bounding box to the
#'same image or to advance to the next image.
#'
#'The name of the original file, bounding box coordinates, classifiers, box
#'colors, and optional image resize values are returned in a dataframe. Boxer2
#'also offers the option to call the outBox function that will output each
#'image with its bounding box(es). If outputting the images, the show_classifier
#'parameter controls whether or not to show the class as a label above the
#'bounding box.
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
#'If all of the images have the same classifier and only one bounding box is
#'required per image, consider using the boxer function instead.
#'
#'
#'@param names A vector that contains the names of JPG, PNG, or BMP image files.
#'@param file_path_input The directory that contains the image files.
#'@param color The name of the color for selected points.Default is "red".
#'@param resize_x Number of pixels to resize the x-axis. Default is NA to keep
#'  original size.
#'@param resize_y Number of pixels to resize the y-axis. Default is NA to keep
#'  original size.
#'@param classifier A data frame with columns "ref" (int), "class" (chr),
#'  and "color" (chr). The "ref" value must be unique. "class" will appear as
#'  the label, and "color" will be used as the color of the bounding box.
#'@param batch Number of images before prompt to truncate stream. Default is
#'  length(names).
#'@param outbox Logical to run outBox and output images. Default is F.
#'@param file_path_output The directory where to save annotated images if
#'  outbox is T.
#'@param show_classifier Logical to include the classifier above the bounding
#'  box if outbox is T. Default is F.
#'
#'@return A dataframe with the bounding box coordinates for each image, and a
#'  new image file with the bounding box annotation for each of the images
#'  processed. Note the y-coordinate extends downward, not upward.The name of
#'  each output image file will be the same as the corresponding input file
#'  prefixed with "out_".
#'
#' @examples
#' # A sample classifier dataframe
#' dog_df <- data.frame(read.csv(system.file("extdata","dog_df.csv",
#' package = "boundingbox")), stringsAsFactors = FALSE)
#'
#' # Use 1 to classify the first dog and 2 to classify the second.
#'\dontrun{
#' boxer2_results <- boxer2(names = c("SW1.jpg"),
#' file_path_input = system.file("extdata", package = "boundingbox"),
#' resize_x = 224, resize_y = 224, classifier = dog_df, outbox = FALSE)
#'}
#'
#'@export
boxer2 <- function(names, file_path_input, color = "red", resize_x = NA, resize_y = NA,
                  classifier, batch = length(names),
                  outbox = FALSE, file_path_output = NA, show_classifier = FALSE) {



    # run checks to validate parameters
    checks <- c(1:11)
    paramCheck(names = names, file_path_input = file_path_input, color = color,
               outbox = outbox, file_path_output = file_path_output, checks = checks,
               classifier = classifier)



    #Empty data frame used to capture bounding box coorindates
    points_master <- data.frame()


    #Cycle through each of the named image files
    for(n in 1:length(names)){

        # offer User to breakout out of the loop
        if((n - 1 != 0) & ((n - 1) %% batch == 0)){
            response <- as.numeric(readline(prompt = "Enter 1 if you want to truncate the stream or any other key to continue."))
            if(response == 1){
                break
            }
        }

        cycle <- TRUE
        box_count <- 0

        while (cycle == TRUE){

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


        class_check <- FALSE

        while(class_check == FALSE){

        class <- as.numeric(readline(prompt = "Enter ref# for the classifier of this bounding box "))
        if(!class %in% unique(as.numeric(classifier$ref))) {
                message("You did not select a valid value for classifier. Please try again.")
        } else {
                class_check <- TRUE
            }
        }

        points_boxed$classifier <- classifier$class[classifier$ref == class]
        points_boxed$color <- classifier$color[classifier$ref == class]



        # Update dataframe with image coordinates
        points_master <- rbind(points_master, points_boxed)

        image <- if(box_count > 0){
                    recaptureImage(file_path_input = file_path_input, fname = fname, size_x = size_x, size_y = size_y)
                } else {
                    image
                }

        box_count <- box_count + 1

        cycle <- as.numeric(readline(prompt = "Enter 1 to add another bounding box to same image, any other key for next image "))

        } #ends cycle
    }

    # generate output images
    if(outbox == TRUE) {outBox(points_master = points_master, file_path_input = file_path_input,
                                file_path_output = file_path_output, show_classifier = show_classifier)}
    return(points_master)
}
