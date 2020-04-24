#'Create a bounding box in an image based on selected points
#'
#'\code{boxer} allows the user to identify an area in an image around which to
#'generate a bounding box.
#'
#'Stream a series of images (JPEG, PNG, BMP) from a directory. As each image
#'appears, indicate between 2 and 26 points. A bounding box will be generated
#'based on the max/min x and y values of the selected coordinates. There are two
#'outputs. One is a dataframe with the name of the original image, the
#'coordinates of the bounding box and the classifier. Another is, for each
#'image, an image file annotated with the bounding box, the classifier printed
#'above the bounding box if show_classifier is set to TRUE, and resized if either
#'the size_x or size_y parameter is given.
#'
#'When an image appears, use the left mouse button to select a point, and the
#'right mouse button to signal completion and to move to the next image. At
#'least two points must be selected to record a bounding box. If more than 26
#'points are selected, only the first 26 will be used. At the completion of each
#'image, a file of the image with the bounding box will be output. If you do not
#'want to create a bounding box the image currently shown, select the right mouse
#'button without selecting any points. A warning message will be printed at the
#'end for each image skipped.
#'
#'The dataframe of bounding box coordinates will only be returned when all of
#'the images have been cycled through unless the stream is truncated at the
#'batch prompt.  The batch paramater can be set for the number of images to
#'cycle through before the user is prompted to truncate the stream. Selecting
#'"y" at the prompt will truncate the stream and return the bounding box
#'coordinates for all of the images up to that point. Selecting any other key
#'will continue to stream through the next batch of images.
#'
#'@param names A list that contains the names of JPG, PNG, or BMP image files.
#'@param file_path_input The directory that contains the image files.
#'@param file_path_output The directory where to save annotated images.
#'@param color The color of the bounding box. Default is "red".
#'@param size_x Number of columns to resize the x-axis. Default is NA for no
#'  change.
#'@param size_y Number of columns to resize the y-axis. Default is NA for no
#'  change.
#'@param classifier Character string to add a classifier to output. Default is
#'  NA.
#'@param show_classifier Logical to include the classifier above the bounding
#'  box. Default is F.
#'@param batch Number of images before prompt to truncate stream. Default is
#'  length(names).
#'
#'@return A dataframe with the bounding box coordinates for each image, and a
#'  new image file with the bounding box annotation for each of the images
#'  processed. The name of each output image file will be the same as the
#'  corresponding input file prefixed with "out_".
#'
#' @examples
#' \dontrun{
#' boxer(names = c("dog_pic1.jpg", "dog_pic2.jpg"),
#' file_path_input = "/dog_pics/input/", file_path_output = "/dog_pics/output/",
#' size_x = 224, size_y = 224, classifier = "dog", show_classifier = T)
#' }
#'
#'@export
boxer2 <- function(names, file_path_input, resize_x = NA, resize_y = NA,
                  batch = length(names), classifier = classifier, color = "red",
                  outpics = F, file_path_output = NA ,show_classifier = F) {

    #check for file formats
    if(!all(tools::file_ext(names) %in% c("jpg", "jpeg", "png", "bmp"))) stop('Only supports jp(e)g, png, bmp formats')

    #Empty data frame used to capture bounding box coorindates
    points_master <- data.frame()


    #Cycle through each of the named image files
    for(n in 1:length(names)){

        # offer User to breakout out of the loop
        if((n - 1 != 0) & ((n - 1) %% batch == 0)){
            response <- readline(prompt = "Enter 'y' if you want to truncate the stream or any other key to continue.")
            if(response == "y"){
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


        class <- as.numeric(readline(prompt = "Enter ref# for the classifier of this bounding box "))
        points_boxed$classifier <- as.character(classifier$class[classifier$ref == class])
        points_boxed$color <- as.character(classifier$color[classifier$ref == class])



        # Update dataframe with image coordinates
        points_master <- rbind(points_master, points_boxed)

        image <- if(box_count > 0){
                    recaptureImage(file_path_input = file_path_input, fname = fname, size_x = size_x, size_y = size_y)
                } else {
                    image
                }

        box_count <- box_count + 1

        cycle <- as.numeric(readline(prompt = "Select (0) for next image, (1) to add another bounding box "))

        } #ends cycle


    }
    if(outpics == T) {createBox(points_master = points_master, file_path_input = file_path_input,
                                file_path_output = file_path_output, show_classifier = show_classifier)}
    return(points_master)
}
