
paramCheck <- function(names, file_path_input, color, outbox, file_path_output, classifier = NA, checks,
                       points_master = NA) {

    checksList <- list(
        #1
        function(){if(!is.vector(names)) {
            stop('names must be a vector')}},
        #2
        function(){if(!all(tools::file_ext(names) %in% c("jpg", "jpeg", "png", "bmp"))) {
            stop('Only supports jp(e)g, png, bmp formats')}},
        #3
        function(){if(!dir.exists(file_path_input)) {
            stop('file_path_input not found')}},
        #4
        function(){if(outbox == TRUE) if(!dir.exists(file_path_output)) {
            stop('file_path_output not found')}},
        #5
        function(){if(!color %in% grDevices::colors()) {
            stop('See grDevices::color() for valid color names')}},
        #6
        function(){if(!is.data.frame(classifier)) {
            stop('classifier should be a data.frame')}},
        #7
        function(){if(!all(names(classifier) %in% c("ref", "class", "color"))) {
            stop('classifier columns should be "ref", "color", and "class"')}},
        #8
        function(){if(nrow(classifier) == 0) {
            stop('classifier is empty')}},
        #9
        function(){if(!is.integer(classifier$ref)) {
            stop('The value for "ref" in classifier should be an integer')}},
        #10
        function(){if(!length(unique(classifier$ref)) == nrow(classifier)) {
            stop('Values in "ref" must be unique')}},
        #11
        function(){if(!all(classifier$color %in% grDevices::colors())) {
            stop('See grDevices::color() for valid color names')}},
        #12
        function(){if(!is.data.frame(points_master)) {
            stop('points_master should be a data.frame')}},
        #13
        function(){if(!all(names(points_master) %in%
            c("file_name", "x_left", "y_top", "x_right", "y_bottom",
                "size_x", "size_y", "classifier","color"))) {
            stop('Review help for required fields in points_master')}},
        #14
        function(){if(!is.integer(c(points_master$x_left, points_master$y_top,
            points_master$x_right, points_master$y_bottom, points_master$size_x,
            points_master$size_y))) {
            stop('Review help for which fields in points_master should be integers')}},
        #15
        function(){if(!all(points_master$color %in% grDevices::colors())) {
            stop('See grDevices::color() for valid color names')}}


    )

    for(n in checks){
    checksList[[n]]()
    }
}
