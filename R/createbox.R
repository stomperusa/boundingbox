createBox <- function(points_master, file_path_input, file_path_output, show_classifier = F){

    for (fname in points_master$file_name){

       image <- recaptureImage(file_path_input = file_path_input, fname = fname,
                               size_x = points_master[points_master$file_name == fname,"size_x"][1],
                               size_y = points_master[points_master$file_name == fname,"size_y"][1])


       points_boxed <- points_master[points_master$file_name == fname,]



    #DRAW THE BOUNDING BOX OVER THE IMAGE AND SAVE AS OUTPUT
    if (tools::file_ext(fname) =="png"){
        grDevices::png(filename= paste0(file_path_output, "out_",fname),
                       width =  points_master[points_master$file_name == fname,"size_x"][1],
                       height = points_master[points_master$file_name == fname,"size_y"][1])

    } else if (tools::file_ext(fname) =="bmp") {
        grDevices::bmp(filename= paste0(file_path_output, "out_",fname),
                       width =  points_master[points_master$file_name == fname,"size_x"][1],
                       height = points_master[points_master$file_name == fname,"size_y"][1])

    } else {
        grDevices::jpeg(filename= paste0(file_path_output, "out_",fname),
                        width =  points_master[points_master$file_name == fname,"size_x"][1],
                        height = points_master[points_master$file_name == fname,"size_y"][1],
                        quality = 100)
    }

    graphics::plot(image)

    for (n in 1:nrow(points_boxed)){

        lab <- ifelse(show_classifier == TRUE, points_boxed$classifier[n], "")

        graphics::lines(c(points_boxed$x_left[n], points_boxed$x_left[n]),
                        c(points_boxed$size_y[n] - points_boxed$y_top[n], points_boxed$size_y[n] - points_boxed$y_bottom[n]),
                        lwd = (points_boxed$size_x[n]/360),  col = points_boxed$color[n])
        graphics::lines(c(points_boxed$x_right[n], points_boxed$x_right[n]),
                        c(points_boxed$size_y[n] - points_boxed$y_top[n], points_boxed$size_y[n] - points_boxed$y_bottom[n]),
                        lwd = (points_boxed$size_x[n]/360),  col = points_boxed$color[n])
        graphics::lines(c(points_boxed$x_left[n], points_boxed$x_right[n]),
                        c(points_boxed$size_y[n] - points_boxed$y_top[n], points_boxed$size_y[n] - points_boxed$y_top[n]),
                        lwd = (points_boxed$size_x[n]/360),  col = points_boxed$color[n])
        graphics::lines(c(points_boxed$x_left[n], points_boxed$x_right[n]),
                        c(points_boxed$size_y[n] - points_boxed$y_bottom[n], points_boxed$size_y[n] - points_boxed$y_bottom[n]),
                        lwd = (points_boxed$size_x[n]/360),  col = points_boxed$color[n])
        graphics::text(x = (points_boxed$x_left[n] + points_boxed$x_right[n])/2, y = (points_boxed$size_y[n] - points_boxed$y_top[n] + 5),
                       label = lab, col = points_boxed$color[n], cex = (points_boxed$size_x[n]/360))
    }

    grDevices::dev.off()


    }
    print(paste0("Files created in ", file_path_output))
}



