# This function reads data from household power consumption data file
# and plots Sub_metering_1, Sub_metering_2, and Sub_metering_3 values 
# against two days of usuage. 
# This plot gets drawn in plot3.png file and 
# after drawing this function closes the plot3.png.
# @ Shirin Murrassa-I-Khuda

plot3 <- function(data_file) {
    ## read file
    con <- file(data_file, "r")
    on.exit(close(con))
  
    Sub_metering_1 <- double()
    Sub_metering_2 <- double()
    Sub_metering_3 <- double()
    day <- NULL
  
    beg_date <- "01/02/2007"
    end_date <- "03/02/2007"
    bg_date <- as.Date(beg_date, format = '%d/%m/%Y')
    ed_date <- as.Date(end_date, format = '%d/%m/%Y')
  
    ## reads the first line of headers from the file
    line <- readLines(con, 1)
    line_sep <- strsplit(line, ";")
    line_new <- strsplit(line_sep[[1]], " ")
    i <- 1
    ## reading second line to end of file when length of line is 0
    while(length(line <- readLines(con, 1)) >0 ) {
        sep_line <- strsplit(line, ";")
        new_line <- strsplit(sep_line[[1]], " ")
        new_date <- as.Date(new_line[[1]], format = '%d/%m/%Y')
    if(new_date >= bg_date && new_date < ed_date) {
        Sub_metering_1[i] <- as.double(new_line[[7]])
        Sub_metering_2[i] <- as.double(new_line[[8]])
        Sub_metering_3[i] <- as.double(new_line[[9]])
        new_date<- as.Date(new_line[[1]], format = '%d/%m/%Y')
        new_day <- weekdays(new_date)
        day[i] <- new_day
        i <- i +1
    }
    
    if(ed_date < new_date) {
        break;
    }
    
  }
     frame <- data.frame( day, Sub_metering_1, Sub_metering_2, Sub_metering_3)
     png(file = "figure/plot3.png", width = 480, height = 480, units ="px")
     par(mar = c(4, 6, 2, 2))
     plot(frame$Sub_metering_1, type ="n", axes=FALSE, ann=FALSE)
     lines(frame$Sub_metering_1, type ="l", col="black")
     lines(frame$Sub_metering_2, type ="l", col="red")
     lines(frame$Sub_metering_3, type ="l", col="blue")
     box(col ="black")
     legend("topright", lty = c(1,1,1), col = c("black", "red", "blue"), legend = c(line_new[[7]], line_new[[8]], line_new[[9]]))
     axis(1, at=c(1, 2880/2, 2880), labels =c("Thu", "Fri", "Sat"))
     axis(2, at=c(0, 10, 20, 30), labels =c("0", "10", "20", "30"))
     title(ylab = "Energy Sub Metering")
     dev.off()
     print("plot3.png file saved successfully")
  }