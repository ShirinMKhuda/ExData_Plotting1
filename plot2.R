# This function reads data from household power consumption data file
# and plots Global_active_power against two days of usuage. Global active power variable
# contains usuage of household global minute-averaged active power (in kilowatts).
# This plot gets drawn in plot2.png file and after drawing this function closes the plot2.png.
# @ Shirin Murrassa-I-Khuda
plot2 <- function(data_file) {
    ## read file
    con <- file(data_file, "r")
    on.exit(close(con))
  
    Global_active_power <- double()
    day <- NULL
  
    beg_date <- "01/02/2007"
    end_date <- "03/02/2007"
    bg_date <- as.Date(beg_date, format = '%d/%m/%Y')
    ed_date <- as.Date(end_date, format = '%d/%m/%Y')
  
    ## reads the first line of headers from the file
    line <- readLines(con, 1)
    i <- 1
  
    ## reading second line to end of file when length of line is 0
    while(length(line <- readLines(con, 1)) >0 ) {
        sep_line <- strsplit(line, ";")
        new_line <- strsplit(sep_line[[1]], " ")
        new_date <- as.Date(new_line[[1]], format = '%d/%m/%Y')

    if(new_date >= bg_date && new_date < ed_date) {
        if(is.double(as.double(new_line[[3]]))) {
            Global_active_power[i] <- as.double(new_line[[3]])
            new_date<- as.Date(new_line[[1]], format = '%d/%m/%Y')
            new_day <- weekdays(new_date)
            day[i] <- new_day
            i <- i +1
        }  
    }
    
    if(ed_date < new_date) {
        break;
    }
    
  } 
    frame <- data.frame( day, Global_active_power)
    png(file = "figure/plot2.png", width = 480, height = 480, units ="px")
    par(mar = c(4, 6, 2, 2))
    plot(frame$Global_active_power, type ="n", axes=FALSE, ann=FALSE)
    lines(frame$Global_active_power, col ="black")
    axis(1, at=c(1, 2880/2, 2880), labels =c("Thu", "Fri", "Sat"))
    axis(2, at=c(0, 2, 4, 6), labels =c("0", "2", "4", "6"))
    box(col = "black")
    title( ylab = "Global Active Power (kilowatts)")
    dev.off()
    print("plot2.png file saved successfully")
  } 