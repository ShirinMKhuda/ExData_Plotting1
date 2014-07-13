# This function reads data from household power consumption data file
# and creates four plots. The first plot creates a plot of Global Active
# Power against two days of usuage. The second plot plots Voltage against 
# two days ususage. The third plot plots Sub_metering_1, Sub_metering_2, 
# and Sub_metering_3 values against two days of usuage. The fourth one 
# creates plot of Global reactive power against two days of usuage.
# These plots are all drawn in plot4.png file and 
# after drawing this function closes the plot4.png.
# @ Shirin Murrassa-I-Khuda
plot4 <- function(data_file) {
    ## read file
    con <- file(data_file, "r")
    on.exit(close(con))
  
    Global_active_power <- double()
    Global_reactive_power <- double()
    Voltage <- double()
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
        Global_active_power[i] <-as.double(new_line[[3]]) 
        Global_reactive_power[i] <-as.double(new_line[[4]]) 
        Voltage[i] <-as.double(new_line[[5]])
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
    frame <- data.frame( day, Global_active_power, Global_reactive_power, Voltage, Sub_metering_1, Sub_metering_2, Sub_metering_3)
    png(file = "figure/plot4.png", width = 480, height = 480, units ="px")
    par(mfrow=c(2,2))
    plot(frame$Global_active_power, type ="n", axes=FALSE, ann=FALSE)
    lines(frame$Global_active_power, col ="black")
    axis(1, at=c(1, 2880/2, 2880), labels =c("Thu", "Fri", "Sat"))
    axis(2, at=c(0, 2, 4, 6), labels =c("0", "2", "4", "6"))
    box(col = "black")
    title( ylab = "Global Active Power (kilowatts)")
  
    plot(frame$Voltage, type ="n", axes=FALSE, ann=FALSE)
    lines(frame$Voltage, col ="black")
    axis(1, at=c(1, 2880/2, 2880), labels =c("Thu", "Fri", "Sat"))
    axis(2, at=c(234, 238, 242, 246), labels =c("234", "238", "242", "246"))
    box(col = "black")
    title( ylab = "Voltage", xlab = "datetime")
  
  
    plot(frame$Sub_metering_1, type ="n", axes=FALSE, ann=FALSE)
    lines(frame$Sub_metering_1, type ="l", col="black")
    lines(frame$Sub_metering_2, type ="l", col="red")
    lines(frame$Sub_metering_3, type ="l", col="blue")
    legend("topright", lty = c(1,1,1), cex=0.5,  col = c("black", "red", "blue"), legend = c(line_new[[7]], line_new[[8]], line_new[[9]]))
    axis(1, at=c(1, 2880/2, 2880), labels =c("Thu", "Fri", "Sat"))
    axis(2, at=c(0, 10, 20, 30), labels =c("0", "10", "20", "30"))
    box(col ="black")
    title(ylab = "Energy Sub Metering")
  
    plot(frame$Global_reactive_power, type ="n", axes=FALSE, ann=FALSE)
    lines(frame$Global_reactive_power, col ="black")
    axis(1, at=c(1, 2880/2, 2880), labels =c("Thu", "Fri", "Sat"))
    axis(2, at=c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5), labels =c("0.0","0.1", "0.2", "0.3", "0.4", "0.5"), cex=0.5)
    box(col = "black")
    title( ylab = "Global_reactive_power", xlab = "datetime")
    dev.off()
    print("plot4.png file saved successfully")
  } 