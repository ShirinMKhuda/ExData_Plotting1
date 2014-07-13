# This function reads data from household power consumption data file
# and draws a histogram for Global_active_power. The histogram shows frequency 
# of the usuage of household global minute-averaged active power (in kilowatts).
# This histogram gets drawn in plot1.png file and after drawing this function closes the file.
# @ Shirin Murrassa-I-Khuda

plot1 <- function(data_file) {
    ## read file
    con <- file(data_file, "r")
    on.exit(close(con))
    Global_active_power <- double()
    day <- double()
  
    beg_date <- "01/02/2007"
    end_date <- "02/02/2007"
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
    

    if(new_date >= bg_date && new_date <= ed_date) {
        if(is.numeric(as.numeric(new_line[3]))) {
        Global_active_power[i] <- as.numeric(new_line[3])
        day[i] <- new_line[1]
        i <- i +1
        } 
    }
    
    if(ed_date < new_date) {
      break;
    }
    
  } 
    frame <- data.frame( day, Global_active_power)
    png(file = "figure/plot1.png", width = 480, height = 480, units ="px")
    hist(frame$Global_active_power, main = "Global Active Power", xlab = "Global Active Power (Kilowatts)", col="red")
    dev.off()
    print("plot1.png saved successfully.")
} 