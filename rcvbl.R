rcvbl <- function(file, date1=0, date2=0){ 
        data <- read.csv2(file, header=F, skip=1) ##change to file
        data[,1]<- as.Date(data[,1], "%d.%m.%Y")
        data <- data[!is.na(data[,1]),]
        dates <- seq(as.Date(min(data[,1])), as.Date(max(data[,1])), by = 1)
        for(i in 6:9) {
                data[,i] <- sub(",", ".", data[,i]) ##change demical
                data[,i] <- gsub("\\s", "", data[,i]) ## del all spaces, incl harp sp
                data[,i] <- as.numeric(data[,i])
        }
        vec <- NULL
        for(i in 1:length(dates)){
                a <- data[data$V1==dates[i],]
                if(length(a$V8)==0){
                        debt <- vec[length(vec)]
                        vec <- c(vec, debt)}
                else { 
                        debt <- max(a$V8)
                        vec <- c(vec, debt)
                }
        }
        mean_debt <- mean(vec)
        revenue <- sum(data$V6, na.rm=T)
        rcvbls <- mean_debt / revenue * as.numeric(max(data[,1])-min(data[,1]))
        cat("Оборачиваемость за период",
            as.character(c(format(min(data[,1]), format="%d-%b-%Y"), format(max(data[,1]), format="%d-%b-%Y"))),
            "составила",
            round(rcvbls), "дн.") 
}
