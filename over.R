over <- function(file, otsrochka=30){ 
        library(dplyr)
        data <- read.csv2(file, header=F, skip=1) ##change to file.xlsx
        data[,1]<- as.Date(data[,1], "%d.%m.%Y")
        data <- data[!is.na(data[,1]),]
        data <- select(data, V1:V8) ##��������
        for(i in 6:8) {
                data[,i] <- sub(",", ".", data[,i]) ##change demical
                data[,i] <- gsub("\\s", "", data[,i]) ## del all! spaces, incl hard space
                data[,i] <- as.numeric(data[,i])
        }
        debt <- data$V8[1]
        data1 <- data %>% select(V1, V6, V7) %>%
                group_by(V1) %>%
                summarize(otgr=sum(V6, na.rm=T), opl=sum(V7, na.rm=T))
        for(i in 2:length(data1$opl)){ ##����������� ����, ����� ������� ��������� ����
                if(debt>=0) {
                        debt <- debt-data1$opl[i]
                }
                else {
                        print(data1$V1[i-1])
                        index_date <<- i
                        pereplata <<- -debt
                        break
                }
        }
        
        for(i in index_date:length(data1$opl)){
                otgruzki <- filter(data1,V1 <= data1$V1[i]-otsrochka) ## �������� � ������ ������� �� ���� ������-��������
                if(sum(data1$opl[index_date:i], pereplata) < sum(otgruzki$otgr)){ ##���� ����� ����� �� ���� ������ ����� �������� � ������ ������� �� ����-��������
                        if(data1$opl[i]>0) (cat("\n ������", 
                                                round(data1$opl[i]/1000, 0), 
                                                "���. ���", 
                                                format(data1$V1[i], format="%d-%b-%Y"), 
                                                "� ����������"))
                }
                else if(data1$opl[i]>0) cat("\n ������", 
                                            round(data1$opl[i]/1000, 0), 
                                            "���. ���", 
                                            format(data1$V1[i], format="%d-%b-%Y"), 
                                            "��� ���������")
        }
}
