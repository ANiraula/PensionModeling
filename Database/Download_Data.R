##Working With Reason Database
#by Anil Niraula

###How to Download Reason data###

##Way [1]

######## Download state-level data from "GitHub":
##Use a combination of url() + read_csv()
#RUN THIS:
urlfile="https://raw.githubusercontent.com/ANiraula/PensionModeling/master/Database/reason.data.state.csv"
reason.data <- read_csv(url(urlfile), col_names = TRUE, na = c(""), skip_empty_rows = TRUE, col_types = NULL)

##Way [2]

######## Download the whole database from "R":
##Use a combination of pullData() + filter() from dplyr
##Packages: 
library(pensionviewr)#more info: https://github.com/ReasonFoundation/pensionviewr
library(dplyr)
#RUN THIS:
reason.data  <- pullData(pl, pl$display_name)) %>% arrange(state)
reason.data <- reason.data %>% filter(administering_government_type == 0)#To filter for state-level plans