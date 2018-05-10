#Jig14947

#Set_Path
setwd("C:\\Data Science with R\\Assignments\\Graded Assignments\\Topic 13 -  Final Case Study Course Wrap up")

#Import_Data_Set
data<-read.csv("telecomfinal.csv")
#View(data)

#Import data dictionary
data_dic<-readxl::read_xls("data_documentation.xls")
#View(data_dic)

#Import Sample_Data
data_sam<-readxl::read_xlsx("Sample Data quality report.xlsx")
#View(data_sam)

#Change "character" to "data.frame" Variable_Names
data_var<-names(data)
class(data_var)
data_var<-data.frame(data_var)
class(data_var)

#Extraction the datatype
data_var$DataType<-sapply(data,class)

#No of records
data_var$No.of_Records<-nrow(data)

#No_of_unique Records
for(i in 1:length(data)){
  data_var$Unique_Reco[i]<-length(unique(data[,i]))
}

#Data_Avaliable
data_var$Data_Aval<-data_var$No.of_Records-colSums(is.na(data))

#Data_Avaliable in Percentage
data_var$Data_Aval_Per<-round(data_var$Data_Aval/nrow(data),3)


#Check missing values
sum(is.na(data))
data_var$No.of_miss<-colSums(is.na(data))

#Missing_value in Percentage
data_var$No.of_miss_Per<-round(data_var$No.of_miss/nrow(data),3)



# "Minimum"          "Maximum"          "Mean"             "5th Percentile"  
# "10th Percentile"  "25th Percentile"  "50th Percentile" 
# "75th Percentile"  "90th Percentile"  "95th Percentile"

summary(data)

for(i in 1:length(data)){
  data_var$Minimum[i]<-round(ifelse(class(data[,i])=="integer"|class(data[,i])=="numeric",min(data[,i],na.rm = T),0),2)
  data_var$Maximum[i]<-round(ifelse(class(data[,i])=="integer"|class(data[,i])=="numeric",max(data[,i],na.rm = T),0),2)
  data_var$Mean[i]<-round(ifelse(class(data[,i])=="integer"|class(data[,i])=="numeric",mean(data[,i],na.rm = T),0),2)
  data_var$fifth.th_Percentile[i]<-round(ifelse(class(data[,i])=="integer"|class(data[,i])=="numeric",quantile(data[,i],p=0.05,na.rm = T),0),2)
  data_var$Ten.th_Percentile[i]<-round(ifelse(class(data[,i])=="integer"|class(data[,i])=="numeric",quantile(data[,i],p=0.10,na.rm = T),0),2)
  data_var$Twenty.fifth.th_Percentile[i]<-round(ifelse(class(data[,i])=="integer"|class(data[,i])=="numeric",quantile(data[,i],p=0.25,na.rm = T),0),2)
  data_var$Fifthy.th_Percentile[i]<-round(ifelse(class(data[,i])=="integer"|class(data[,i])=="numeric",quantile(data[,i],p=0.50,na.rm = T),0),2)
  data_var$Seventy.fifth.th_Percentile[i]<-round(ifelse(class(data[,i])=="integer"|class(data[,i])=="numeric",quantile(data[,i],p=0.75,na.rm = T),0),2)
  data_var$nine.th_Percentile[i]<-round(ifelse(class(data[,i])=="integer"|class(data[,i])=="numeric",quantile(data[,i],p=0.90,na.rm = T),0),2)
  data_var$ninty.fifth_Percentile[i]<-round(ifelse(class(data[,i])=="integer"|class(data[,i])=="numeric",quantile(data[,i],p=0.95,na.rm = T),0),2)
}

#write data quality report in to csv file
write.csv(data_var,"C:\\Jig14947\\T13\\data_quality_sample.csv",row.names = T)
data_var_ori<-read.csv("C:\\Jig14947\\T13\\data_quality_sample.csv")


#Remove the column which have more than 20% of missing values
index<-ifelse(data_var$No.of_miss_Per>0.20,colnames(data),0)
index        # (12, 46, 47, 48, 49, 52, 53, 55, 61, 62, 63, 64, 66, 72) 


apply(data,2,function(x)sum(is.na(x))/length(x))*100

new_data<-data[,-c(12, 46, 47, 48, 49, 52, 55, 61, 62, 63, 64, 66, 72)]
sum(is.na(data))
sum(is.na(new_data))
View(new_data)

#the retdays have many missin values and that's mean is that no retaintion call so
#so instead of removing we will consider NA as 0

new_data$retcall<-ifelse(is.na(data$retdays)==TRUE,0,1)

#Removing few observation which have more than 6 same variable missing calues

#  index1<-which(is.na(new_data$da_Mean))
summary(new_data[47])
new_data<-new_data[,-47]

#Remove few observation which have more than 6 same variable
index<-which(is.na(new_data$da_Mean))
new_data<-new_data[-index,]

#Check missing values
apply(new_data,2,function(x)sum(is.na(x))/length(x))*100
sum(is.na(new_data))

#Variable profiling continous variable, categorical variables
library(dplyr)

#mou_Mean()
new_data %>% mutate(dec=ntile(mou_Mean,n=10)) %>% count(churn,dec) %>% filter(churn==1)->dat01
dat01$N<-unclass(new_data %>% mutate(dec=ntile(mou_Mean,n=10)) %>% count(dec) %>% unname())[[2]]
dat01$churn_perc<-dat01$n/dat01$N
dat01$Greater_Than<-unclass(new_data %>% mutate(dec=ntile(mou_Mean,n=10)) %>%group_by(dec) %>%  summarise(min(mou_Mean)))[[2]]
dat01$Less_Than<-unclass(new_data %>% mutate(dec=ntile(mou_Mean,n=10)) %>% group_by(dec) %>% summarise(max(mou_Mean)))[[2]]
dat01$varname<-rep("mou_Mean",nrow(dat01))

#totmrc_Mean()

new_data %>% mutate(dec=ntile(totmrc_Mean,n=10)) %>% count(churn,dec) %>% filter(churn==1)->dat02
dat02$N<-unclass(new_data %>% mutate(dec=ntile(totmrc_Mean,n=10)) %>% count(dec) %>% unname())[[2]]
dat02$churn_perc<-dat02$n/dat02$N
dat02$Greater_Than<-unclass(new_data %>% mutate(dec=ntile(totmrc_Mean,n=10)) %>%group_by(dec) %>%  summarise(min(totmrc_Mean)))[[2]]
dat02$Less_Than<-unclass(new_data %>% mutate(dec=ntile(totmrc_Mean,n=10)) %>% group_by(dec) %>% summarise(max(totmrc_Mean)))[[2]]
dat02$varname<-rep("totmrc_Mean",nrow(dat02))

#rev_Range()

new_data %>% mutate(dec=ntile(rev_Range,n=10)) %>% count(churn,dec) %>% filter(churn==1)->dat03
dat03$N<-unclass(new_data %>% mutate(dec=ntile(rev_Range,n=10)) %>% count(dec) %>% unname())[[2]]
dat03$churn_perc<-dat03$n/dat03$N
dat03$Greater_Than<-unclass(new_data %>% mutate(dec=ntile(rev_Range,n=10)) %>%group_by(dec) %>%  summarise(min(rev_Range)))[[2]]
dat03$Less_Than<-unclass(new_data %>% mutate(dec=ntile(rev_Range,n=10)) %>% group_by(dec) %>% summarise(max(rev_Range)))[[2]]
dat03$varname<-rep("rev_Range",nrow(dat03))

#mou_Range()

new_data %>% mutate(dec=ntile(mou_Range,n=10)) %>% count(churn,dec) %>% filter(churn==1)->dat04
dat04$N<-unclass(new_data %>% mutate(dec=ntile(mou_Range,n=10)) %>% count(dec) %>% unname())[[2]]
dat04$churn_perc<-dat04$n/dat04$N
dat04$Greater_Than<-unclass(new_data %>% mutate(dec=ntile(mou_Range,n=10)) %>%group_by(dec) %>%  summarise(min(mou_Range)))[[2]]
dat04$Less_Than<-unclass(new_data %>% mutate(dec=ntile(mou_Range,n=10)) %>% group_by(dec) %>% summarise(max(mou_Range)))[[2]]
dat04$varname<-rep("mou_Range",nrow(dat04))

#change_mou()

new_data %>% mutate(dec=ntile(change_mou,n=10)) %>% count(churn,dec) %>% filter(churn==1)->dat05
dat05$N<-unclass(new_data %>% mutate(dec=ntile(change_mou,n=10)) %>% count(dec) %>% unname())[[2]]
dat05$churn_perc<-dat05$n/dat05$N
dat05$Greater_Than<-unclass(new_data %>% mutate(dec=ntile(change_mou,n=10)) %>%group_by(dec) %>%  summarise(min(change_mou)))[[2]]
dat05$Less_Than<-unclass(new_data %>% mutate(dec=ntile(change_mou,n=10)) %>% group_by(dec) %>% summarise(max(change_mou)))[[2]]
dat05$varname<-rep("change_mou",nrow(dat05))

#drop_blk_Mean()

new_data %>% mutate(dec=ntile(drop_blk_Mean,n=10)) %>% count(churn,dec) %>% filter(churn==1)->dat06
dat06$N<-unclass(new_data %>% mutate(dec=ntile(drop_blk_Mean,n=10)) %>% count(dec) %>% unname())[[2]]
dat06$churn_perc<-dat06$n/dat06$N
dat06$Greater_Than<-unclass(new_data %>% mutate(dec=ntile(drop_blk_Mean,n=10)) %>%group_by(dec) %>%  summarise(min(drop_blk_Mean)))[[2]]
dat06$Less_Than<-unclass(new_data %>% mutate(dec=ntile(drop_blk_Mean,n=10)) %>% group_by(dec) %>% summarise(max(drop_blk_Mean)))[[2]]
dat06$varname<-rep("drop_blk_Mean",nrow(dat06))

#drop_vce_Range()

new_data %>% mutate(dec=ntile(drop_vce_Range,n=10)) %>% count(churn,dec) %>% filter(churn==1)->dat07
dat07$N<-unclass(new_data %>% mutate(dec=ntile(drop_vce_Range,n=10)) %>% count(dec) %>% unname())[[2]]
dat07$churn_perc<-dat07$n/dat07$N
dat07$Greater_Than<-unclass(new_data %>% mutate(dec=ntile(drop_vce_Range,n=10)) %>%group_by(dec) %>%  summarise(min(drop_vce_Range)))[[2]]
dat07$Less_Than<-unclass(new_data %>% mutate(dec=ntile(drop_vce_Range,n=10)) %>% group_by(dec) %>% summarise(max(drop_vce_Range)))[[2]]
dat07$varname<-rep("drop_vce_Range",nrow(dat07))

#owylis_vce_Range()

new_data %>% mutate(dec=ntile(owylis_vce_Range,n=10)) %>% count(churn,dec) %>% filter(churn==1)->dat08
dat08$N<-unclass(new_data %>% mutate(dec=ntile(owylis_vce_Range,n=10)) %>% count(dec) %>% unname())[[2]]
dat08$churn_perc<-dat08$n/dat08$N
dat08$Greater_Than<-unclass(new_data %>% mutate(dec=ntile(owylis_vce_Range,n=10)) %>%group_by(dec) %>%  summarise(min(owylis_vce_Range)))[[2]]
dat08$Less_Than<-unclass(new_data %>% mutate(dec=ntile(owylis_vce_Range,n=10)) %>% group_by(dec) %>% summarise(max(owylis_vce_Range)))[[2]]
dat08$varname<-rep("owylis_vce_Range",nrow(dat08))

#mou_opkv_Range()

new_data %>% mutate(dec=ntile(mou_opkv_Range,n=10)) %>% count(churn,dec) %>% filter(churn==1)->dat09
dat09$N<-unclass(new_data %>% mutate(dec=ntile(mou_opkv_Range,n=10)) %>% count(dec) %>% unname())[[2]]
dat09$churn_perc<-dat09$n/dat09$N
dat09$Greater_Than<-unclass(new_data %>% mutate(dec=ntile(mou_opkv_Range,n=10)) %>%group_by(dec) %>%  summarise(min(mou_opkv_Range)))[[2]]
dat09$Less_Than<-unclass(new_data %>% mutate(dec=ntile(mou_opkv_Range,n=10)) %>% group_by(dec) %>% summarise(max(mou_opkv_Range)))[[2]]
dat09$varname<-rep("mou_opkv_Range",nrow(dat09))

#months()

new_data %>% mutate(dec=ntile(months,n=10)) %>% count(churn,dec) %>% filter(churn==1)->dat10
dat10$N<-unclass(new_data %>% mutate(dec=ntile(months,n=10)) %>% count(dec) %>% unname())[[2]]
dat10$churn_perc<-dat10$n/dat10$N
dat10$Greater_Than<-unclass(new_data %>% mutate(dec=ntile(months,n=10)) %>%group_by(dec) %>%  summarise(min(months)))[[2]]
dat10$Less_Than<-unclass(new_data %>% mutate(dec=ntile(months,n=10)) %>% group_by(dec) %>% summarise(max(months)))[[2]]
dat10$varname<-rep("months",nrow(dat10))

#totcalls()

new_data %>% mutate(dec=ntile(totcalls,n=10)) %>% count(churn,dec) %>% filter(churn==1)->dat11
dat11$N<-unclass(new_data %>% mutate(dec=ntile(totcalls,n=10)) %>% count(dec) %>% unname())[[2]]
dat11$churn_perc<-dat11$n/dat11$N
dat11$Greater_Than<-unclass(new_data %>% mutate(dec=ntile(totcalls,n=10)) %>%group_by(dec) %>%  summarise(min(totcalls)))[[2]]
dat11$Less_Than<-unclass(new_data %>% mutate(dec=ntile(totcalls,n=10)) %>% group_by(dec) %>% summarise(max(totcalls)))[[2]]
dat11$varname<-rep("totcalls",nrow(dat11))

#eqpdays()
#one missing value observed. To move forward we need to remove it

index<-which(is.na(new_data$eqpdays))
new_data<-new_data[-index,]

new_data %>% mutate(dec=ntile(eqpdays,n=10)) %>% count(churn,dec) %>% filter(churn==1)->dat12
dat12$N<-unclass(new_data %>% mutate(dec=ntile(eqpdays,n=10)) %>% count(dec) %>% unname())[[2]]
dat12$churn_perc<-dat12$n/dat12$N
dat12$Greater_Than<-unclass(new_data %>% mutate(dec=ntile(eqpdays,n=10)) %>%group_by(dec) %>%  summarise(min(eqpdays)))[[2]]
dat12$Less_Than<-unclass(new_data %>% mutate(dec=ntile(eqpdays,n=10)) %>% group_by(dec) %>% summarise(max(eqpdays)))[[2]]
dat12$varname<-rep("eqpdays",nrow(dat12))

#custcare_Mean()


new_data %>% mutate(dec=ntile(custcare_Mean,n=10)) %>% count(churn,dec) %>% filter(churn==1)-> dat13
dat13$varname<-rep("custcare_Mean",nrow(dat13))

#Only 7 deciles are available

#callwait_Mean()  4 deciles

new_data %>% mutate(dec=ntile(callwait_Mean,n=4)) %>% count(churn,dec) %>% filter(churn==1)->dat14
dat14$N<-unclass(new_data %>% mutate(dec=ntile(callwait_Mean,n=4)) %>% count(dec) %>% unname())[[2]]
dat14$churn_perc<-dat14$n/dat14$N
dat14$Greater_Than<-unclass(new_data %>% mutate(dec=ntile(callwait_Mean,n=4)) %>%group_by(dec) %>%  summarise(min(callwait_Mean)))[[2]]
dat14$Less_Than<-unclass(new_data %>% mutate(dec=ntile(callwait_Mean,n=4)) %>% group_by(dec) %>% summarise(max(callwait_Mean)))[[2]]
dat14$varname<-rep("callwait_Mean",nrow(dat14))

#iwylis_vce_Mean() 6 deciles

new_data %>% mutate(dec=ntile(iwylis_vce_Mean,n=6)) %>% count(churn,dec) %>% filter(churn==1)->dat15
dat15$N<-unclass(new_data %>% mutate(dec=ntile(iwylis_vce_Mean,n=6)) %>% count(dec) %>% unname())[[2]]
dat15$churn_perc<-dat15$n/dat15$N
dat15$Greater_Than<-unclass(new_data %>% mutate(dec=ntile(iwylis_vce_Mean,n=6)) %>%group_by(dec) %>%  summarise(min(iwylis_vce_Mean)))[[2]]
dat15$Less_Than<-unclass(new_data %>% mutate(dec=ntile(iwylis_vce_Mean,n=6)) %>% group_by(dec) %>% summarise(max(iwylis_vce_Mean)))[[2]]
dat15$varname<-rep("iwylis_vce_Mean",nrow(dat15))

#callwait_Range() getting less than 4 deciles only therefore omit

new_data %>% mutate(dec=ntile(callwait_Range,n=4)) %>% count(churn,dec) %>% filter(churn==1)->dat16
dat16$varname<-rep("callwait_Range",nrow(dat16))

#ccrndmou_Range()

new_data %>% mutate(dec=ntile(ccrndmou_Range,n=4)) %>% count(churn,dec) %>% filter(churn==1)->dat17
dat17$varname<-rep("ccrndmou_Range",nrow(dat17))

#adjqty()

new_data %>% mutate(dec=ntile(adjqty,n=10)) %>% count(churn,dec) %>% filter(churn==1)->dat18
dat18$N<-unclass(new_data %>% mutate(dec=ntile(adjqty,n=10)) %>% count(dec) %>% unname())[[2]]
dat18$churn_perc<-dat18$n/dat18$N
dat18$Greater_Than<-unclass(new_data %>% mutate(dec=ntile(adjqty,n=10)) %>%group_by(dec) %>%  summarise(min(adjqty)))[[2]]
dat18$Less_Than<-unclass(new_data %>% mutate(dec=ntile(adjqty,n=10)) %>% group_by(dec) %>% summarise(max(adjqty)))[[2]]
dat18$varname<-rep("adjqty",nrow(dat18))

#ovrrev_Mean()


new_data %>% mutate(dec=ntile(ovrrev_Mean,n=4)) %>% count(churn,dec) %>% filter(churn==1)->dat19
dat19$N<-unclass(new_data %>% mutate(dec=ntile(ovrrev_Mean,n=4)) %>% count(dec) %>% unname())[[2]]
dat19$churn_perc<-dat19$n/dat19$N
dat19$Greater_Than<-unclass(new_data %>% mutate(dec=ntile(ovrrev_Mean,n=4)) %>%group_by(dec) %>%  summarise(min(ovrrev_Mean)))[[2]]
dat19$Less_Than<-unclass(new_data %>% mutate(dec=ntile(ovrrev_Mean,n=4)) %>% group_by(dec) %>% summarise(max(ovrrev_Mean)))[[2]]
dat19$varname<-rep("ovrrev_Mean",nrow(dat19))

#rev_Mean() 10

new_data %>% mutate(dec=ntile(rev_Mean,n=10)) %>% count(churn,dec) %>% filter(churn==1)->dat20
dat20$N<-unclass(new_data %>% mutate(dec=ntile(rev_Mean,n=10)) %>% count(dec) %>% unname())[[2]]
dat20$churn_perc<-dat20$n/dat20$N
dat20$Greater_Than<-unclass(new_data %>% mutate(dec=ntile(rev_Mean,n=10)) %>%group_by(dec) %>%  summarise(min(rev_Mean)))[[2]]
dat20$Less_Than<-unclass(new_data %>% mutate(dec=ntile(rev_Mean,n=10)) %>% group_by(dec) %>% summarise(max(rev_Mean)))[[2]]
dat20$varname<-rep("rev_Mean",nrow(dat20))

#ovrmou_Mean() 4

new_data %>% mutate(dec=ntile(ovrmou_Mean,n=4)) %>% count(churn,dec) %>% filter(churn==1)->dat21
dat21$N<-unclass(new_data %>% mutate(dec=ntile(ovrmou_Mean,n=4)) %>% count(dec) %>% unname())[[2]]
dat21$churn_perc<-dat21$n/dat21$N
dat21$Greater_Than<-unclass(new_data %>% mutate(dec=ntile(ovrmou_Mean,n=4)) %>%group_by(dec) %>%  summarise(min(ovrmou_Mean)))[[2]]
dat21$Less_Than<-unclass(new_data %>% mutate(dec=ntile(ovrmou_Mean,n=4)) %>% group_by(dec) %>% summarise(max(ovrmou_Mean)))[[2]]
dat21$varname<-rep("ovrmou_Mean",nrow(dat21))

#comp_vce_Mean() 10

new_data %>% mutate(dec=ntile(comp_vce_Mean,n=10)) %>% count(churn,dec) %>% filter(churn==1)->dat22
dat22$N<-unclass(new_data %>% mutate(dec=ntile(comp_vce_Mean,n=10)) %>% count(dec) %>% unname())[[2]]
dat22$churn_perc<-dat22$n/dat22$N
dat22$Greater_Than<-unclass(new_data %>% mutate(dec=ntile(comp_vce_Mean,n=10)) %>%group_by(dec) %>%  summarise(min(comp_vce_Mean)))[[2]]
dat22$Less_Than<-unclass(new_data %>% mutate(dec=ntile(comp_vce_Mean,n=10)) %>% group_by(dec) %>% summarise(max(comp_vce_Mean)))[[2]]
dat22$varname<-rep("comp_vce_Mean",nrow(dat22))

#plcd_vce_Mean() 10

new_data %>% mutate(dec=ntile(plcd_vce_Mean,n=10)) %>% count(churn,dec) %>% filter(churn==1)->dat23
dat23$N<-unclass(new_data %>% mutate(dec=ntile(plcd_vce_Mean,n=10)) %>% count(dec) %>% unname())[[2]]
dat23$churn_perc<-dat23$n/dat23$N
dat23$Greater_Than<-unclass(new_data %>% mutate(dec=ntile(plcd_vce_Mean,n=10)) %>%group_by(dec) %>%  summarise(min(plcd_vce_Mean)))[[2]]
dat23$Less_Than<-unclass(new_data %>% mutate(dec=ntile(plcd_vce_Mean,n=10)) %>% group_by(dec) %>% summarise(max(plcd_vce_Mean)))[[2]]
dat23$varname<-rep("plcd_vce_Mean",nrow(dat23))

#avg3mou() 4

new_data %>% mutate(dec=ntile(avg3mou,n=4)) %>% count(churn,dec) %>% filter(churn==1)->dat24
dat24$N<-unclass(new_data %>% mutate(dec=ntile(avg3mou,n=4)) %>% count(dec) %>% unname())[[2]]
dat24$churn_perc<-dat24$n/dat24$N
dat24$Greater_Than<-unclass(new_data %>% mutate(dec=ntile(avg3mou,n=4)) %>%group_by(dec) %>%  summarise(min(avg3mou)))[[2]]
dat24$Less_Than<-unclass(new_data %>% mutate(dec=ntile(avg3mou,n=4)) %>% group_by(dec) %>% summarise(max(avg3mou)))[[2]]
dat24$varname<-rep("avg3mou",nrow(dat24))

#avgmou() 10

new_data %>% mutate(dec=ntile(avgmou,n=10)) %>% count(churn,dec) %>% filter(churn==1)->dat25
dat25$N<-unclass(new_data %>% mutate(dec=ntile(avgmou,n=10)) %>% count(dec) %>% unname())[[2]]
dat25$churn_perc<-dat25$n/dat25$N
dat25$Greater_Than<-unclass(new_data %>% mutate(dec=ntile(avgmou,n=10)) %>%group_by(dec) %>%  summarise(min(avgmou)))[[2]]
dat25$Less_Than<-unclass(new_data %>% mutate(dec=ntile(avgmou,n=10)) %>% group_by(dec) %>% summarise(max(avgmou)))[[2]]
dat25$varname<-rep("avgmou",nrow(dat25))

#avg3qty() 10

new_data %>% mutate(dec=ntile(avg3qty,n=10)) %>% count(churn,dec) %>% filter(churn==1)->dat26
dat26$N<-unclass(new_data %>% mutate(dec=ntile(avg3qty,n=10)) %>% count(dec) %>% unname())[[2]]
dat26$churn_perc<-dat26$n/dat26$N
dat26$Greater_Than<-unclass(new_data %>% mutate(dec=ntile(avg3qty,n=10)) %>%group_by(dec) %>%  summarise(min(avg3qty)))[[2]]
dat26$Less_Than<-unclass(new_data %>% mutate(dec=ntile(avg3qty,n=10)) %>% group_by(dec) %>% summarise(max(avg3qty)))[[2]]
dat26$varname<-rep("avg3qty",nrow(dat26))

#avgqty() 10

new_data %>% mutate(dec=ntile(avgqty,n=10)) %>% count(churn,dec) %>% filter(churn==1)->dat27
dat27$N<-unclass(new_data %>% mutate(dec=ntile(avgqty,n=10)) %>% count(dec) %>% unname())[[2]]
dat27$churn_perc<-dat27$n/dat27$N
dat27$Greater_Than<-unclass(new_data %>% mutate(dec=ntile(avgqty,n=10)) %>%group_by(dec) %>%  summarise(min(avgqty)))[[2]]
dat27$Less_Than<-unclass(new_data %>% mutate(dec=ntile(avgqty,n=10)) %>% group_by(dec) %>% summarise(max(avgqty)))[[2]]
dat27$varname<-rep("avgqty",nrow(dat27))

#avg6mou() 10

new_data %>% mutate(dec=ntile(avg6mou,n=10)) %>% count(churn,dec) %>% filter(churn==1)->dat28
dat28$N<-unclass(new_data %>% mutate(dec=ntile(avg6mou,n=10)) %>% count(dec) %>% unname())[[2]]
dat28$churn_perc<-dat28$n/dat28$N
dat28$Greater_Than<-unclass(new_data %>% mutate(dec=ntile(avg6mou,n=10)) %>%group_by(dec) %>%  summarise(min(avg6mou)))[[2]]
dat28$Less_Than<-unclass(new_data %>% mutate(dec=ntile(avg6mou,n=10)) %>% group_by(dec) %>% summarise(max(avg6mou)))[[2]]
dat28$varname<-rep("avg6mou",nrow(dat28))

#avg6qty() 10

new_data %>% mutate(dec=ntile(avg6qty,n=10)) %>% count(churn,dec) %>% filter(churn==1)->dat29
dat29$N<-unclass(new_data %>% mutate(dec=ntile(avg6qty,n=10)) %>% count(dec) %>% unname())[[2]]
dat29$churn_perc<-dat29$n/dat29$N
dat29$Greater_Than<-unclass(new_data %>% mutate(dec=ntile(avg6qty,n=10)) %>%group_by(dec) %>%  summarise(min(avg6qty)))[[2]]
dat29$Less_Than<-unclass(new_data %>% mutate(dec=ntile(avg6qty,n=10)) %>% group_by(dec) %>% summarise(max(avg6qty)))[[2]]
dat29$varname<-rep("avg6qty",nrow(dat29))


#age1() 6

new_data %>% mutate(dec=ntile(age1,n=6)) %>% count(churn,dec) %>% filter(churn==1)->dat38
dat38$N<-unclass(new_data %>% mutate(dec=ntile(age1,n=6)) %>% count(dec) %>% unname())[[2]]
dat38$churn_perc<-dat38$n/dat38$N
dat38$Greater_Than<-unclass(new_data %>% mutate(dec=ntile(age1,n=6)) %>%group_by(dec) %>%  summarise(min(age1)))[[2]]
dat38$Less_Than<-unclass(new_data %>% mutate(dec=ntile(age1,n=6)) %>% group_by(dec) %>% summarise(max(age1)))[[2]]
dat38$varname<-rep("age1",nrow(dat38))

#age2() lesss than 4

new_data %>% mutate(dec=ntile(age2,n=4)) %>% count(churn,dec) %>% filter(churn==1)->dat39
dat39$varname<-rep("age2",nrow(dat39))


#models() 5 decile

new_data %>% mutate(dec=ntile(models,n=4)) %>% count(churn,dec) %>% filter(churn==1)->dat40

#hnd_price() 
new_data %>% mutate(dec=ntile(hnd_price,n=10)) %>% count(churn,dec) %>% filter(churn==1)->dat41
summary(new_data$hnd_price)


dat41$N<-unclass(new_data %>% mutate(dec=ntile(hnd_price,n=9)) %>% count(dec) %>% unname())[[2]]
dat41$churn_perc<-dat41$n/dat41$N
dat41$Greater_Than<-unclass(new_data %>% mutate(dec=ntile(age1,n=9)) %>%group_by(dec) %>%  summarise(min(hnd_price)))[[2]]
dat41$Less_Than<-unclass(new_data %>% mutate(dec=ntile(age1,n=9)) %>% group_by(dec) %>% summarise(max(hnd_price)))[[2]]
dat41$varname<-rep("hnd_price",nrow(dat41))

#actvsubs() 4 decile.

new_data %>% mutate(dec=ntile(actvsubs,n=4)) %>% count(churn,dec) %>% filter(churn==1)->dat42
dat42$varname<-rep("actvsubs",nrow(dat42))

#uniqsubs()

new_data %>% mutate(dec=ntile(uniqsubs,n=4)) %>% count(churn,dec) %>% filter(churn==1)->dat43
dat43$varname<-rep("uniqsubs",nrow(dat43))

#forgntvl()

new_data %>% mutate(dec=ntile(forgntvl,n=4)) %>% count(churn,dec) %>% filter(churn==1)->dat44
dat44$varname<-rep("forgntvl",nrow(dat44))

#opk_dat_Mean()

new_data %>% mutate(dec=ntile(opk_dat_Mean,n=4)) %>% count(churn,dec) %>% filter(churn==1)->dat45
dat45$varname<-rep("opk_dat_Mean",nrow(dat45))

#mtrcycle()

new_data %>% mutate(dec=ntile(mtrcycle,n=4)) %>% count(churn,dec) %>% filter(churn==1)->dat46
dat46$varname<-rep("mtrcycle",nrow(dat46))

#truck()

new_data %>% mutate(dec=ntile(truck,n=4)) %>% count(churn,dec) %>% filter(churn==1)->dat47
dat47$varname<-rep("truck",nrow(dat47))

#roam_Mean()

new_data %>% mutate(dec=ntile(roam_Mean,n=4)) %>% count(churn,dec) %>% filter(churn==1)->dat48
dat48$varname<-rep("roam_Mean",nrow(dat48))

#recv_sms_Mean()

new_data %>% mutate(dec=ntile(recv_sms_Mean,n=4)) %>% count(churn,dec) %>% filter(churn==1)->dat49
dat49$varname<-rep("recv_sms_Mean",nrow(dat49))

#blck_dat_Mean()


new_data %>% mutate(dec=ntile(blck_dat_Mean,n=4)) %>% count(churn,dec) %>% filter(churn==1)->dat50
dat50$varname<-rep("blck_dat_Mean",nrow(dat50))


#mou_pead_Mean()


new_data %>% mutate(dec=ntile(mou_pead_Mean,n=4)) %>% count(churn,dec) %>% filter(churn==1)->dat51
dat51$varname<-rep("mou_pead_Mean",nrow(dat51))

#da_Mean()   4


new_data %>% mutate(dec=ntile(da_Mean,n=4)) %>% count(churn,dec) %>% filter(churn==1)->dat55
dat55$N<-unclass(new_data %>% mutate(dec=ntile(da_Mean,n=4)) %>% count(dec) %>% unname())[[2]]
dat55$churn_perc<-dat55$n/dat55$N
dat55$Greater_Than<-unclass(new_data %>% mutate(dec=ntile(da_Mean,n=4)) %>%group_by(dec) %>%  summarise(min(da_Mean)))[[2]]
dat55$Less_Than<-unclass(new_data %>% mutate(dec=ntile(da_Mean,n=4)) %>% group_by(dec) %>% summarise(max(da_Mean)))[[2]]
dat55$varname<-rep("da_Mean",nrow(dat55))

#da_Range()

new_data %>% mutate(dec=ntile(da_Range,n=4)) %>% count(churn,dec) %>% filter(churn==1)->dat56
dat56$N<-unclass(new_data %>% mutate(dec=ntile(da_Range,n=4)) %>% count(dec) %>% unname())[[2]]
dat56$churn_perc<-dat56$n/dat56$N
dat56$Greater_Than<-unclass(new_data %>% mutate(dec=ntile(da_Range,n=4)) %>%group_by(dec) %>%  summarise(min(da_Range)))[[2]]
dat56$Less_Than<-unclass(new_data %>% mutate(dec=ntile(da_Range,n=4)) %>% group_by(dec) %>% summarise(max(da_Range)))[[2]]
dat56$varname<-rep("da_Range",nrow(dat56))

#datovr_Mean()

new_data %>% mutate(dec=ntile(datovr_Mean,n=4)) %>% count(churn,dec) %>% filter(churn==1)->dat57
dat57$varname<-rep("datovr_Mean",nrow(dat57))

#datovr_Range()


new_data %>% mutate(dec=ntile(datovr_Range,n=4)) %>% count(churn,dec) %>% filter(churn==1)->dat58
dat58$varname<-rep("datovr_Range",nrow(dat58))

#drop_dat_Mean()


new_data %>% mutate(dec=ntile(drop_dat_Mean,n=4)) %>% count(churn,dec) %>% filter(churn==1)->dat59
dat59$varname<-rep("drop_dat_Mean",nrow(dat59))

#drop_vce_Mean()


new_data %>% mutate(dec=ntile(drop_vce_Mean,n=10)) %>% count(churn,dec) %>% filter(churn==1)->dat60
dat60$N<-unclass(new_data %>% mutate(dec=ntile(drop_vce_Mean,n=10)) %>% count(dec) %>% unname())[[2]]
dat60$churn_perc<-dat60$n/dat60$N
dat60$Greater_Than<-unclass(new_data %>% mutate(dec=ntile(drop_vce_Mean,n=10)) %>%group_by(dec) %>%  summarise(min(drop_vce_Mean)))[[2]]
dat60$Less_Than<-unclass(new_data %>% mutate(dec=ntile(drop_vce_Mean,n=10)) %>% group_by(dec) %>% summarise(max(drop_vce_Mean)))[[2]]
dat60$varname<-rep("drop_vce_Mean",nrow(dat60))

#adjmou() 10

new_data %>% mutate(dec=ntile(adjmou,n=10)) %>% count(churn,dec) %>% filter(churn==1)->dat61
dat61$N<-unclass(new_data %>% mutate(dec=ntile(adjmou,n=10)) %>% count(dec) %>% unname())[[2]]
dat61$churn_perc<-dat61$n/dat61$N
dat61$Greater_Than<-unclass(new_data %>% mutate(dec=ntile(adjmou,n=10)) %>%group_by(dec) %>%  summarise(min(adjmou)))[[2]]
dat61$Less_Than<-unclass(new_data %>% mutate(dec=ntile(adjmou,n=10)) %>% group_by(dec) %>% summarise(max(adjmou)))[[2]]
dat61$varname<-rep("adjmou",nrow(dat61))

#totrev() 10

new_data %>% mutate(dec=ntile(totrev,n=10)) %>% count(churn,dec) %>% filter(churn==1)->dat62
dat62$N<-unclass(new_data %>% mutate(dec=ntile(totrev,n=10)) %>% count(dec) %>% unname())[[2]]
dat62$churn_perc<-dat62$n/dat62$N
dat62$Greater_Than<-unclass(new_data %>% mutate(dec=ntile(totrev,n=10)) %>%group_by(dec) %>%  summarise(min(totrev)))[[2]]
dat62$Less_Than<-unclass(new_data %>% mutate(dec=ntile(totrev,n=10)) %>% group_by(dec) %>% summarise(max(totrev)))[[2]]
dat62$varname<-rep("totrev",nrow(dat62))

#adjrev() 10

new_data %>% mutate(dec=ntile(adjrev,n=10)) %>% count(churn,dec) %>% filter(churn==1)->dat63
dat63$N<-unclass(new_data %>% mutate(dec=ntile(adjrev,n=10)) %>% count(dec) %>% unname())[[2]]
dat63$churn_perc<-dat63$n/dat63$N
dat63$Greater_Than<-unclass(new_data %>% mutate(dec=ntile(adjrev,n=10)) %>%group_by(dec) %>%  summarise(min(adjrev)))[[2]]
dat63$Less_Than<-unclass(new_data %>% mutate(dec=ntile(adjrev,n=10)) %>% group_by(dec) %>% summarise(max(adjrev)))[[2]]
dat63$varname<-rep("adjrev",nrow(dat63))

#avgrev() 10 

new_data %>% mutate(dec=ntile(avgrev,n=10)) %>% count(churn,dec) %>% filter(churn==1)->dat64
dat64$N<-unclass(new_data %>% mutate(dec=ntile(avgrev,n=10)) %>% count(dec) %>% unname())[[2]]
dat64$churn_perc<-dat64$n/dat64$N
dat64$Greater_Than<-unclass(new_data %>% mutate(dec=ntile(avgrev,n=10)) %>%group_by(dec) %>%  summarise(min(avgrev)))[[2]]
dat64$Less_Than<-unclass(new_data %>% mutate(dec=ntile(avgrev,n=10)) %>% group_by(dec) %>% summarise(max(avgrev)))[[2]]
dat64$varname<-rep("avgrev",nrow(dat64))

#comp_dat_Mean() 


new_data %>% mutate(dec=ntile(comp_dat_Mean,n=4)) %>% count(churn,dec) %>% filter(churn==1)->dat66
dat66$varname<-rep("drop_dat_Mean",nrow(dat66))

#plcd_dat_Mean()


new_data %>% mutate(dec=ntile(plcd_dat_Mean,n=4)) %>% count(churn,dec) %>% filter(churn==1)->dat67
dat67$varname<-rep("plcd_dat_Mean",nrow(dat67))

#Creating Derived Variable/Dummy.


new_data$comp_vce_dat_Mean<-new_data$comp_vce_Mean+new_data$comp_dat_Mean

#comp_vce_dat_Mean() 10


new_data %>% mutate(dec=ntile(comp_vce_dat_Mean,n=10)) %>% count(churn,dec) %>% filter(churn==1)->dat69
dat69$N<-unclass(new_data %>% mutate(dec=ntile(comp_vce_dat_Mean,n=10)) %>% count(dec) %>% unname())[[2]]
dat69$churn_perc<-dat69$n/dat69$N
dat69$Greater_Than<-unclass(new_data %>% mutate(dec=ntile(comp_vce_dat_Mean,n=10)) %>%group_by(dec) %>%  summarise(min(comp_vce_dat_Mean)))[[2]]
dat69$Less_Than<-unclass(new_data %>% mutate(dec=ntile(comp_vce_dat_Mean,n=10)) %>% group_by(dec) %>% summarise(max(comp_vce_dat_Mean)))[[2]]
dat69$varname<-rep("comp_vce_dat_Mean",nrow(dat69))

#Creating Derived variable


new_data$plcd_vce_dat_Mean<-new_data$plcd_vce_Mean+new_data$plcd_dat_Mean

#comp_vce_data_Mean() 10


new_data %>% mutate(dec=ntile(comp_vce_dat_Mean,n=10)) %>% count(churn,dec) %>% filter(churn==1)->dat70
dat70$N<-unclass(new_data %>% mutate(dec=ntile(comp_vce_dat_Mean,n=10)) %>% count(dec) %>% unname())[[2]]
dat70$churn_perc<-dat70$n/dat70$N
dat70$Greater_Than<-unclass(new_data %>% mutate(dec=ntile(comp_vce_dat_Mean,n=10)) %>%group_by(dec) %>%  summarise(min(comp_vce_dat_Mean)))[[2]]
dat70$Less_Than<-unclass(new_data %>% mutate(dec=ntile(comp_vce_dat_Mean,n=10)) %>% group_by(dec) %>% summarise(max(comp_vce_dat_Mean)))[[2]]
dat70$varname<-rep("comp_vce_dat_Mean",nrow(dat70))

#Creating new vatiable for qualitu, billing, adjustment, overage and churn_family


new_data$drp_block<-new_data$drop_blk_Mean/new_data$totcalls

#drp_block() 10               first remove missing values


summary(new_data)
index2<-which(is.na(new_data$drp_block))
new_data<-new_data[-index2,]
summary(new_data)

new_data %>% mutate(dec=ntile(drp_block,n=10)) %>% count(churn,dec) %>% filter(churn==1)->dat71
dat71$N<-unclass(new_data %>% mutate(dec=ntile(drp_block,n=10)) %>% count(dec) %>% unname())[[2]]
dat71$churn_perc<-dat71$n/dat71$N
dat71$Greater_Than<-unclass(new_data %>% mutate(dec=ntile(drp_block,n=10)) %>%group_by(dec) %>%  summarise(min(drp_block)))[[2]]
dat71$Less_Than<-unclass(new_data %>% mutate(dec=ntile(drp_block,n=10)) %>% group_by(dec) %>% summarise(max(drp_block)))[[2]]
dat71$varname<-rep("drp_block",nrow(dat71))

#over_age()

new_data$over_age<-new_data$ovrrev_Mean/new_data$rev_Mean
#Deal with missing values

summary(new_data)
index3<-which(is.na(new_data$over_age))
new_data<-new_data[-index3,]
summary(new_data)

new_data %>% mutate(dec=ntile(over_age,n=4)) %>% count(churn,dec) %>% filter(churn==1)->dat72
dat72$N<-unclass(new_data %>% mutate(dec=ntile(over_age,n=4)) %>% count(dec) %>% unname())[[2]]
dat72$churn_perc<-dat72$n/dat72$N
dat72$Greater_Than<-unclass(new_data %>% mutate(dec=ntile(over_age,n=4)) %>%group_by(dec) %>%  summarise(min(over_age)))[[2]]
dat72$Less_Than<-unclass(new_data %>% mutate(dec=ntile(over_age,n=4)) %>% group_by(dec) %>% summarise(max(over_age)))[[2]]
dat72$varname<-rep("over_age",nrow(dat72))

#Export the file in to csv format

new_data_final<-(rbind(dat01,dat02,dat03,dat04,dat05,dat06,dat07,dat08,dat09,dat10,
                       dat11,dat12,dat13,dat14,dat15,dat16,dat17,dat18,dat19,dat20,
                       dat21,dat22,dat23,dat24,dat25,dat26,dat26,dat27,dat28,dat29,
                       dat38,dat39,dat40,dat41,dat42,dat43,dat44,dat45,dat46,dat47,
                       dat50,dat51,dat55,dat56,dat48,dat49,dat50,dat51,dat55,dat56,
                       dat57,dat58,dat59,dat60,dat50,dat51,dat55,dat56,dat61,dat62,
                       dat63,dat64,dat66,dat67,dat69,dat70,dat71,dat72))
#  C:\Jig14947\T13
write.csv(new_data_final,"C:\\Jig14947\\T13\\Continuous_Varibles.csv",row.names = F)

new_data_final_try<-read.csv("C:\\Jig14947\\T13\\Continuous_Varibles.csv")
View(new_data_final_try)

#crclscod()  catagorical variable
new_data %>% count(churn,levels = crclscod) %>% filter(churn==1) -> dat30
dat30$N <- unclass(new_data %>% filter(crclscod %in% dat30$levels) %>% count(crclscod))[[2]]
dat30$churn_perc<-dat30$n/dat30$N
dat30$varname<-rep("crclscod",nrow(dat30))

#asl_flag()
new_data %>% count(churn,levels = asl_flag) %>% filter(churn==1) -> dat31
dat31$N <- unclass(new_data %>% filter(asl_flag %in% dat31$levels) %>% count(asl_flag))[[2]]
dat31$churn_perc<-dat31$n/dat31$N
dat31$varname<-rep("asl_flag",nrow(dat31))

#prizm_social_one()
new_data %>% count(churn,levels = prizm_social_one) %>% filter(churn==1) -> dat32
dat32$N <- unclass(new_data %>% filter(prizm_social_one %in% dat32$levels) %>% count(prizm_social_one))[[2]]
dat32$churn_perc<-dat32$n/dat32$N
dat32$varname<-rep("prizm_social_one",nrow(dat32))

#area()
new_data %>% count(churn,levels = area) %>% filter(churn==1) -> dat33
dat33$N <- unclass(new_data %>% filter(area %in% dat33$levels) %>% count(area))[[2]]
dat33$churn_perc<-dat33$n/dat33$N
dat33$varname<-rep("area",nrow(dat33))

#refurb_new()
new_data %>% count(churn,levels = refurb_new) %>% filter(churn==1) -> dat34
dat34$N <- unclass(new_data %>% filter(refurb_new %in% dat34$levels) %>% count(refurb_new))[[2]]
dat34$churn_perc<-dat34$n/dat34$N
dat34$varname<-rep("refurb_new",nrow(dat34))

#hnd_webcap()
new_data %>% count(churn,levels = hnd_webcap) %>% filter(churn==1) -> dat35
dat35$N <- unclass(new_data %>% filter(hnd_webcap %in% dat35$levels) %>% count(hnd_webcap))[[2]]
dat35$churn_perc<-dat35$n/dat35$N
dat35$varname<-rep("hnd_webcap",nrow(dat35))

#marital()
new_data %>% count(churn,levels = marital) %>% filter(churn==1) -> dat36
dat36$N <- unclass(new_data %>% filter(marital %in% dat36$levels) %>% count(marital))[[2]]
dat36$churn_perc<-dat36$n/dat36$N
dat36$varname<-rep("marital",nrow(dat36))

#ethnic()
new_data %>% count(churn,levels = ethnic) %>% filter(churn==1) -> dat37
dat37$N <- unclass(new_data %>% filter(ethnic %in% dat37$levels) %>% count(ethnic))[[2]]
dat37$churn_perc<-dat37$n/dat37$N
dat37$varname<-rep("ethnic",nrow(dat37))

#car_buy() 53
new_data %>% count(churn,levels = car_buy) %>% filter(churn==1) -> dat53
dat53$N <- unclass(new_data %>% filter(car_buy %in% dat53$levels) %>% count(car_buy))[[2]]
dat53$churn_perc<-dat53$n/dat53$N
dat53$varname<-rep("car_buy",nrow(dat53))

#csa()     54
new_data %>% count(churn,levels = csa) %>% filter(churn==1) -> dat54
dat54$N <- unclass(new_data %>% filter(csa %in% dat54$levels) %>% count(csa))[[2]]
dat54$churn_perc<-dat54$n/dat54$N
dat54$varname<-rep("csa",nrow(dat54))

#retcall() 68
new_data %>% count(churn,levels = retcall) %>% filter(churn==1) -> dat68
dat68$N <- unclass(new_data %>% filter(retcall %in% dat68$levels) %>% count(retcall))[[2]]
dat68$churn_perc<-dat68$n/dat68$N
dat68$varname<-rep("retcall",nrow(dat68))

#uniqsubs,forgntvl,age2,models,actvsubs,mtrcycle,truck
#39 40 42 43 44 46 47

new_data %>% count(churn,levels = age2) %>% filter(churn==1) -> dat39n
dat39n$N <- unclass(new_data %>% filter(age2 %in% dat39n$levels) %>% count(age2))[[2]]
dat39n$churn_perc<-dat39n$n/dat39n$N
dat39n$varname<-rep("age2",nrow(dat39n))


new_data %>% count(churn,levels = models) %>% filter(churn==1) -> dat40n
dat40n$N <- unclass(new_data %>% filter(models %in% dat40n$levels) %>% count(models))[[2]]
dat40n$churn_perc<-dat40n$n/dat40n$N
dat40n$varname<-rep("models",nrow(dat40n))

new_data %>% count(churn,levels = actvsubs) %>% filter(churn==1) -> dat42n
dat42n$N <- unclass(new_data %>% filter(actvsubs %in% dat42n$levels) %>% count(actvsubs))[[2]]
dat42n$churn_perc<-dat42n$n/dat42n$N
dat42n$varname<-rep("actvsubs",nrow(dat42n))

new_data %>% count(churn,levels = uniqsubs) %>% filter(churn==1) -> dat43n
dat43n$N <- unclass(new_data %>% filter(uniqsubs %in% dat43n$levels) %>% count(uniqsubs))[[2]]
dat43n$churn_perc<-dat43n$n/dat43n$N
dat43n$varname<-rep("uniqsubs",nrow(dat43n))

new_data %>% count(churn,levels = forgntvl) %>% filter(churn==1) -> dat44n
dat44n$N <- unclass(new_data %>% filter(forgntvl %in% dat44n$levels) %>% count(forgntvl))[[2]]
dat44n$churn_perc<-dat44n$n/dat44n$N
dat44n$varname<-rep("forgntvl",nrow(dat44n))

new_data %>% count(churn,levels = mtrcycle) %>% filter(churn==1) -> dat46n
dat46n$N <- unclass(new_data %>% filter(mtrcycle %in% dat46n$levels) %>% count(mtrcycle))[[2]]
dat46n$churn_perc<-dat46n$n/dat46n$N
dat46n$varname<-rep("mtrcycle",nrow(dat46n))

new_data %>% count(churn,levels = truck) %>% filter(churn==1) -> dat47n
dat47n$N <- unclass(new_data %>% filter(truck %in% dat47n$levels) %>% count(truck))[[2]]
dat47n$churn_perc<-dat47n$n/dat47n$N
dat47n$varname<-rep("truck",nrow(dat47n))

#all all labels
datcatfn<-rbind(dat30,dat31,dat32,dat33,dat34,dat35,dat36,dat37,dat53,
                dat54,dat68,dat39n,dat40n,dat42n,dat43n,dat44n,dat46n,dat47n)

datcatfn1<-rbind(dat30,dat31,dat32,dat33,dat34,dat35,dat36,dat37,dat53,dat54)

datcatfn2<-rbind(dat68,dat39n,dat40n,dat42n,dat43n,dat44n,dat46n,dat47n)

write.csv(datcatfn1,"C:\\Jig14947\\T13\\catagorical_variables1.csv",row.names = F)
write.csv(datcatfn2,"C:\\Jig14947\\T13\\catagorical_variables2.csv",row.names = F)
write.csv(datcatfn,"C:\\Jig14947\\T13\\catagorical_variables3.csv",row.names = F)

datcatfn111<-read.csv("C:\\Jig14947\\T13\\catagorical_variables1.csv")
View(datcatfn111)
datcatfn222<-read.csv("C:\\Jig14947\\T13\\catagorical_variables2.csv")
View(datcatfn222)
datcatfn333<-read.csv("C:\\Jig14947\\T13\\catagorical_variables3.csv")
View(datcatfn333)

#Removing continuous and catagorical variables
new_data<-new_data[,-c(13,14,17,22,23,45,49,51,57,58,59,66,67)]
new_data<-new_data[,-c(25,46)]

#Outlier treatment
library(outliers)
outlier(new_data)
list<-names(new_data)
list

#Removing categorical and factor variables
list<- list[-c(25:31,33,34,36:40,43,44,52,53)]
list

#Box_Plot
par("mar")
par(mar=c(1,1,1,1))

par(mfcol=c(4,10))
for(i in 1:length(list)){
  boxplot(new_data[,list[i]],main=list[i])
}

#Remove
for(i in 1:length(list)){
  x<-boxplot(new_data[,list[i]],main=list[i])
  out<-x$out
  index<-which(new_data[,list[i]] %in% x$out)
  new_data[index,list[i]]<-mean(new_data[,list[i]],na.rm = T)
  rm(x)
  rm(out)
}

#Check
for(i in 1:length(list)){
  boxplot(new_data[,list[i]],main=list[i])
}

dev.off()

#Remove the missing values and also check the missing values
colSums(is.na(new_data))
sum(is.na(new_data))
sum(is.na(new_data$change_mou))


ind1<-which(is.na(new_data$change_mou))
new_data<-new_data[-ind1,]

sum(is.na(new_data$area))
ind2<-which(is.na(new_data$area))
new_data<-new_data[-ind2,]

ind3<-which(is.na(new_data$marital))
new_data<-new_data[-ind3,]

ind4<-which(is.na(new_data$hnd_price))
new_data<-new_data[-ind4,]

#Missing value imputation
new_data$avg6mou[is.na(new_data$avg6mou)]<-median(new_data$avg6mou,na.rm = T)
new_data$avg6qty[is.na(new_data$avg6qty)]<-median(new_data$avg6qty,na.rm = T)

new_data$hnd_webcap_new<-ifelse(is.na(new_data$hnd_webcap),"Missing",as.character(new_data$hnd_webcap))
new_data$hnd_webcap_new<-as.factor(new_data$hnd_webcap_new)


new_data$prizm_social_one_new<-ifelse(is.na(new_data$prizm_social_one),"Missing",as.character(new_data$prizm_social_one))
new_data$prizm_social_one_new<-as.factor(new_data$prizm_social_one_new)
str(new_data$prizm_social_one_new)
summary(new_data$prizm_social_one_new)

new_data<-new_data[-c(26,29)]

summary(new_data)
sum(is.na(new_data))
View(new_data)

write.csv(new_data,"C:\\Jig14947\\T13\\new_data_fn.csv",row.names = F)


new_data_fn<-read.csv("C:\\Jig14947\\T13\\new_data_fn.csv")
dim(new_data_fn)
summarise(new_data_fn)
str(new_data_fn)
sum(is.na(new_data_fn))
View(new_data_fn)

#Dummy variable and also convert in to factor 
#forgntvl,mtrcycle,truck,churn,age1,age2,models,actvsubs,uniqsubs

#age1()
new_data_fn$age1_new<-ifelse(new_data_fn$age1==0,"Unknow",ifelse(new_data_fn$age1<=35,
                                                                 "Young",ifelse(new_data_fn$age1>35 & new_data_fn$age1<=50,"Middle_Age","Old")))
new_data_fn$age1_new<-as.factor(new_data_fn$age1_new)
summary(new_data_fn$age1_new)
#Now remove age1
summary(new_data_fn[30])
new_data_fn<-new_data_fn[,-30]

#age2()
new_data_fn$age2_new<-ifelse(new_data_fn$age2==0,"Unknow",ifelse(new_data_fn$age2<=35,
                                                                 "Young",ifelse(new_data_fn$age2>35 & new_data_fn$age2<=50,"Middle_Age","Old")))
new_data_fn$age2_new<-as.factor(new_data_fn$age2_new)
summary(new_data_fn$age2_new)
#Now remove age1
summary(new_data_fn[30])
new_data_fn<-new_data_fn[,-30]

#hnd_price()
summary(new_data_fn$hnd_price)
new_data_fn$hnd_price_new<-ifelse(new_data_fn$hnd_price<=59.99,"Low_Price_hnd",ifelse(new_data_fn$hnd_price>59.99
                                                                                      & new_data_fn$hnd_price<=103.90, "Medium_Price_hnd",ifelse(new_data_fn$hnd_price>103.90 
                                                                                                                                                 & new_data_fn$hnd_price<=150.00,"High_Price",
                                                                                                                                                 "very_hingh_price_hnd")))
new_data_fn$hnd_price_new<-as.factor((new_data_fn$hnd_price_new))
summary(new_data_fn$hnd_price_new)

#remove
new_data_fn<-new_data_fn[-31]

#models()
new_data_fn$models<-as.factor(new_data_fn$models)
summary(new_data_fn$models)

#actvsubs()
new_data_fn$actvsubs<-as.factor(new_data_fn$actvsubs)
summary(new_data_fn$actvsubs)

#uniqsubs()
new_data_fn$uniqsubs<-as.factor(new_data_fn$uniqsubs)
summary(new_data_fn$uniqsubs)

#forgntvl()
new_data_fn$forgntvl<-as.factor(new_data_fn$forgntvl)
summary(new_data_fn$forgntvl)
str(new_data_fn$forgntvl)

#mtrcycle()
new_data_fn$mtrcycle<-as.factor(new_data_fn$mtrcycle)
summary(new_data_fn$mtrcycle)

#actvsubs()

new_data_fn$truck<-as.factor(new_data_fn$truck)
summary(new_data_fn$truck)


#Build_Model
set.seed(1000)

index<-sample(nrow(new_data_fn),0.70*nrow(new_data_fn),replace = F)
train<-new_data_fn[index,]
test<-new_data_fn[-index,]

summary(train)
summary(test)
sum(is.na(train))
sum(is.na(test))

#Churn_Rate
table(train$churn)/length(train$churn)
table(test$churn)/length(test$churn)

#Apply Logistic regression because  target (Churn) variable is binary variable
summary(new_data_fn$churn)
summary(train[47])  #Custmer_ID
model1<-glm(churn~.,data = train[,-47],family = "binomial")
summary(model1)

#step(model1,direction = "both")

#Creating dummy

train$asl_flag_1<-ifelse(train$asl_flag=="Y",1,0)
test$asl_flag_1<-ifelse(test$asl_flag=="Y",1,0)

train$area_california_North<-ifelse(train$area=="CALIFORNIA NORTH AREA",1,0)
test$area_california_North<-ifelse(test$area=="CALIFORNIA NORTH AREA",1,0)

train$area_new_england<-ifelse(train$area=="NEW ENGLAND AREA",1,0)
test$area_new_england<-ifelse(test$area=="NEW ENGLAND AREA",1,0)

train$area_ROCKY_MOUNTATIN<-ifelse(train$area=="NORTHWEST/ROCKY MOUNTAIN AREA",1,0)
test$area_ROCKY_MOUNTATIN<-ifelse(test$area=="NORTHWEST/ROCKY MOUNTAIN AREA",1,0)

train$area_SOUTH_FLORIDA<-ifelse(train$area=="SOUTH FLORIDA AREA",1,0)
test$area_SOUTH_FLORIDA<-ifelse(test$area=="SOUTH FLORIDA AREA",1,0)

train$area_TENNESSEE<-ifelse(train$area=="TENNESSEE AREA",1,0)
test$area_TENNESSEE<-ifelse(test$area=="TENNESSEE AREA",1,0)

train$refurb<-ifelse(train$refurb_new=="R",1,0)
test$refurb<-ifelse(test$refurb_new=="R",1,0)

train$marital_S<-ifelse(train$marital=="S",1,0)
test$marital_S<-ifelse(test$marital=="S",1,0)

train$ethnic_C<-ifelse(train$ethnic=="C",1,0)
test$ethnic_C<-ifelse(test$ethnic=="C",1,0)

train$ethnic_G<-ifelse(train$ethnic=="G",1,0)
test$ethnic_G<-ifelse(test$ethnic=="G",1,0)

train$ethnic_H<-ifelse(train$ethnic=="H",1,0)
test$ethnic_H<-ifelse(test$ethnic=="H",1,0)

train$ethnic_N<-ifelse(train$ethnic=="N",1,0)
test$ethnic_N<-ifelse(test$ethnic=="N",1,0)

train$ethnic_C<-ifelse(train$ethnic=="C",1,0)
test$ethnic_C<-ifelse(test$ethnic=="C",1,0)

train$marital_P<-ifelse(train$marital=="P",1,0)
test$marital_P<-ifelse(test$marital=="P",1,0)

train$ethnic_S<-ifelse(train$ethnic=="S",1,0)
test$ethnic_S<-ifelse(test$ethnic=="S",1,0)

train$ethnic_U<-ifelse(train$ethnic=="U",1,0)
test$ethnic_U<-ifelse(test$ethnic=="U",1,0)

train$ethnic_Z<-ifelse(train$ethnic=="Z",1,0)
test$ethnic_Z<-ifelse(test$ethnic=="Z",1,0)

train$uniqsubs_2<-ifelse(train$uniqsubs=="2",1,0)
test$uniqsubs_2<-ifelse(test$uniqsubs=="2",1,0)


train$uniqsubs_3<-ifelse(train$uniqsubs=="3",1,0)
test$uniqsubs_3<-ifelse(test$uniqsubs=="3",1,0)

train$uniqsubs_4<-ifelse(train$uniqsubs=="4",1,0)
test$uniqsubs_4<-ifelse(test$uniqsubs=="4",1,0)

train$uniqsubs_5<-ifelse(train$uniqsubs=="5",1,0)
test$uniqsubs_5<-ifelse(test$uniqsubs=="5",1,0)

train$uniqsubs_7<-ifelse(train$uniqsubs=="7",1,0)
test$uniqsubs_7<-ifelse(test$uniqsubs=="7",1,0)

train$uniqsubs_8<-ifelse(train$uniqsubs=="8",1,0)
test$uniqsubs_8<-ifelse(test$uniqsubs=="8",1,0)

train$hnd_webcap_new_WC<-ifelse(train$hnd_webcap_new=="WC",1,0)
test$hnd_webcap_new_WC<-ifelse(test$hnd_webcap_new=="WC",1,0)


train$hnd_webcap_new_WCMB<-ifelse(train$hnd_webcap_new=="WCMB",1,0)
test$hnd_webcap_new_WCMB<-ifelse(test$hnd_webcap_new=="WCMB",1,0)

train$prizm_social_one_new_R<-ifelse(train$prizm_social_one_new=="R",1,0)
test$prizm_social_one_new_R<-ifelse(test$prizm_social_one_new=="R",1,0)

train$prizm_social_one_new_T<-ifelse(train$prizm_social_one_new=="T",1,0)
test$prizm_social_one_new_T<-ifelse(test$prizm_social_one_new=="T",1,0)

train$age1_new_unknow<-ifelse(train$age1_new=="Unknown",1,0)
test$age1_new_unknow<-ifelse(test$age1_new=="Unknown",1,0)


train$age1_new_old<-ifelse(train$age1_new=="Old",1,0)
test$age1_new_old<-ifelse(test$age1_new=="Old",1,0)

train$age1_new_Young<-ifelse(train$age1_new=="Young",1,0)
test$age1_new_Young<-ifelse(test$age1_new=="Young",1,0)


train$age2_new_old<-ifelse(train$age2_new=="Old",1,0)
test$age2_new_old<-ifelse(test$age2_new=="Old",1,0)

train$hnd_price_new_vhph<-ifelse(train$hnd_price_new=="Very high price hnd",1,0)
test$hnd_price_new_vhph<-ifelse(test$hnd_price_new=="Very high price hnd",1,0)

train$hnd_price_new_lph<-ifelse(train$hnd_price_new=="Low price hnd",1,0)
test$hnd_price_new_lph<-ifelse(test$hnd_price_new=="Low price hnd",1,0)


names(train)

# churn ~ mou_Mean + totmrc_Mean + rev_Range + mou_Range + change_mou + 
#   drop_blk_Mean + drop_vce_Range + owylis_vce_Range + mou_opkv_Range + 
#   months + totcalls + eqpdays + iwylis_vce_Mean + callwait_Range + 
#   adjqty + ovrrev_Mean + rev_Mean + ovrmou_Mean + avg3mou + 
#   avgmou + avg3qty + avgqty + avg6mou + avg6qty + asl_flag + 
#   area + refurb_new + marital + ethnic + models + actvsubs + 
#   uniqsubs + forgntvl + mtrcycle + truck + roam_Mean + blck_dat_Mean + 
#   car_buy + da_Mean + da_Range + drop_vce_Mean + adjmou + totrev + 
#   adjrev + avgrev + retcall + comp_vce_dat_Mean + plcd_vce_dat_Mean + 
#   drp_block + over_age + hnd_webcap_new + prizm_social_one_new + 
#   age1_new + age2_new + hnd_price_new


#owylis_vce_Range

# model2<-glm(churn~.,data = train[,-47],family = "binomial")
# summary(model2)

#step(model2,direction = "both")

# churn ~ mou_Mean + totmrc_Mean+ rev_Range + mou_Range+ change_mou + 
#   drop_blk_Mean +drop_vce_Range +mou_opkv_Range +months+eqpdays + iwylis_vce_Mean +
#    ovrrev_Mean + rev_Mean +avgmou + avg3qty + avgqty +
#    asl_flag_1 + area_california_North +
#    + area_ROCKY_MOUNTATIN + area_SOUTH_FLORIDA + 
#    refurb + marital_S + ethnic_C + ethnic_G + 
#     ethnic_H + ethnic_N + marital_P + ethnic_S + ethnic_U + ethnic_Z + 
# uniqsubs_2 + uniqsubs_3 + uniqsubs_4 + uniqsubs_5 + uniqsubs_7 + 
#    hnd_webcap_new_WC + hnd_webcap_new_WCMB + prizm_social_one_new_R +
#    prizm_social_one_new_T + age1_new_unknow + age1_new_Young + age2_new_old +
#    hnd_price_new_vhph + hnd_price_new_lph



model2<-glm(churn ~ mou_Mean + totmrc_Mean+ rev_Range + mou_Range+ change_mou +
              drop_blk_Mean +drop_vce_Range +mou_opkv_Range +months+eqpdays + 
              iwylis_vce_Mean +
              ovrrev_Mean + rev_Mean +avgmou + avg3qty + avgqty +
              asl_flag_1 + area_california_North
              + area_ROCKY_MOUNTATIN + area_SOUTH_FLORIDA +
              refurb + marital_S + ethnic_C + ethnic_G +
              ethnic_H + ethnic_N + marital_P + ethnic_S + ethnic_U + ethnic_Z +
              uniqsubs_2 + uniqsubs_3 + uniqsubs_4 + uniqsubs_5 + uniqsubs_7 +
              roam_Mean+totrev+comp_vce_dat_Mean+  retcall+over_age+hnd_webcap_new_WC + hnd_webcap_new_WCMB + prizm_social_one_new_R +
              prizm_social_one_new_T + age1_new_unknow + age1_new_Young + age2_new_old +
              hnd_price_new_vhph + hnd_price_new_lph,data=train,family = "binomial")
summary(model2)


# model3<-glm(churn~.,data = train,family = "binomial")
# summary(model3)

model3<-glm(churn ~ mou_Mean + totmrc_Mean+ mou_Range+ change_mou +
              drop_blk_Mean +drop_vce_Range +mou_opkv_Range +months+eqpdays + 
              iwylis_vce_Mean+avgmou + avg3qty + avgqty +
              asl_flag_1 + area_california_North
            + area_ROCKY_MOUNTATIN + area_SOUTH_FLORIDA +
              refurb + marital_S + ethnic_C + ethnic_G +
              ethnic_H + ethnic_N + ethnic_S + ethnic_U + ethnic_Z +
              uniqsubs_2 + uniqsubs_3 + uniqsubs_4 +  uniqsubs_7 +
              totrev+comp_vce_dat_Mean+  retcall+over_age+hnd_webcap_new_WC + hnd_webcap_new_WCMB + prizm_social_one_new_R +
              prizm_social_one_new_T + age1_new_unknow + age1_new_Young + age2_new_old +
              hnd_price_new_vhph + hnd_price_new_lph,data=train,family = "binomial")
summary(model3)


#check multicolineatity
pred<-predict(model3,type = "response",newdata = test)
head(pred)

#cut-off
table(new_data_fn$churn)/length(new_data_fn$churn)
pred1<-ifelse(pred>=0.2388286,1,0)
table(pred1)

#kappa matrix
library(irr)
kappa2(data.frame(test$churn,pred1))
#kappa     0.135

#confusion matrix
library(caret)
confusionMatrix(pred1,test$churn,positive = "1")

# Confusion Matrix and Statistics
# 
# Reference
# Prediction    0    1
# 0 8652 1882
# 1 6019 2675
# 
# Accuracy : 0.5891          
# 95% CI : (0.5821, 0.5961)
# No Information Rate : 0.763           
# P-Value [Acc > NIR] : 1               
# 
# Kappa : 0.1346          
# Mcnemar's Test P-Value : <2e-16          
#                                           
#             Sensitivity : 0.5870          
#             Specificity : 0.5897          
#          Pos Pred Value : 0.3077          
#          Neg Pred Value : 0.8213          
#              Prevalence : 0.2370          
#          Detection Rate : 0.1391          
#    Detection Prevalence : 0.4522          
#       Balanced Accuracy : 0.5884          
#                                           
#        'Positive' Class : 1 

table(test$churn)

#ROCR curve
library(ROCR)
pred2<-prediction(pred,test$churn)
pref<-performance(pred2,"tpr","fpr")
plot(pref,col="black")

abline(0,1,lty=8,col="blue")

#AUC Curve
auc<-performance(pred2,"auc")
auc<-unlist(slot(auc,"y.values"))
auc

#0.6212828

#gain chart
library(gains)
gains(test$churn,predict(model3,type = "response",newdata = test),groups = 10)
test$prob<-predict(model3,type = "response",newdata = test)
quantile(test$prob,prob = c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))


# 1. What are the top five factors driving likelihood of churn at Mobicom?

options(scipen=999)
head(sort(model3$coefficients,decreasing = T),50)

# Ans:--
#   
#          retcall             uniqsubs_7               over_age 
#          0.75531398491 0.71550206051          0.68775540571 
#             uniqsubs_4                 refurb     area_SOUTH_FLORIDA 
#          0.29360680474          0.28165759655 


#Q4: ---
# 
# What would be your recommendation on how to use this churn model for prioritisation of customers for a
# proactive retention campaigns in the future?

pred2<-predict(model3,type = "response",newdata = test)
pred2<-ifelse(pred2>=0.3513250,1,0)
targeted_seg<-test[test$prob > 0.3523250 & test$prob<=0.6971524 & test$churn=="1","Customer_id"]
targeted_seg<-data.frame(targeted_seg)
nrow(targeted_seg)
write.csv(targeted_seg,"C:\\Jig14947\\T13\\targeted.csv",row.names=F)
targeted<-read.csv("C:\\Jig14947\\T13\\targeted.csv")
View(targeted)



# Q5:--
#   
#   What would be the target segments for proactive retention campaigns? Falling ARPU forecast is also a
# concern and therefore, Mobicom would like to save their high revenue customers besides managing
# churn. Given a budget constraint of a contact list of 20% of the subscriber pool, which subscribers should
# prioritized if "revenue saves" is also a priority besides controlling churn. In other words, controlling churn
# is the primary objective and revenue saves is the secondary objective.

pred3<-predict(model3,type="response",newdata = test)
test$prob<-predict(model3,type = "response",newdata = test)
quantile(test$prob,prob = c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))

pred4<-ifelse(pred3<=0.2308856,"Low_Churn_Prob",ifelse(pred3>0.2308856 &  pred3<=0.2750276,"Medium_Churn_Prob","High_Churn_Prob"))



#Revenue levels
quantile(test$prob,prob = c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))
test$rev_lvl<-ifelse(test$totrev>799.630 &  test$totrev<=1034.680,"medium_rev","high_rev")
table(pred4,test$rev_lvl)

#Churn_Prob_Level
test$prob_lvl<-ifelse(pred3<=0.2308856,"Low_Churn_Prob",ifelse(pred3>0.2308856 & pred3<=0.2750276,"Medium_Churn_Prob","High_Churn_Prob"))
table(pred4,test$prob_lvl)

#Ectracting the customer segment
targeted_seg1<-test[test$prob_lvl=="High_Churn_Prob" &test$rev_lvl =="high_rev","Customer_ID"]

targeted_seg1<-data.frame(targeted_seg1)

write.csv(targeted_seg1,"C:\\Jig14947\\T13\\targeted_seg1.csv",row.names=F)
# final<-read.csv("C:\\Jig14947\\T13\\targeted_seg1.csv")
# dim(final)
# head(final)
