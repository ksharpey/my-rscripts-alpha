library(dplyr)
library(scales)
library(lubridate)
setwd("~/Desktop/Proj_defaulters/data/Country v2 data")
getwd()
SA_Data = read.csv("South_Africa.csv")
formsFlat20 = read.csv("forms_full_flat_20k.csv")
#TestComment

#Unique
v1 = SA_Data$form...case...update...client_status
unique(v1)
length(v1[v1 == "in_acfu"])
# 21 - SOUTH AFRICA
# 2 - Michael M
# 1 - WC Community training facility
# 17 - Site B
length(v1[v1 == "due"])
length(v1[v1 == ""])

# how many form...case...update...in_acfu=="yes" - 21 in SA.csv, 329 in formsFullFlat

# how to count the number of value X in column Y
length(formsFlat20$form....name[formsFlat20$form....name == "AN Appointment Diary"])

head(na.omit(SA_Data$form...acfu...acfu_round))
v1 = SA_Data$form...acfu...acfu_round
length(v1[SA_Data$form...acfu...acfu_round != ""])
v1 = SA_Data$form...case...update...acfu_round #5555
length(v1[SA_Data$form...case...update...acfu_round == "NA"]) #423
length(v1[SA_Data$form...case...update...acfu_round == "0"]) #5534
length(v1[SA_Data$form...case...update...acfu_round == "1"]) #443 = 9%


# using tidyverse
v_forms_tb <- select(formsFlat20,formsFlat20$form...summary...tb_test_result)
unique(v_forms_tb)
# summary:
table(v_an_all$form...an2...save_to_case...an2_status)

# filter all non-blank rows of column X, then return only that row
v1 = filter(SA_Data,form...case...update...an2_done_date != "")
v1 = select(v1,form...case...update...an2_done_date)
  #same in 1 line
v1 = filter(SA_Data,
            form...status...client_type== "PN"
            )  %>% 
  select(
    form...eighteen_twentyfour_month_infant_test...save_to_case...eighteen_twentyfour_month_infant_test_status)

 #PN cascade
v_pn_all = 
  filter(SA_Data,form...status...client_type== "PN",
         form...case...update...hiv_status =="known_positive" | form...case...update...hiv_status =="tested_positive") %>% 
         #form...client_info...facility =="Imbalenhle CHC"
  select(
    X__retrieved_case_ids, form...case...update...registration_date, 
    form...case...update...infant_dob,
    form...client_info...facility,
    form...case...update...hiv_status,
    form...case...update...six_eight_week_pcr_status,
    form...eighteen_twentyfour_month_infant_test...save_to_case...eighteen_twentyfour_month_infant_test_status
  )

# PN stats
length(v_pn_all$form...case...update...hiv_status)
table(v_pn_all$form...case...update...hiv_status)
table(v_pn_all$form...case...update...six_eight_week_pcr_status)
table(v_pn_all$form...eighteen_twentyfour_month_infant_test...save_to_case...eighteen_twentyfour_month_infant_test_status)
rm(v_pn_all)


  #AN cascade dates
v_an_all = 
  filter(SA_Data,form...an2...an2_due_date_entered !="") %>% 
      select(X__retrieved_case_ids,form...case...update...an2_done_date,form...case...update...an3_done_date,form...case...update...an4_done_date)
  #AN cascade dates AND STATUS column AND edd
v_an_all = 
  filter(SA_Data,form...an2...an2_due_date_entered !="") %>% 
  select(
    X__retrieved_case_ids, form...case...update...registration_date, form...case...update...edd,
    form...case...update...an2_done_date,form...case...update...an3_done_date,form...case...update...an4_done_date,
    form...an2...save_to_case...an2_status,form...an3...save_to_case...an3_status,form...an4...save_to_case...an4_status     
         )
#AN cascade dates AND STATUS column AND edd - by HIV status
v_an_all = 
  filter(SA_Data,form...an2...an2_due_date_entered !="",
         form...case...update...hiv_status =="known_positive" | form...case...update...hiv_status =="tested_positive",
         form...client_info...facility =="Imbalenhle CHC"
         ) %>% 
  select(
    X__retrieved_case_ids, form...case...update...registration_date, form...case...update...edd,
    form...case...update...hiv_status,
    form...case...update...an2_done_date,form...case...update...an3_done_date,form...case...update...an4_done_date,
    form...an2...save_to_case...an2_status,form...an3...save_to_case...an3_status,form...an4...save_to_case...an4_status     
  )


#Facilities stats
facilities = table(SA_Data$form...client_info...facility)
facilities = as.data.frame(facilities)
facilities = filter(facilities, Freq>5)
    %>%
    select(form...client_info...facility)
facilities = table(SA_Data$form...client_info...facility)


#Mentor mother notes analysis
vNotes = select(SA_Data,form...mentor_mother_notes)  %>% 
  filter(form...mentor_mother_notes!="")
vNotes = select(SA_Data,form...mentor_mother_notes,form...meta...timeStart,form...status...client_type)  %>% 
  filter(form...mentor_mother_notes!="") %>%
  group_by(form...status...client_type)

#List words here
"Trans visit east cape retest reffer refer default baby died cd"

word = "moved"
match = grepl(word, ignore.case = TRUE, vNotes$form...mentor_mother_notes)
num = length(match[match!=FALSE])
#print(word);print(" = "); print(num); pct = num/21000; paste(pct)
paste(word, '=',num, ' ', percent(num/21000))

#text indexing
#install.packages("quanteda", dependencies = T), http://www.mjdenny.com/Text_Processing_In_R.html


#-------TRUE defaulting script -----------
# based on last seen date
last_seen = Sys.Date()-as.Date(SA_Data$form...case....date_modified[1])
#____ exract raw data
tLast = select(SA_Data,
                        form...case....case_id,
                        form...case...update...national_id, 
                        form...case...update...m2m_id, 
                        form...case...update...registration_date,
                        form...case....date_modified,
                        form...acfu...acfu_round)

#____ add engineered columns
tLast$Last_Date <- as.Date(tLast$form...case....date_modified);
tLast$Diff_Today <- as.numeric(Sys.Date()-tLast$Last_Date);
tLast$Year <-year(tLast$form...case....date_modified)
tLast$Reg_Date <- as.Date(tLast$form...case...update...registration_date)
tLast$Days_Since_created <- as.numeric(Sys.Date()-tLast$Reg_Date);

fivenum(tLast$Diff_Today)
hist(tLast$Diff_Today,breaks=9)

export_date <- as.Date("2017-06-12")
paste("the data was exported", today-export_date, " days ago");
tLast$Diff_Export <- as.numeric(export_date-tLast$Last_Date);

#By year
tLast2 <- filter(tLast,Year>2015)
tLast2 <- filter(tLast2,Diff_Export>=0)
fivenum(tLast2$Diff_Export)

#stats
fivenum(tLast2$Diff_Export)
#hist(tLast2$Diff_Export,breaks=c(0,seq(30,180, 30)))##10);
x<-tLast2$Diff_Export
h<-hist(x, breaks=20, col="red", xlab="Days Since Last Visit", 
        main="Patients Seen since 2016", #Seen in 2017", 
        ) 
xfit<-seq(min(x),max(x),length=40) 
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="blue", lwd=2)

##Tests
daysSpread <- as.data.frame(table(tLast$Days))
Day_freq <- table(tLast$Days)
select(tLast,form...case....case_id,
       form...case...update...national_id, 
       form...case...update...m2m_id, )  %>% filter(tLast$form...case....case_id =='f492998b-b2c3-48ec-bde2-eebeb229fe72')


#---------------- testing day of the week --------------

alldays <- weekdays(tLast$Last_Date)
daystats <- table(alldays)
daystats <- as.data.frame(daystats)
# was to clean - daystats$order <- c(1,2,3,4,5,6,7)
daystats$percent <- daystats$Freq/length(alldays)
#----------------- testing day of month --------------
tLast$Last_Date[2]
print(format(as.Date(tLast$Last_Date[2],format="%Y-%m-%d"), "%d"))
alldays <- format(as.Date(tLast$Last_Date,format="%Y-%m-%d"), "%")
daystats <- table(alldays)
daystats <- as.data.frame(daystats)
#----------------- day of year ---------------------
daystats <- table(tLast$Last_Date)

daystats <- as.data.frame(daystats)
write.table(daystats,"Day of Year data.csv", sep = ",")

#--- close stats
View(SA_Data$form...close_reason_ltfu)

daystats <- table(SA_Data$form...hiv_status)
daystats <- as.data.frame(daystats)
daystats$percent <- daystats$Freq/length(SA_Data$form...hiv_status)
View(daystats)