# Visit the NHANES website: http://wwwn.cdc.gov/nchs/nhanes/search/nhanes_continuous.aspx
# and right click on the link to the data you want, copy and paste the URL here:
URL<-"ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/nhanes/dxx/dxx_c.xpt"
download.file(URL,destfile="CDC",method="curl") # <-- On a PC you want to delete "method="curl"
library(Hmisc) #<-- need to load package for SAS files .xpt
mydata <- sasxport.get("CDC")
#head(mydata) #<-- Uncomment this to see the first six lines of your file
#str(mydata) #<-- Uncomment this to see the structre of your file
#summary(mydata) #<-- Uncomment this to see a summary of your file
# Descriptions of the variables are at (example): http://www.cdc.gov/nchs/data/nhanes/dxa/dxx_c.pdf
means<-aggregate(.~seqn,FUN=mean,data=mydata) #<-- Each subject had five measurements, find the means
# Load the demographic data for variables like age, etc., matched by "seqn":
URL2<-"http://wwwn.cdc.gov/nchs/nhanes/2003-2004/DEMO_C.XPT" 
download.file(URL2,destfile="CDC2",method="curl") #<-- No curl for PC
mydata2 <- sasxport.get("CDC2")
# Descriptions of the variables are at (ex): http://wwwn.cdc.gov/nchs/nhanes/search/nhanes03_04.aspx
URL3<-"http://wwwn.cdc.gov/nchs/nhanes/2003-2004/BMX_C.XPT"
download.file(URL3,destfile="CDC3",method="curl")
mydata3 <- sasxport.get("CDC3")
heights<-mydata3[,c(1,15)] #<-- choose the subject id and height
ages<-mydata2[,c(1,4,5,6)] #<-- choose the subject id, age in months, age in years, and gender
masses<-means[,c(1,105,104,102)] #<-- choose the subject id, bodyweight, fat mass, and lean mass
all_data<-merge(masses,ages,by="seqn") #<-- Merge the set by subject id
all_data<-merge(all_data,heights,by="seqn")
all_data<-all_data[which(all_data$ridageyr<19),] #<-- choose children
male_data<-all_data[which(all_data$riagendr==1),] #<-- choose males
female_data<-all_data[which(all_data$riagendr==2),] #<-- choose females
library(xlsx)
write.xlsx(male_data,file="./maleCDCdataMo.xlsx") #<-- where do you want the file?
write.xlsx(female_data,file="./femaleCDCdataMo.xlsx") 
