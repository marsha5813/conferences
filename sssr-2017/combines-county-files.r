# Reads tweets from multiple .csv files and summarizes by county fips

# intialize
# setwd("C:/Users/jmarshall2/Documents/twitter")
setwd("E:/OneDrive/OneDrive - purdue.edu/wd/twitter-sentiment-and-religious-geography")

# Read in data and append
filelist = list.files(pattern="*counties.csv")

# append
dframes = lapply(filelist, read.csv)
data = do.call(rbind, dframes)
rm(dframes)

# collapse to county level
data = aggregate(data, by=list(data$Group.1), FUN = sum)
data$fips = data$Group.1
data = subset(data, select=c(fips,count,isharassment))
data$pctharassment = (data$isharassment/data$count)*100

# Make fips a string variable with five digits (prepend a zero to four-digit numbers)
data$fips_str = formatC(data$fips, width = 5, format = "d", flag = "0")

# Add in RCMS data
library(xlsx)
rcms = read.xlsx("U.S. Religion Census Religious Congregations and Membership Study, 2010 (County File).XLSX", 1)

# subset
rcms = subset(rcms, select = c(TOTCNG,TOTADH,TOTRATE,EVANCNG,EVANADH,EVANRATE,BPRTCNG,BPRTADH,BPRTRATE,MPRTCNG,MPRTADH,MPRTRATE,CATHCNG,CATHADH,CATHRATE,POP2010,STNAME,STCODE,FIPS))
rcms$fips = rcms$FIPS

# merge
data_merged = merge(data,rcms, by = "fips")

# write to csv
write.csv(data_merged, file="countydata.csv")






