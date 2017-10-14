# Classifies tweets as harassment (yes/no) using an ensemble classifier

# intialize
setwd("C:/Users/jmarshall2/Documents/twitter") # Work
# setwd("E:/OneDrive/OneDrive - purdue.edu/wd/twitter-sentiment-and-religious-geography") # Home
.libPaths("C:/Users/jmarshall2/Documents/libraries/r-3-4-win") # Work
# .libPaths("E:/OneDrive/OneDrive - purdue.edu/wd/libraries/r/3.4_win") # Home
source("functions.r")

# libraries
library(readr)
library(quanteda)
library(RTextTools)

# Load in the pre-labeled data
labeled_data = read_csv("harassment_trainingset.csv")

# Construct the dfm
tweetsdfm = create_matrix(labeled_data$text, 
                          language = "english",
                          removeNumbers=T, 
                          removePunctuation=T, 
                          toLower = T)

# build the container
container = create_container(tweetsdfm, labeled_data$harassment,
                             trainSize = 1:800,
                             testSize = 801:1000,
                             virgin = FALSE)

# Train models
# Possible algos include: "SVM", "SLDA", "BOOSTING", "BAGGING", "RF", "GLMNET", "TREE", "NNET", "MAXENT"
models = train_models(container, algorithms = c("SVM","MAXENT","RF","TREE"))

# Classify the testing set
results = classify_models(container, models)

# Summarize results 
analytics = create_analytics(container, results)
summary(analytics)
create_ensembleSummary(analytics@document_summary)

# Write out a csv
write.csv(analytics@document_summary, "DocumentSummary10-10-17.csv")

################ Classify the out-of-sample data

# Read in combined data
tweets = read_csv("htweets_combined.csv")

# Merge the pre-labeled data back in, along with the 1,000 true labels
tweets = merge(tweets,labeled_data, by="text", all.x = TRUE) 
tweets = subset(tweets, select = c(text,lat.x,lon.x,fips.x,harassment))

# Limit the new data to just the unlabeled tweets
newdata = subset(tweets,is.na(tweets$harassment)) 

# Construct corpus and the dtm
tweets2dfm = create_matrix(newdata$text, 
                          language = "english",
                          removeNumbers=T, 
                          removePunctuation=T, 
                          toLower = T,
                          originalMatrix = tweetsdfm)

container2 = create_container(tweets2dfm, newdata$harassment,
                              testSize = 1:nrow(newdata),
                             virgin = TRUE)

# Classify the new data
finalresults = classify_models(container2, models)

# Collate final results
finalresults.df = as.data.frame(finalresults)
finalresults.df = cbind(newdata,finalresults.df)

# Merge the labeled data back in
# Have to make the columns the same
finalresults.df$lat = finalresults.df$lat.x
finalresults.df$lon = finalresults.df$lon.x
finalresults.df$fips = finalresults.df$fips.x
finalresults.df = subset(finalresults.df, select = c(text,lat,lon,fips,harassment,
                                                     SVM_LABEL,SVM_PROB,MAXENTROPY_LABEL,
                                                     MAXENTROPY_PROB,FORESTS_LABEL,FORESTS_PROB,
                                                     TREE_LABEL,TREE_PROB))
labeled_data$SVM_LABEL = "NA"
labeled_data$SVM_PROB = "NA"
labeled_data$MAXENTROPY_LABEL = "NA"
labeled_data$MAXENTROPY_PROB = "NA"
labeled_data$FORESTS_LABEL = "NA"
labeled_data$FORESTS_PROB = "NA"
labeled_data$TREE_LABEL = "NA"
labeled_data$TREE_PROB = "NA"

labeled_data$X = NULL

finalresults.df = rbind(finalresults.df,labeled_data)

# Decide to filter tweet if at least three algos think it's *not* harassment
finalresults.df$SVM_LABEL = as.numeric(finalresults.df$SVM_LABEL)
finalresults.df$FORESTS_LABEL = as.numeric(finalresults.df$FORESTS_LABEL)
finalresults.df$TREE_LABEL = as.numeric(finalresults.df$TREE_LABEL)
finalresults.df$MAXENTROPY_LABEL = as.numeric(finalresults.df$MAXENTROPY_LABEL)
finalresults.df$algosum = rowSums(finalresults.df[c("SVM_LABEL", "FORESTS_LABEL", "TREE_LABEL", "MAXENTROPY_LABEL")], na.rm = FALSE)
finalresults.df$decision <- ifelse(finalresults.df$algosum<=5, 1, 2)
finalresults.df$decision = finalresults.df$decision-1 # To make it 0/1 rather than 1/2
finalresults.df$harassment[is.na(finalresults.df$harassment)] = finalresults.df$decision

# Write out to CSV
write.csv(finalresults.df, file = "harassmentdata_tweetlevel_filtered.csv")

# collapse to county level
tweetdata = subset(finalresults.df, select = c(fips,harassment))
tweetdata = aggregate(tweetdata, by=list(tweetdata$fips), FUN = sum)
tweetdata$fips = tweetdata$Group.1
tweetdata$Group.1 = NULL
tweetdata$harassment_filtered = tweetdata$harassment
tweetdata$harassment = NULL

# Merge in other county-level data
countydata = read.csv("countydata.csv")
countydata = merge(countydata,tweetdata, by = "fips", all.x = TRUE)
countydata$harassment_filtered[is.na(countydata$harassment_filtered)] = 0

# Pct harassment filtered
countydata$pctharassment_filtered = (countydata$harassment_filtered/countydata$count)*100

# write to csv
write.csv(data_merged, file="countydata_classified_10-10-17.csv")




