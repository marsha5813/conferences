# Reads tweets from multiple .csv files and creates a partition for training and test data

# intialize
# setwd("C:/Users/jmarshall2/Documents/twitter/htweets")

# Read in data and append
 
# append
dframes = lapply(filelist, read.csv)
data = do.call(rbind, dframes)
rm(dframes)

# write to csv
write.csv(data, file="htweets_combined.csv")

# Take a sample
## set the seed to make your partition reproductible
set.seed(8675309)
train_ind = sample(seq_len(nrow(data)), size = 1000)

train = data[train_ind, ]
test = data[-train_ind, ]

# write training set to csv
write.csv(train, file="harassment_trainingset.csv")

