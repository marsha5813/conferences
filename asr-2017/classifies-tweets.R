

###########################################
# LOAD TWEETS
###########################################

# read in my tweets
tweets = tweets.df[sample(1:nrow(tweets.df), 10000, replace=FALSE),] 
# tweets = tweets.df

# convert my text to object of type corpus
tweetscorpus = corpus(tweets$text)

# save the raw text of the tweets in a docvar
docvars(tweetscorpus, "Rawtext") <- tweets$text

# tokenize to get number of words
tweetstokens = tokenize(tweetscorpus,
                        remove_punct=T)




###########################################
# CLASSIFY EMOTION
###########################################


# Uses the NRC word-emotion lexicon to associate tweets
# with one of eight emotions: anger, fear, anticipation, trust, surprise, sadness, joy, and disgust

# Load libraries
library(quanteda)

# Read in the dictionary
words = readLines("nrc-words-emotions-lexicon.txt")[29:141848]
words = strsplit(words, "\t")

# Convert to object of type corpus
wordscorp = corpus(sapply(words,function(x) x[1]))

# Assign document-level variables
docvars(wordscorp, "Emotion") <- sapply(words,function(x) x[2])
docvars(wordscorp, "TF") <- sapply(words,function(x) x[3])

# Create lists of words
angerwords = wordscorp[(docvars(wordscorp, "Emotion")=="anger" & docvars(wordscorp, "TF")==1)]
fearwords = wordscorp[(docvars(wordscorp, "Emotion")=="fear" & docvars(wordscorp, "TF")==1)]
anticipationwords = wordscorp[(docvars(wordscorp, "Emotion")=="anticipation" & docvars(wordscorp, "TF")==1)]
trustwords = wordscorp[(docvars(wordscorp, "Emotion")=="trust" & docvars(wordscorp, "TF")==1)]
surprisewords = wordscorp[(docvars(wordscorp, "Emotion")=="surprise" & docvars(wordscorp, "TF")==1)]
sadnesswords = wordscorp[(docvars(wordscorp, "Emotion")=="sadness" & docvars(wordscorp, "TF")==1)]
joywords = wordscorp[(docvars(wordscorp, "Emotion")=="joy" & docvars(wordscorp, "TF")==1)]
disgustwords = wordscorp[(docvars(wordscorp, "Emotion")=="disgust" & docvars(wordscorp, "TF")==1)]

# Create the emotions dictionary
emotionsdict = dictionary(list(anger=angerwords,
                               fear=fearwords,
                               anticipation=anticipationwords,
                               trust=trustwords,
                               surprise=surprisewords,
                               sadness=sadnesswords,
                               joy=joywords,
                               disgust=disgustwords))




# Get english stopwords from quanteda and assign them to a vector
eng.stopwords = stopwords('english')

# Convert tweets to doc-term matrix
tweetsdfm = dfm(tweetscorpus,
                tolower = TRUE,
                dictionary=emotionsdict)


# Get proportions of various kinds of words in tweets
propanger = as.numeric(tweetsdfm[,"anger"] / ntoken(tweetstokens))
propfear =  as.numeric(tweetsdfm[,"fear"] / ntoken(tweetstokens))
propanticipation =  as.numeric(tweetsdfm[,"anticipation"] / ntoken(tweetstokens))
proptrust =  as.numeric(tweetsdfm[,"trust"] / ntoken(tweetstokens))
propsurprise =  as.numeric(tweetsdfm[,"surprise"] / ntoken(tweetstokens))
propsadness =  as.numeric(tweetsdfm[,"sadness"] / ntoken(tweetstokens))
propjoy =  as.numeric(tweetsdfm[,"joy"] / ntoken(tweetstokens))
propdisgust =  as.numeric(tweetsdfm[,"disgust"] / ntoken(tweetstokens))

# Put everything back into the dataframe
tweets$propanger = propanger
tweets$propfear = propfear
tweets$propanticipation = propanticipation
tweets$proptrust = proptrust
tweets$propsurprise = propsurprise
tweets$propsadness = propsadness
tweets$propjoy = propjoy
tweets$propdisgust = propdisgust




###########################################
# CLASSIFY SENTIMENT
###########################################


library(e1071)

# Load the readr package
# install.packages("readr")
library(readr)

# Load in the pre-labeled data
labeled_data = read_csv("Sentiment Analysis Dataset.csv")

# Take a sample of prelabaled data
labeled_data_sample = labeled_data[sample(1:nrow(tweets.df), 10000, replace=FALSE),] 

# convert my text to object of type corpus
tweetscorpus = corpus(labeled_data_sample$SentimentText)

# Assign the true sentiment score labels to a document-level variable
docvars(tweetscorpus, "trueSentiment") <- labeled_data_sample$Sentiment

# Create the doc-term matrix
tweetsdfm = dfm(tweetscorpus,
                remove_numbers = TRUE, 
                remove_punct = TRUE,
                remove_symbols = TRUE,
                remove_url = TRUE,
                remove_twitter = TRUE,
                remove_separators = TRUE,
                remove=eng.stopwords,
                tolower = TRUE)

# Remove infrequent terms
tweetsdfmtrim = dfm_trim(tweetsdfm,
                         min_doc = floor(0.01*ndoc(tweetscorpus)))

# Extract true sentiment labels
realSentiments = docvars(tweetscorpus)$trueSentiment

# Convert to dense matrix
training.set = as.matrix(tweetsdfmtrim)
training.set = training.set[,colSums(tweetsdfmtrim)>0] # only keep columns with colsums>0
training.set = training.set[rowSums(tweetsdfmtrim)>0,] # only keep rows with rowsums>0
trainingLabels = as.factor(realSentiments)[rowSums(tweetsdfmtrim)>0] # converting real sentiments to factors so R will know they're categories

# Put everything together in a dataframe
train.obs = sample(nrow(training.set),size=.5*nrow(training.set))
nbdat = data.frame(training.set[train.obs,],out=as.factor(trainingLabels[train.obs]))

# Fitting the Naive Bayes Model
J = ncol(nbdat)
naive.bayes <- naiveBayes(x=nbdat[,-J], # x is everything in nbdat except the target (J)
                          y=nbdat$out,laplace=0) 

## Looking at the predictions from the training set:
trainPred = predict(naive.bayes,
                    newdata=nbdat, 
                    type="class")

# Evaluate performance
performance(nbdat$out,trainPred)


# Confusion matrix
table(trainPred,nbdat$out)
prop.table(table(trainPred,nbdat$out))
prop.table(table(trainPred,nbdat$out),margin=1)
prop.table(table(trainPred,nbdat$out),margin=2)


# investigate false negatives
false_neg = which(nbdat$out ==1,
                  trainPred == 0)

# and false positives
false_pos = which(nbdat$out ==0,
                  trainPred ==1)


# Now bring in the out-of-sample data
tweets=tweets.df
tweetscorpus = corpus(tweets$text)

# Create the doc-term matrix
tweetsdfm = dfm(tweetscorpus,
                remove_numbers = TRUE, 
                remove_punct = TRUE,
                remove_symbols = TRUE,
                remove_url = TRUE,
                remove_twitter = TRUE,
                remove_separators = TRUE,
                remove=eng.stopwords,
                tolower = TRUE)


# Remove infrequent terms
tweetsdfmtrim = dfm_trim(tweetsdfm,
                         min_doc = floor(0.01*ndoc(tweetscorpus)))

# Convert to dense matrix
newdata = as.matrix(tweetsdfmtrim)

# Predict 0-1 sentiment labels
predicted = predict(naive.bayes, newdata)

# Predict raw probabilites 
predicted_raw = predict(naive.bayes, newdata, type="raw")

# Put everything back into the dataframe
tweets$nb_sent = predicted # the 0-1 sentiment labels
tweets$nb_probpos = predicted_raw[,2]
tweets$nb_probnegs = predicted_raw[,1]

# And add back the emotion classification from above
tweets$propanger = tweets_w_emotion$propanger
tweets$propfear = tweets_w_emotion$propfear
tweets$propanticipation = tweets_w_emotion$propanticipation
tweets$proptrust = tweets_w_emotion$proptrust
tweets$propsurprise = tweets_w_emotion$propsurprise
tweets$propsadness = tweets_w_emotion$propsadness
tweets$propjoy = tweets_w_emotion$propjoy
tweets$propdisgust = tweets_w_emotion$propdisgust

# Save this file just in case
save(tweets, file="tweets_classified")


###########################################
# GEOCODE
###########################################
# Load my functions
source("functions.r")

# Load libraries
library(sp)
library(maps)
library(maptools)

# Clean location data for tweets
tweets = clean.location.data(tweets)

# Drop observations with no valid location data
tweets <- tweets[complete.cases(tweets$lat), ]

## This is a decent method of getting fips codes. Got this from a
## Stackoverflow response. 
## It's fast but sometimes leaves locations unidentified
## If this doesn't run, look for an error message regarding "rgeos" or "gpclib"
## Note that there is a much slower, but more thorough, method of getting fips codes
## that involves submitting every lat/lon pair to the FCC's geolocation API
## Check my "functions" file to see it.
coords = get.coordinates(tweets)
coords$name=latlong2county(coords)
coords[coords==99999] = NA
coords$fips = with(county.fips, fips[match(coords$name, polyname)])
tweets$assigned_name = coords$name
tweets$fips = coords$fips
rm(coords)

# Save the data as Rdata
save(tweets, file="tweets_classified_geocoded")

# Save the data as stata .dta
# library(foreign)
# write.dta(tweets, "E:/OneDrive/OneDrive - purdue.edu/wd/twitter-sentiment-and-religious-geography/tweets_classified_geocoded.dta")

# write output to csv
write.csv(tweets, file = "tweets_classified_geocoded.csv")





