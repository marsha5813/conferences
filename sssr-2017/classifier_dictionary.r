# Define function
process_tweets = function(file) {
    filename = paste(file, ".json", sep = "") 
    tweets = parseTweets(filename, verbose=TRUE)

    # Use grepl to search for expressions in harassment.terms
    results.df = sapply(harassment.terms, grepl, tweets$text, ignore.case=TRUE)
    results.df = as.data.frame(results.df)
    
    # Make syntactically valid column names
    colnames(results.df) = make.names(colnames(results.df))
    
    # Merge results back into the dataframe (tweets and results.df should have same N)
    tweets = cbind(tweets, results.df)
    
    # Create a flag if the tweets contains any instance of harassment
    tweets$isharassment <- rowSums(tweets[,make.names(harassment.terms)])
    tweets$isharassment <- ifelse(tweets$isharassment>0,1,0)
    
    # Create a flag if the tweets contains any instance of harassment except "bitch"
    terms = harassment.terms[-1]
    tweets$isharassment2 <- rowSums(tweets[,make.names(terms)])
    tweets$isharassment2 <- ifelse(tweets$isharassment2>0,1,0)

    # Clean up the location data
    ## Replaces the more precise "lat" and "lon" with "place_lat" or "place_lon" if the more precise coordinates are missing
    tweets$lat = ifelse(is.na(tweets$lat), tweets$place_lat, tweets$lat)
    tweets$lon = ifelse(is.na(tweets$lon), tweets$place_lon, tweets$lon)
    
  # Geocode the full data
  tweets = clean.location.data(tweets)
  tweets <- tweets[complete.cases(tweets$lat), ]
  coords = get.coordinates(tweets)
  coords$name=latlong2county(coords)
  coords[coords==99999] = NA
  coords$fips = with(county.fips, fips[match(coords$name, polyname)])
  tweets$assigned_name = coords$name
  tweets$fips = coords$fips
  rm(coords)
  
  # Save the full data as .csv
  filename <- paste(file, "_geocoded_dictclassified.csv", sep = "") 
  write.csv(tweets, filename)
  
  # Save a smaller version of the data as .csv
  tweets_small = subset(tweets, select=c(text,lat,lon,place_lat,place_lon,fips,isharassment))
  filename <- paste(file, "_geocoded_dictclassified_reduced.csv", sep = "") 
  write.csv(tweets_small, filename)
  
  # Save just the "harassment" tweets, with text and lat/lon, to csv
  harassmentdata = subset(tweets, isharassment==1, select=(c(text, lat, lon, fips)))
  filename <- paste(file, "_htweets.csv", sep = "") 
  write.csv(harassmentdata, filename)
  
  # Collapse to county level and save as csv
  tweets = subset(tweets, select = c(fips, isharassment))
  tweets$count = 1
  tweets = aggregate(tweets, by=list(tweets$fips), FUN = sum, na.rm=T)
  tweets = subset(tweets, select = c(Group.1,count, isharassment))
  tweets$propharassment = tweets$isharassment/tweets$count
  filename <- paste(file, "_counties.csv", sep = "") 
  write.csv(tweets, filename)
  
}


# Make a clean-up function
clean_up = function() {
  
  # wipe environment
  rm(list = ls())
  
  # Libraries
  library(streamR)
  library(quanteda)
  library(tidyverse)
  library(maptools)
  library(maps)
  library(sp)
  
  # Functions
  source("functions.r")
  source("classify_harassment_wdict.r")
  
  # Load dictionary
  harassment.terms <- readLines("sexual-harassment-words.txt")
  
}














