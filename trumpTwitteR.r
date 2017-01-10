# some stuff is from http://www.rdatamining.com/docs/twitter-analysis-with-r

library(twitteR)
library(tm)
#library(ROAuth)
library(wordcloud)

#setup_twitter_oauth(	myapikey,#
					#myapisecret,
					#myaccesstoken,
					#myaccesstokensecret)
#################################################
#tweets <- userTimeline("realDonaldTrump",n=10000)
# convert tweets to data frame
#tweets.df <- twListToDF(tweets)
#write.csv(tweets.df, file = "data/trumpTweets.csv", row.names=F)
#length(tweets)
############ some user info
user <- getUser("realDonaldTrump")
user$toDataFrame()
friends <- user$getFriends() # who this user follows
followers <- user$getFollowers() # this user's followers
####################################################
tweets <- searchTwitter("#trump", n=10000)
length(tweets)
tweets.df <- twListToDF(tweets)
write.csv(tweets.df, file = "data/trumpHashtagTweets.csv", row.names=F)

tweets <- read.csv("data/trumpTweets.csv")
library(ggplot2)
library(lubridate)
library(scales)

ggplot(tweets, aes(factor(grepl("#", tweets$text)))) +
        geom_bar(fill = "midnightblue") + 
        theme(legend.position="none", axis.title.x = element_blank()) +
        ylab("Number of tweets") + 
        ggtitle("Tweets with Hashtags") +
        scale_x_discrete(labels=c("No hashtags", "Tweets with hashtags"))

ggplot(tweets, aes(factor(grepl('^"', tweets$text)))) +
        geom_bar(fill = "midnightblue") + 
        theme(legend.position="none", axis.title.x = element_blank()) +
        ylab("Number of tweets") + 
        ggtitle("Tweets with quatation mark") +
        scale_x_discrete(labels=c("No quatation mark", "Tweets with quatation mark"))

tweets$created <- ymd_hms(tweets$created)

ggplot(data = tweets, aes(x = created)) +
        geom_histogram(aes(fill = ..count..)) +
        theme(legend.position = "none") +
        xlab("Time") + ylab("Number of tweets") + 
        scale_fill_gradient(low = "midnightblue", high = "aquamarine4")

ggplot(data = tweets, aes(x = wday(created))) +
        geom_histogram(breaks = seq(0.5, 8, by =1), aes(fill = ..count..)) +
        theme(legend.position = "none") +
        xlab("Day of the Week") + ylab("Number of tweets") + 
        scale_fill_gradient(low = "midnightblue", high = "aquamarine4")


############################### http://varianceexplained.org/r/trump-tweets/
 # https://www.r-bloggers.com/twitter-sentiment-analysis-with-r/
 # http://www.rdatamining.com/docs/twitter-analysis-with-r
 # http://varianceexplained.org/r/trump-tweets/

library(dplyr)
library(purrr)
library(tidyr)

tweets <- read.csv("data/trumpTweets.csv")

# percentage of devices from which is tweeted
# if device is Android that is Trump himself
# if device is iPhone or anything else that is his staff
device <- function(dataframe) {
    tot_all <- length(dataframe$statusSource)
 
    Web <- 100*(length(grep("Twitter Web Client", dataframe$statusSource))/tot_all)
    TweetDeck <- 100*(length(grep("TweetDeck", dataframe$statusSource))/tot_all)
    iPhone <- 100*(length(grep("Twitter for iPhone", dataframe$statusSource))/tot_all)
    iPad <- 100*(length(grep("Twitter for iPad", dataframe$statusSource))/tot_all)
    Blackberry <- 100*(length(grep("Twitter for BlackBerry", dataframe$statusSource))/tot_all)
    Tweetbot <- 100*(length(grep("Tweetbot for i", dataframe$statusSource))/tot_all)
    Hootsuite <- 100*(length(grep("Hootsuite", dataframe$statusSource))/tot_all)
    Android <- 100*(length(grep("Twitter for Android", dataframe$statusSource))/tot_all)
    Ads <- 100*(length(grep("Twitter Ads", dataframe$statusSource))/tot_all)
    M5 <- 100*(length(grep("Mobile Web (M5)", dataframe$statusSource))/tot_all)
    Mac <- 100*(length(grep("Twitter for Mac", dataframe$statusSource))/tot_all)
    Facebook <- 100*(length(grep("Facebook", dataframe$statusSource))/tot_all)
    Instagram <- 100*(length(grep("Instagram", dataframe$statusSource))/tot_all)
    IFTT <- 100*(length(grep("IFTT", dataframe$statusSource))/tot_all)
    Buffer <- 100*(length(grep("Buffer", dataframe$statusSource))/tot_all)
    CoSchedule <- 100*(length(grep("CoSchedule", dataframe$statusSource))/tot_all)
    GainApp <- 100*(length(grep("Gain App", dataframe$statusSource))/tot_all)
    MobileWeb <- 100*(length(grep("Mobile Web", dataframe$statusSource))/tot_all)
    iOS <- 100*(length(grep("iOS", dataframe$statusSource))/tot_all)
    OSX <- 100*(length(grep("OS X", dataframe$statusSource))/tot_all)
    Echofon <- 100*(length(grep("Echofon", dataframe$statusSource))/tot_all)
    Fireside <- 100*(length(grep("Fireside Publishing", dataframe$statusSource))/tot_all)
    Google <- 100*(length(grep("Google", dataframe$statusSource))/tot_all)
    MailChimp <- 100*(length(grep("MailChimp", dataframe$statusSource))/tot_all)
    TwitterForWebsites <- 100*(length(grep("Twitter for Websites", dataframe$statusSource))/tot_all)
 
    percentages <- data.frame(Web, TweetDeck, iPhone, iPad, Blackberry, Tweetbot,
               Hootsuite, Android, Ads, M5, Mac, Facebook, Instagram,
               IFTT, Buffer, CoSchedule, GainApp, MobileWeb, iOS, OSX,
               Echofon, Fireside, Google, MailChimp, TwitterForWebsites)
    return(percentages) }

trumpDevice <- sort(device(tweets), decreasing = T)
 


iPhone <- length(grep("Twitter for iPhone", tweets$statusSource))
Android <- length(grep("Twitter for Android", tweets$statusSource))

