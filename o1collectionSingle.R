# setup the libraries
wants <- c("SocialMediaLab", "magrittr", "stringr","twitteR","ROAuth")
has   <- wants %in% rownames(installed.packages())
if(any(!has)) install.packages(wants[!has])
lapply(wants, library, character.only=T)

# good tutorial http://www.academia.edu/19064267/Absolute_Beginners_Guide_to_the_SocialMediaLab_package_in_R
# and http://vosonlab.net/papers/SocialMediaLab/SocialMediaLab_package_tutorial.pdf
#instagramClientId <- "b1162e9e7e21423d908cc74e40ba46df"

myapikey =''
myapisecret =''
myaccesstoken = ''
myaccesstokensecret = ''

tweets <- Authenticate("twitter",
                       apiKey = myapikey,
                       apiSecret = myapisecret,
                       accessToken = myaccesstoken,
                       accessTokenSecret = myaccesstokensecret) %>%
  # interesting tags: #starwars, #trump
  # restore from file csv doesnt work, rdata works
Collect(searchTerm="#trump", numTweets = 100000,writeToFile=TRUE,verbose=TRUE)#, waitForRateLimit = TRUE)

fileString <- paste('tweets',format(Sys.time(), "%a-%b-%d-%X-%Y"), "_tweets.Rdata", sep = '')
first <- str_replace(fileString, ':', "_")
second <- str_replace(first, ':', "_")
save(tweets, file = second)



#####################################################
setup_twitter_oauth(	myapikey,
					myapisecret,
					myaccesstoken,
					myaccesstokensecret)
user <- getUser("realDonaldTrump")
user$toDataFrame()
friends <- user$getFriends() # who this user follows
followers <- user$getFollowers() # this user's followers
####################################################
tweets <- searchTwitter("#trump", n=10000)
length(tweets)
tweets.df <- twListToDF(tweets)
write.csv(tweets.df, file = "data/trumpHashtagTweets.csv", row.names=F)
