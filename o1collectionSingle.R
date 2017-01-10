# setup the libraries
wants <- c("SocialMediaLab", "magrittr", "stringr")
has   <- wants %in% rownames(installed.packages())
if(any(!has)) install.packages(wants[!has])
lapply(wants, library, character.only=T)

# good tutorial http://www.academia.edu/19064267/Absolute_Beginners_Guide_to_the_SocialMediaLab_package_in_R
# and http://vosonlab.net/papers/SocialMediaLab/SocialMediaLab_package_tutorial.pdf
#instagramClientId <- "b1162e9e7e21423d908cc74e40ba46df"

myapikey ='WTvRSzC5IF5nibr9UiscV0WVq'
myapisecret ='7UBJsGQrYnbzcP8QMXFHNKYNSHZcq9swHNhkS7r9K7rSY7vuPk'
myaccesstoken = '619375828-XFamxXqogBMLVJmelgJ9iNntXGtCD16vrfUDkbVP'
myaccesstokensecret = 'e7reXp4rAEASrS3E9yL2INYi2YNGGjzPKrrqCOdHeQL6k'

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
