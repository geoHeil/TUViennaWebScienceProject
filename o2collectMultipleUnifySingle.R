wants <- c("SocialMediaLab", "magrittr", "stringr", "data.table")
has   <- wants %in% rownames(installed.packages())
if(any(!has)) install.packages(wants[!has])
lapply(wants, library, character.only=T)

#####################
# unify loaded datasets
# TODO improve write a function wich is catching all _TwitterData files in a folder
myDataFromDisk1 <- importData("Dez_28_14_33_13_2016_CET_#trump_TwitterData.csv","twitter")
myDataFromDisk2 <- importData("Dez_28_14_33_13_2016_CET_#trump_TwitterData.csv","twitter")

# TODO perform union https://github.com/vosonlab/SocialMediaLab/issues/23
# return singleBigTweets.csv file