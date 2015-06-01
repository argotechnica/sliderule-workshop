"
monitor sentiment toward NSA on twitter

created with help from:
  * https://rdatamining.wordpress.com/2011/11/09/using-text-mining-to-find-out-what-rdatamining-tweets-are-about/
  * http://pastebin.com/Ygkaj1W6
  * http://www.vikparuchuri.com/blog/tracking-us-sentiments-over-time-in/
  * http://www2.imm.dtu.dk/pubdb/views/publication_details.php?id=6010
  * http://ipsc.jrc.ec.europa.eu/index.php?id=42
"

# init
require(dplyr)
require(jsonlite)
require(lubridate)
require(stringr)
require(tm)
require(twitteR)

# setup oath session using credentials loaded from json file,
# removing credential data from memory after use
auths <- fromJSON("C:\\CMD\\auths.json")
options(httr_oauth_cache = FALSE)
setup_twitter_oauth(consumer_key = auths$twitter$api_key,
                    consumer_secret = auths$twitter$api_secret,
                    access_token = auths$twitter$access_token,
                    access_secret = auths$twitter$access_token_secret)
rm(auths)

# get tweets as a data frame
tweets.list <- searchTwitter("NSA", n=500, lang="en")
tweets.df <- twListToDF(tweets.list)
tweets.df <- filter(tweets.df, isRetweet == FALSE)
tweets.df$text <- str_replace_all(tweets.df$text,"[^[:graph:]]", " ")

write.csv(tweets.df, file = paste0("tweets.df.",format(Sys.time(), "%Y-%m-%d.%H%M"),".txt"))

############ SKIP BELOW FOR NOW

# build a clean word corpus, e.g. remove punctuation, numbers, stopwords...
# tweets.corpus <- Corpus(VectorSource(tweets.df$text))
# tweets.corpus <- tm_map(tweets.corpus, tolower)
# tweets.corpus <- tm_map(tweets.corpus, removePunctuation)
# tweets.corpus <- tm_map(tweets.corpus, removeNumbers)
# tweets.corpus <- tm_map(tweets.corpus, removeWords, stopwords("english"))

"
maybe use these at some point?
  * tidyr
  * wordcloud
  * RTextTools
  * topicmodels
  * SnowballC
"