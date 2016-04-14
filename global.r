## load library
library(twitteR)

## setting up OAuth ----
cred <- read.csv("credentials/twitter_keys.csv", header = TRUE)
api_key <-  cred$api_key
api_secret <- cred$api_secret
access_token <- cred$access_token
access_secret <- cred$access_secret
setup_twitter_oauth(api_key, api_secret, access_token, access_secret)
