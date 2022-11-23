
# Tweetr <a href='https://github.com/younessbahi/tweetr/blob/main/'><img src='man/figures/logo.png' align="right" height="139" /></a>
[![R-CMD-check](https://github.com/younessbahi/tweetr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/younessbahi/tweetr/actions/workflows/R-CMD-check.yaml)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

A Twitter scraping package that enables the collection of Tweets and other data without using the Twitter's API.

### Some of the benefits from using the tweetr package:

- Fetch real-time data.
- Can fetch all tweets.
- No limit of tweets.
- No initial setup, import or installation is needed.
- Anonymous usage.
- No login or authentication is required.
- No API key is required.
- Complexity-free.


### Installation
```R 
# Development
remotes::install_github("younessbahi/tweetr")
``` 

### Basic usage

```R
#Search tweets
tw <- 
  tweetr::get_tweets(
    query     = '#Bitcoin',
    since    = '2022-10-14', 
    until    = '2022-10-16',
    count    = 2000,
    minLikes = 50,
    lang = 'en'
  )

#Influence score of a term or a hashtag
sc <- tweetr::get_score('#Bitcoin')

#Trending in London
location <- tweetr::loc

london_id <- 
  location %>% 
    filter(name == 'London') %>%
    pull(woeid) %>%
    as.character #must be a string

trend <- tweetr::get_trends(london_id)
```
### Documentation
Learn more about Tweetr and follow the instructions from [here](https://younessbahi.github.io/tweetr.docs/).