
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


#### Search tweets
```R
tw <- tweetr::get_tweets(
  query    = '#Bitcoin',
  since    = '2022-10-14',
  until    = '2022-10-16',
  count    = 1000, #recommended to specify tweets count
  minLikes = 50,
  lang = 'en'
)
```

#### Search by location
```R
tw <- get_tweets(
  place    = 'London', #search by location (city or country name)
  count = 100
)
```

#### Search by geolocation
<a href='https://github.com/younessbahi/tweetr/blob/main/'><img src='man/figures/amsterdam.png' align="center"/></a>

```R
tw <- get_tweets(
  lat    = "52.37033345325099", 
  long   = "4.878548393047705", 
  radius = "1km", 
  count  = 100
)
```

#### Search tweets from a user
```R
user_tweets <- get_tweets(
    from = '@elonMusk', 
    since = '2022/01/01',
    hasMedia = TRUE, #include only posts with media
    replies = FALSE #exclude replies from search result
  )
```

#### Influence score of a term or a hashtag
```R
sc <- tweetr::get_score('#Bitcoin')
```

#### Search trends
```R
#trending in London
location <- tweetr::loc

london_id <-
  location %>%
    filter(name == 'London') %>%
    pull(woeid) %>%
    as.character #must be a string

trend <- tweetr::get_trends(london_id)
```
### Documentation
Learn more about Tweetr from [here](https://younessbahi.github.io/tweetr.docs/).