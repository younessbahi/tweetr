
# Tweetr <a href='https://github.com/younessbahi/tweetr/blob/main/'><img src='man/figures/logo.png' align="right" height="139" /></a>
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
#search tweets
tw <- 
  tweetr::get_tweets(
    query     = '#Bitcoin',
    .since    = '2022-10-14', 
    .until    = '2022-10-16',
    .count    = 500,
    .minLikes = 50
  )

#influence score of a term or a hashtag
sc <- tweetr::get_score('#Bitcoin')

#trending in London
tr <- tweetr::get_trends('44418')
```
### Documentation
Learn more about Tweetr and follow the instructions from [here](https://younessbahi.github.io/tweetr.docs/).