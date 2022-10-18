# Tweetr

A Twitter scraping package written that enables the collection of Tweets and other data without using the Twitter's API.

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
tw <- tweetr::get_tweets(query = '#Bitcoin', .count = 500, .since = '2022-10-14', .until = '2022-10-16', .minLikes = 50)

#influence score of a term or a hashtag
sc <- tweetr::get_score('#Bitcoin')

#trending in London
tr <- tweetr::get_trends('44418')
```