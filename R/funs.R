#' Helper function to unnest nested dataframe in list.
#' @rdname get_tweets
#' @keywords internal
#' @param .x A list vector.
#' @noRd

tidy_ <- function(.x) {
  unlist(.x, recursive = F) %>%
    enframe('rowID') %>%
    unnest_wider(value)
}

#' Helper function to format datetime.
#' @rdname get_tweets
#' @keywords internal
#' @param str_date A character date, (example: "Thu Oct 16 20:16:47 +0000 2022")
#' @noRd

parse_datetime <- function(str_date) {
  as.POSIXct(str_date, format = "%a %b %d %H:%M:%S +0000 %Y", tz = "GMT")
}

#' Helper function to scrape search influence scores.
#' @rdname get_score
#' @keywords internal
#' @param keyword A search term or a hashtag.
#' @noRd

score_ <- function(keyword) {
  
  q <- 'science'
  
  cookies <- tweetr:::set_cookies(q = q)
  header  <- tweetr:::header_score(data = cookies, q = q)
  
  params <-
    list(
      `q`           = as.character(keyword),
      `src`         = 'search_box',
      `result_type` = 'users,topics,tweets'
    )
  
  get.score <-
    httr::GET(
      url   = 'https://twitter.com/i/api/1.1/search/typeahead.json',
      httr::add_headers(.headers = header),
      query = params,
      httr::set_cookies(.cookies = cookies)
    )
  
  return(httr::content(get.score))
}

#' Helper function to scrape trend.
#' @rdname get_trends
#' @keywords internal
#' @param id Location ID.
#' @noRd

trends_ <- function(id) {
  
  keyword <- 'science'
  cookies <- tweetr:::set_cookies(q = keyword)
  header  <- tweetr:::header_trends(cookies, q = keyword)
  
  params = list(
    `id` = id #get place id from locID.rds
  )
  
  get.trends <-
    httr::GET(
      url   = 'https://api.twitter.com/1.1/trends/place.json',
      httr::add_headers(.headers = header),
      query = params,
      httr::set_cookies(.cookies = cookies)
    )
  
  return(httr::content(get.trends))
  
}

#' Helper function to clean tweets entities.
#' @importFrom stats na.omit
#' @rdname get_tweets
#' @keywords internal
#' @param tweets Tweets dataframe.
#' @noRd

tw_entity_clean <- function(tweets) {
  entities <-
    tweets %>%
      pluck('entities') %>%
      enframe() %>%
      unnest_wider(value)
  
  ## Hashtags ####
  #/ linkage with tweets rowID /
  
  
  if (all_na(entities$hashtags)) {
    
    hashtags <- list()
    
  } else {
    
    hashtags <-
      entities %>%
        select(name, hashtags) %>%
        rename(rowID = name)
    
    hashtags$hashtags <- map_depth(hashtags$hashtags, 2, ~ .$text)
    
    hashtags$hashtags <-
      lapply(
        hashtags$hashtags,
        function(e) { if (is_empty(e)) NA else e }
      )
    
    hashtags %<>%
      unnest(cols = hashtags) %>%
      unnest(cols = hashtags)
    
    hashtags$id_str <- pull(tweets[hashtags$rowID, "id_str"])
  }
  
  ## URLS ####
  
  if (all_na(entities$urls)) {
    
    tw.urls <- list()
    
  } else {
    
    tw.urls <-
      entities %>%
        select(name, urls) %>%
        rename(rowID = name)
    
    tw.urls$urls <- lapply(tw.urls$urls, function(e) { if (is_empty(e)) NA else e })
    
    tw.urls <-
      tw.urls %>%
        unnest(cols = 'urls') %>%
        unnest_wider('urls')
    
    tw.urls$id_str <- pull(tweets[tw.urls$rowID, "id_str"])
    
    if (any(names(tw.urls) == 'indices')) {
      tw.urls %<>% select(- indices)
    }
    
    tw.urls %<>% filter(! is.na(expanded_url))
  }
  
  ## Mentions ####
  #/ linkage with tweets rowID /
  mentions <-
    entities %>%
      select(name, user_mentions)
  
  mentions$user_mentions <-
    lapply(
      mentions$user_mentions,
      function(e) { if (is_empty(e)) NA else e }
    )
  
  mentions %<>%
    unnest(user_mentions)
  
  mentions %<>% pluck('user_mentions') %>%
    enframe(name = 'rowID') %>%
    mutate(rowID = mentions$name) %>%
    unnest_wider(value)
  
  if (length(mentions) < 6) {
    
    mentions <- list()
    
  } else {
    
    mentions$id_str <- pull(tweets[mentions$rowID, "id_str"])
    mentions$id     <- as.character(mentions$id)
    
    if (any(names(mentions) == 'indices')) {
      mentions %<>% select(- indices)
    }
    
    mentions %<>% filter(! is.na(id))
    
  }
  
  ## MEDIAS ####
  
  if (any(names(entities) == 'media')) {
    
    tw.media <-
      entities %>%
        select(name, media) %>%
        rename(rowID = name)
    
    tw.media$media <- lapply(tw.media$media, function(e) { if (is_empty(e)) NA else e })
    
    tw.media <-
      unnest(tw.media, cols = 'media')
    
    tw.media_ <-
      tw.media %>%
        pluck('media') %>%
        enframe('rowID') %>%
        mutate(rowID = tw.media$rowID) %>%
        unnest_wider(value)
    
    tw.media_$id_tweet <- pull(tweets[tw.media_$rowID, 'id_str'])
    tw.media           <-
      tw.media_ %>%
        select(- c(indices, original_info, sizes)) %>%
        filter(! is.na(id_str))
    
  } else {
    
    tw.media <- list()
    
  }
  
  ## GEO ####
  if (is.null(tweets[['geo']])) {
    
    tw.geo <- list()
    
  } else {
    
    tw.geo <-
      tweets %>%
        select(id_str, user_id_str, geo) %>%
        pluck('geo') %>%
        enframe() %>%
        filter(! is.na(value)) %>%
        unnest_wider(value) %>%
        pluck('coordinates') %>%
        enframe() %>%
        unnest_wider(value)
    
    if (length(tw.geo) == 1) {
      
      tw.geo <- list()
      
    } else {
      
      tw.geo %<>%
        set_colnames(c('name', 'lat', 'long')) %>%
        na.omit()
      
      tw.geo$id_str <- pull(tweets[tw.geo$name, 'id_str'])
    }
  }
  
  return(
    list(
      hashtags = hashtags,
      tw.urls  = tw.urls,
      mentions = mentions,
      tw.media = tw.media,
      geo      = tw.geo
    
    )
  )
}

#' Helper function to clean user entity.
#' @rdname get_tweets
#' @keywords internal
#' @param users Users dataframe
#' @noRd

usr_entity_clean <- function(users) {
  
  entities_usr <-
    users %>%
      pluck('entities') %>%
      enframe() %>%
      unnest_wider(value)
  
  if (any(names(entities_usr) == 'url')) {
    
    user.url_ <-
      entities_usr %>%
        select(name, url) %>%
        rename(rowID = name)
    
    user.url <-
      user.url_ %>%
        pluck('url') %>%
        enframe('rowID')
    
    user.url$value <- lapply(user.url$value, function(e) { if (is_empty(e)) NA else e })
    user.url %<>%
      unnest(value) %>%
      pluck('value') %>%
      enframe('rowID')
    
    user.url$value <- lapply(user.url$value, function(e) { if (is_empty(e)) NA else e })
    user.url %<>%
      unnest(value) %>%
      unnest_wider(value) %>%
      mutate(
        rowID      = user.url_$rowID,
        usr_id_str = pull(users[rowID, "id_str"])
      ) %>%
      select(- c(indices, rowID))
    
  } else {
    
    user.url <- list()
    
  }
  return(user.url)
  
}