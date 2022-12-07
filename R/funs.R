#' Helper function to unnest nested dataframe in list.
#' @importFrom tibble enframe
#' @importFrom tidyr unnest_wider
#' @keywords internal
#' @param .x A list vector.
#' @noRd

tidy_ <- function(.x) {
  unlist(.x, recursive = F) %>%
    tibble::enframe('rowID') %>%
    tidyr::unnest_wider(value)
}

#' Helper function to format datetime.
#' @keywords internal
#' @param str_date A character date, (example: "Thu Oct 16 20:16:47 +0000 2022")
#' @noRd

parse_datetime <- function(str_date) {
  as.POSIXct(str_date, format = "%a %b %d %H:%M:%S +0000 %Y", tz = "GMT")
}

#' Helper function to scrape search influence scores.
#' @importFrom httr GET add_headers set_cookies content
#' @keywords internal
#' @param keyword A search term or a hashtag.
#' @noRd

score_ <- function(keyword) {
  
  q <- 'science'
  
  cookies <- set_cookies_(q = q)
  header  <- header_score(data = cookies, q = q)
  
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
#' @importFrom httr GET add_headers set_cookies content
#' @keywords internal
#' @param id Location ID.
#' @noRd

trends_ <- function(id) {
  
  keyword <- 'science'
  cookies <- set_cookies_(q = keyword)
  header  <- header_trends(cookies, q = keyword)
  
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
#' @import dplyr
#' @importFrom magrittr set_colnames %<>%
#' @importFrom stats na.omit
#' @importFrom purrr pluck map_depth is_empty
#' @importFrom tibble enframe
#' @importFrom tidyr unnest_wider unnest
#' @importFrom na.tools all_na
#' @keywords internal
#' @param tweets Tweets dataframe.
#' @noRd

tw_entity_clean <- function(tweets) {
  entities <-
    tweets %>%
      purrr::pluck('entities') %>%
      tibble::enframe() %>%
      tidyr::unnest_wider(value)
  
  ## Hashtags ####
  #/ linkage with tweets rowID /
  
  
  if (na.tools::all_na(entities$hashtags)) {
    
    hashtags <- list()
    
  } else {
    
    hashtags <-
      entities %>%
        dplyr::select(name, hashtags) %>%
        dplyr::rename(rowID = name)
    
    hashtags$hashtags <- purrr::map_depth(hashtags$hashtags, 2, ~ .$text)
    
    hashtags$hashtags <-
      lapply(
        hashtags$hashtags,
        function(e) { if (purrr::is_empty(e)) NA else e }
      )
    
    hashtags %<>%
      tidyr::unnest(cols = hashtags) %>%
      tidyr::unnest(cols = hashtags)
    
    hashtags$id_str <- dplyr::pull(tweets[hashtags$rowID, "id_str"])
  }
  
  ## URLS ####
  
  if (na.tools::all_na(entities$urls)) {
    
    tw.urls <- list()
    
  } else {
    
    tw.urls <-
      entities %>%
        dplyr::select(name, urls) %>%
        dplyr::rename(rowID = name)
    
    tw.urls$urls <- lapply(tw.urls$urls, function(e) { if (purrr::is_empty(e)) NA else e })
    
    tw.urls <-
      tw.urls %>%
        tidyr::unnest(cols = 'urls') %>%
        tidyr::unnest_wider('urls')
    
    tw.urls$id_str <- dplyr::pull(tweets[tw.urls$rowID, "id_str"])
    
    if (any(names(tw.urls) == 'indices')) {
      tw.urls %<>% select(- indices)
    }
    
    tw.urls %<>% dplyr::filter(! is.na(expanded_url))
  }
  
  ## Mentions ####
  #/ linkage with tweets rowID /
  mentions <-
    entities %>%
      dplyr::select(name, user_mentions)
  
  mentions$user_mentions <-
    lapply(
      mentions$user_mentions,
      function(e) { if (purrr::is_empty(e)) NA else e }
    )
  
  mentions %<>%
    tidyr::unnest(user_mentions)
  
  mentions %<>% purrr::pluck('user_mentions') %>%
    tibble::enframe(name = 'rowID') %>%
    dplyr::mutate(rowID = mentions$name) %>%
    tidyr::unnest_wider(value)
  
  if (length(mentions) < 6) {
    
    mentions <- list()
    
  } else {
    
    mentions$id_str <- dplyr::pull(tweets[mentions$rowID, "id_str"])
    mentions$id     <- as.character(mentions$id)
    
    if (any(names(mentions) == 'indices')) {
      mentions %<>% dplyr::select(- indices)
    }
    
    mentions %<>% dplyr::filter(! is.na(id))
    
  }
  
  ## MEDIAS ####
  
  if (any(names(entities) == 'media')) {
    
    tw.media <-
      entities %>%
        dplyr::select(name, media) %>%
        dplyr::rename(rowID = name)
    
    tw.media$media <- lapply(tw.media$media, function(e) { if (purrr::is_empty(e)) NA else e })
    
    tw.media <-
      tidyr::unnest(tw.media, cols = 'media')
    
    tw.media_ <-
      tw.media %>%
        purrr::pluck('media') %>%
        tibble::enframe('rowID') %>%
        dplyr::mutate(rowID = tw.media$rowID) %>%
        tidyr::unnest_wider(value)
    
    tw.media_$id_tweet <- pull(tweets[tw.media_$rowID, 'id_str'])
    tw.media           <-
      tw.media_ %>%
        dplyr::select(- c(indices, original_info, sizes)) %>%
        dplyr::filter(! is.na(id_str))
    
  } else {
    
    tw.media <- list()
    
  }
  
  ## GEO ####
  if (is.null(tweets[['geo']])) {
    
    tw.geo <- list()
    
  } else {
    
    tw.geo <-
      tweets %>%
        dplyr::select(id_str, user_id_str, geo) %>%
        purrr::pluck('geo') %>%
        tibble::enframe() %>%
        dplyr::filter(! is.na(value)) %>%
        tidyr::unnest_wider(value) %>%
        purrr::pluck('coordinates') %>%
        tibble::enframe() %>%
        tidyr::unnest_wider(value)
    
    if (length(tw.geo) == 1) {
      
      tw.geo <- list()
      
    } else {
      
      tw.geo %<>%
        magrittr::set_colnames(c('name', 'lat', 'long')) %>%
        na.omit()
      
      tw.geo$id_str <- dplyr::pull(tweets[tw.geo$name, 'id_str'])
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
#' @import dplyr
#' @importFrom tidyr unnest_wider unnest
#' @importFrom tibble enframe
#' @importFrom purrr pluck
#' @keywords internal
#' @param users Users dataframe
#' @noRd

usr_entity_clean <- function(users) {
  
  entities_usr <-
    users %>%
      purrr::pluck('entities') %>%
      tibble::enframe() %>%
      tidyr::unnest_wider(value)
  
  if (any(names(entities_usr) == 'url')) {
    
    user.url_ <-
      entities_usr %>%
        dplyr::select(name, url) %>%
        dplyr::rename(rowID = name)
    
    user.url <-
      user.url_ %>%
        purrr::pluck('url') %>%
        tibble::enframe('rowID')
    
    user.url$value <- lapply(user.url$value, function(e) { if (purrr::is_empty(e)) NA else e })
    user.url %<>%
      tidyr::unnest(value) %>%
      purrr::pluck('value') %>%
      tibble::enframe('rowID')
    
    user.url$value <- lapply(user.url$value, function(e) { if (purrr::is_empty(e)) NA else e })
    user.url %<>%
      tidyr::unnest(value) %>%
      tidyr::unnest_wider(value) %>%
      dplyr::mutate(
        rowID      = user.url_$rowID,
        usr_id_str = dplyr::pull(users[rowID, "id_str"])
      ) %>%
      dplyr::select(- c(indices, rowID))
    
  } else {
    
    user.url <- list()
    
  }
  return(user.url)
  
}