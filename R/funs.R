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
#' @importFrom utils  tail
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
      tw.urls %<>% dplyr::select(- indices)
    }
    
    tw.urls %<>% dplyr::filter(! is.na(expanded_url))
  }
  
  ##Mentions
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
  
  ##Photos (deprecated)
  # if ('media' %in% names(entities)) {
  #
  #   photos <-
  #     entities %>%
  #       dplyr::select(name, media) %>%
  #       dplyr::rename(rowID = name)
  #
  #   photos$media <- lapply(photos$media, function(e) { if (purrr::is_empty(e)) NA else e })
  #
  #   photos <-
  #     tidyr::unnest(photos, cols = 'media')
  #
  #   photos_ <-
  #     photos %>%
  #       purrr::pluck('media') %>%
  #       tibble::enframe('rowID') %>%
  #       dplyr::mutate(rowID = photos$rowID) %>%
  #       tidyr::unnest_wider(value)
  #
  #   photos_$id_tweet <- pull(tweets[photos_$rowID, 'id_str'])
  #   photos           <-
  #     photos_ %>%
  #       dplyr::select(- c(indices, original_info, sizes)) %>%
  #       dplyr::filter(! is.na(id_str))
  #
  # } else {
  #
  #   photos <- list()
  #
  # }
  
  ##Media
  if ('extended_entities' %in% names(tweets)) {
    
    media <- tweets %>%
      purrr::pluck('extended_entities') %>%
      tibble::enframe('row_id') %>%
      tidyr::unnest_wider(value)
    
    media$media <- lapply(media$media, function(e) { if (purrr::is_empty(e) | is.null(e))  NA else e })
    
    media %<>%
      filter(! is.na(media)) %>%
      #purrr::pluck('media') %>%
      #tibble::enframe('row_id') %>%
      tidyr::unnest(media) %>%
      tidyr::unnest_wider(media)
    
    #Media views count
    #todo:task 2 failed - "Can't subset columns that don't exist.✖ Column `viewCount` doesn't exist."
    viewCount = media[, 'ext'] %>%
      tidyr::unnest(cols = ext) %>%
      tidyr::unnest_wider(ext) %>%
      dplyr::select(r)
    
    viewCount$r <- lapply(viewCount$r, function(c) { if (is.character(c)) NA else c })
    viewCount %<>% purrr::pluck('r') %>%
      tibble::enframe('rowID') %>%
      tidyr::unnest(value) %>%
      tidyr::unnest_wider(value)
    
    if ('viewCount' %in% colnames(viewCount)) {
      media %<>% cbind(., viewCount['viewCount']) %>% dplyr::select(- ext)
    }
    
    #Videos info
    if ('video_info' %in% colnames(media)) {
      media$video_info <- lapply(media$video_info, function(e) { if (is.null(e)) list() else e })
      videoInfo        <- media %>%
        purrr::pluck('video_info') %>%
        tibble::enframe('rowID') %>%
        tidyr::unnest_wider(value)
      
      videoUrl <- videoInfo %>%
        purrr::pluck('variants') %>%
        tibble::enframe('rowID') %>%
        tidyr::unnest(value) %>%
        dplyr::rowwise() %>%
        dplyr::filter(length(value) != 2) %>%
        dplyr::group_by(rowID) %>%
        dplyr::summarise(info = tail(value, n = 1)) %>%
        tidyr::unnest_wider(info) %>%
        dplyr::ungroup()
      
      videoInfo %<>% dplyr::left_join(., videoUrl, by = 'rowID') %>%
        dplyr::select(- rowID, - aspect_ratio, - variants) %>%
        dplyr::rename(url_video = url)
      
      media %<>% cbind(., videoInfo) %>%
        mutate(tw_id_str = tweets[["id_str"]][row_id]) %>%
        dplyr::select(- video_info, - row_id)
    }
    
    media_names <- c(
      "id", "id_str", "indices", "media_url", "media_url_https", "url", "display_url", "expanded_url", "type", "original_info", "sizes", "media_key", "ext_sensitive_media_warning", "ext_media_availability", "ext_alt_text",
      "ext_media_color", "additional_media_info", "source_status_id", "source_status_id_str", "source_user_id", "source_user_id_str", "viewCount", "duration_millis", "bitrate", "content_type", "url_video", "tw_id_str"
    )
    
    if (any(media_names %!in% colnames(media))) {
      vars_ <- media_names[which(media_names %!in% colnames(media))]
      media <- cbind(media, setNames(lapply(vars_, function(x) x = NA), vars_)) %>%
        select(media_names)
    }
    
    
  } else {
    
    media <- list()
    
  }
  
  ##GEO
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
        stats::na.omit()
      
      tw.geo$id_str <- dplyr::pull(tweets[tw.geo$name, 'id_str'])
    }
  }
  
  
  return(
    list(
      hashtags = unique(hashtags),
      tw.urls  = unique(tw.urls),
      mentions = unique(mentions),
      media    = unique(media),
      geo      = unique(tw.geo)
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
      dplyr::select(- c(indices, rowID)) %>%
      dplyr::filter(! is.na(url)) %>%
      unique()
    
  } else {
    
    user.url <- list()
    
  }
  return(user.url)
  
}