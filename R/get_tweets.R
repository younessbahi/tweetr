#' Get tweets data from custom search
#'
#' @importFrom urltools url_encode url_decode
#' @importFrom purrr map_depth pluck is_empty
#' @importFrom tibble enframe
#' @import chromote
#' @importFrom stringr str_trim str_replace_all
#' @importFrom tidyr unnest_wider
#' @importFrom na.tools all_na
#' @importFrom dplyr relocate select arrange mutate rename pull filter
#' @import httr
#' @importFrom magrittr %<>% %>% set_colnames
#' @importFrom operator.tools %!in%
#' @importFrom crayon green bold yellow red
#'
#' @param query Can be hashtag, keyword, or a user..
#' @param count 	Default to `-1`, this will collect all tweets related to your search unless you specify a number of tweets to collect. Depending on the tweets volume, the process might take a while.
#' @param lat 	Latitude value to provide for geographical search (ex: `33.575692`); Latitude, longitude, and radius are each necessary parameters for geographical search.
#' @param long Longitude value to provide for geographical search (ex: `-7.625285`); Latitude, longitude, and radius are each necessary parameters for geographical search.
#' @param radius Match tweets with tweet-specific location information that are within a sphere-shaped geographic area. (ex: 100km); Latitude, longitude, and radius are each necessary parameters for geographical search.
#' @param place Match tweets nearby a city or country.
#' @param lang Filter tweets by language (ex: 'en'). Default to `NULL`
#' @param since Filter tweets from a specific date. Allowed date format: `YYYY/MM/DD`
#' @param until 	Filter tweets upto a specific date. Allowed date format: `YYYY/MM/DD`
#' @param from Collect tweets from a user. (ex: `@BillGates`)
#' @param to Collect tweets mentioning or replying to a specific user. (ex: `@BillGates`)
#' @param minLikes Tweets with at least `(n)` likes should be filtered.
#' @param minReplies	Tweets with at least `(n)` replies should be filtered.
#' @param minRetweets Tweets with at least `(n)` retweets should be filtered.
#' @param replies If TRUE, Only replies should be filtered. If set to FALSE, replies will be excluded from the search result.
#' @param verified If TRUE, only tweets from verified accounts will be filtered. If set to FALSE, the verified account will be excluded from the result.
#' @param hasImage If TRUE, only tweets with images should be filtered. If set to FALSE, tweets with images will be excluded from the search result.
#' @param hasVideo If TRUE, Only tweets with videos should be filtered. If set to FALSE, tweets with videos will be excluded from the search result.
#' @param hasMedia If TRUE, Only tweets with medias should be filtered. If set to FALSE, tweets with medias will be excluded from the search result.
#' @param hasLinks If TRUE, Only tweets with links should be filtered. If set to FALSE, tweets with links will be excluded from the search result.
#' @param url 	Get tweets that only contain links to specific domain names (ex: Oscars.org); Alternatively, you may type in "oscars" to get all tweets with urls that mention the Oscars term.
#' @return A list.
#'
#' @export

get_tweets <-
  function(query = NA, lat = NA, long = NA, radius = NA, place = NA, lang = NULL, since = NA, until = NA, from = NA, to = NA,
           replies = 'none', minLikes = NA, minReplies = NA, minRetweets = NA, verified = 'none', hasImage = 'none', hasVideo = 'none',
           hasMedia = 'none', hasLinks = 'none', url = NA, count = '-1') {
    
    q.clean_ <- query(query, lat, long, radius, lang, place, since, until, from, to, replies, minLikes,
                      minReplies, minRetweets, verified, hasImage, hasVideo, hasMedia, hasLinks, url)
    
    #colRm <- load('data/colRm.rda')
    
    q.parse_ = urltools::url_encode(q.clean_)
    #userAgent <- readLines('https://raw.githubusercontent.com/younessbahi/agents/main/user-agent.txt')
    #shuffle   <- sample(userAgent, size = 1, replace = T)
    #cookies   <- set_cookies_(q = q.parse_, ua = shuffle)
    #header    <- header_tweets(cookies, q = q.parse_, ua = shuffle)
    
    s4 <- sample(1111:9999, 1, replace = TRUE)
    s2 <- sample(11:99, 1, replace = TRUE)
    
    cookies <- set_cookies_(q = q.parse_, s4 = s4, s2 = s2)
    header  <- header_tweets(cookies, q = q.parse_, s4 = s4, s2 = s2)
    
    params <- list(
      `include_profile_interstitial_type`    = '1',
      #`include_blocking`                    = '1',
      # `include_blocked_by`                 = '1',
      `include_followed_by`                  = '1',
      #`include_want_retweets`               = '1',
      #`include_mute_edge` = '1',
      `include_can_dm`                       = '1',
      `include_can_media_tag`                = '1',
      `include_ext_has_nft_avatar`           = '1',
      #`skip_status`                         = '1',
      #`cards_platform`                      = 'Web-12',
      #`include_cards`                       = '1',
      `include_ext_alt_text`                 = 'true',
      `include_quote_count`                  = 'true',
      `include_reply_count`                  = '1',
      `tweet_mode`                           = 'extended',
      `include_ext_collab_control`           = 'true',
      `include_entities`                     = 'true',
      `include_user_entities`                = 'true',
      `include_ext_media_color`              = 'true',
      `include_ext_media_availability`       = 'true',
      `include_ext_sensitive_media_warning`  = 'true',
      `include_ext_trusted_friends_metadata` = 'true',
      `send_error_codes`                     = 'true',
      `simple_quoted_tweet`                  = 'true',
      `q`                                    = q.clean_,
      `tweet_search_mode`                    = 'live',
      `count`                                = '20',
      `cursor`                               = '-1',
      #`query_source`                         = 'typeahead_click',
      `query_source`                         = 'typed_query',
      `pc`                                   = '1',
      `spelling_corrections`                 = '1',
      `include_ext_edit_control`             = 'true',
      `requestContext`                       = "launch",
      `include_rts`                          = 'false',
      `ext`                                  = 'mediaStats,highlightedLabel,hasNftAvatar,voiceInfo,enrichments,superFollowMetadata,unmentionInfo,editControl,collab_control,vibe'
    )
    
    cat(crayon::yellow(crayon::bold("Process initiated...\n")))
    
    res <- tw_scrape(count = count, header, cookies, params)
    
    cat('\r')
    cat(crayon::yellow(crayon::bold("Cleaning data...")))
    
    tryCatch({
      res.tidy <-
        res %>%
          purrr::pluck() %>%
          tibble::enframe('rowID') %>%
          tidyr::unnest_wider(value) %>%
          dplyr::select(- timeline) %>%
          tidyr::unnest_wider(globalObjects) %>%
          dplyr::select(rowID, tweets, users)
    },
      error = function(e) {
        
        cat(crayon::red(crayon::bold("\n[unsuccesfull]")), fill = T)
        stop(call. = T)
      }
    )
    
    if (all_na(res.tidy$tweets)) {
      
      tw.list            <- list()
      tw_entity          <- list()
      tw_entity$hashtags <- list()
      tw_entity$mentions <- list()
      tw_entity$tw.urls  <- list()
      tw_entity$tw.media <- list()
      tw_entity$geo      <- list()
      
    } else {
      
      tw.list <-
        tidy_(res.tidy$tweets) %>%
          mutate(
            at_GMT_time = parse_datetime(created_at) + 3600,
            at_UTC_time = parse_datetime(created_at)
          )
      
      tw_entity <- suppressMessages(tw_entity_clean(tweets = tw.list))
      
      tw.list %<>%
        select(- c(rowID, created_at, entities, ext, ext_edit_control)) %>%
        group_by(id_str) %>%
        mutate(
          retweet_count  = max(retweet_count),
          favorite_count = max(favorite_count),
          reply_count    = max(reply_count),
          quote_count    = max(quote_count)
        ) %>%
        ungroup() %>%
        arrange(desc(at_GMT_time)) %>%
        relocate(at_GMT_time, at_UTC_time) %>%
        unique()
      
      if ('display_text_range' %in% names(tw.list)) {
        tw.list %<>% select(- display_text_range)
      }
      if ('extended_entities' %in% names(tw.list)) {
        tw.list %<>% select(- extended_entities)
      }
      tw.list %<>% unique()
    }
    
    if (all_na(res.tidy$users)) {
      
      users.list <- list()
      user.url   <- list()
      
    } else {
      users.list <-
        tidy_(res.tidy$users) %>%
          mutate(
            created_at = parse_datetime(created_at) + 3600
          )
      
      index_rm <- cRm[which(cRm$to_rm %in% names(users.list)),]$to_rm
      users.list %<>% select(- all_of(index_rm))
      user.url <- suppressMessages(usr_entity_clean(users = users.list))
      users.list %<>%
        select(- entities) %>%
        group_by(id_str) %>%
        mutate(
          followers_count        = max(followers_count),
          friends_count          = max(friends_count),
          normal_followers_count = max(normal_followers_count),
          fast_followers_count   = max(fast_followers_count),
          listed_count           = max(listed_count),
          statuses_count         = max(statuses_count),
          media_count            = max(media_count),
          favourites_count       = max(favourites_count)
        ) %>%
        ungroup()
    }
    
    cat('\r');
    cat(crayon::green(crayon::bold('[successful]')), fill = T)
    
    return(
      list(
        tweets_count       = nrow(unique(tw.list)),
        unique_users_count = length(unique(users.list$id_str)),
        tweets             = list(
          items    = tw.list,
          hashtags = tw_entity$hashtags,
          mentions = tw_entity$mentions,
          urls     = tw_entity$tw.urls,
          media    = tw_entity$media,
          geo      = tw_entity$geo
        ),
        users              = list(
          items = unique(users.list),
          url   = user.url)
      )
    )
    rm(list = ls())
    
  }