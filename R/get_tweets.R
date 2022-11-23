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
#' @importFrom crayon green bold yellow
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
#' @param replies Set to false by default. if it's true then it will fetch tweets replies only.
#' @param minLikes Tweets with at least `(n)` likes should be filtered.
#' @param minReplies	Tweets with at least `(n)` replies should be filtered.
#' @param minRetweets Tweets with at least `(n)` retweets should be filtered.
#' @param verified If true, only tweets from verified accounts will be filtered.
#' @param hasImage If true, only tweets with images should be filtered.
#' @param hasVideo If true, Only tweets with videos should be filtered.
#' @param hasMedia If true, Only tweets with medias should be filtered.
#' @param hasLinks If true, Only tweets with links should be filtered.
#' @param url 	Get tweets that only contain links to specific domain names (ex: Oscars.org); Alternatively, you may type in "oscars" to get all tweets with urls that mention the Oscars term.
#' @return A list.
#'
#' @export

get_tweets <-
  function(query = NA, lat = NA, long = NA, radius = NA, place = NA, lang = NULL, since = NA, until = NA, from = NA, to = NA,
           replies = F, minLikes = NA, minReplies = NA, minRetweets = NA, verified = F, hasImage = F, hasVideo = F,
           hasMedia = F, hasLinks = F, url = NA, count = '-1') {
    
    q.clean_ <- tweetr:::query(query, lat, long, radius, lang, place, since, until, from, to, replies, minLikes,
                               minReplies, minRetweets, verified, hasImage, hasVideo, hasMedia, hasLinks, url)
    
    #colRm <- load('data/colRm.rda')
    
    q.parse_ = urltools::url_encode(q.clean_)
    
    cookies <- tweetr:::set_cookies(q = q.parse_)
    header  <- tweetr:::header_tweets(cookies, q = q.parse_)
    
    params <- list(
      `include_profile_interstitial_type`    = '1',
      #`include_blocking`                    = '1',
      # `include_blocked_by`                 = '1',
      `include_followed_by`                  = '1',
      #`include_want_retweets`               = '1',
      #`include_mute_edge` = '1',
      #`include_can_dm`                      = '1',
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
      `count`                                = '60',
      `cursor`                               = '-1',
      `query_source`                         = 'typeahead_click',
      `pc`                                   = '1',
      `spelling_corrections`                 = '1',
      `include_ext_edit_control`             = 'true',
      `ext`                                  = 'mediaStats,highlightedLabel,hasNftAvatar,voiceInfo,enrichments,superFollowMetadata,unmentionInfo,editControl,collab_control,vibe'
    )
    
    cat(crayon::yellow(crayon::bold("Process initiated...\n")))
    
    res <- tweetr:::tw_scrape(count = count, header, cookies, params)
    
    res.tidy <-
      res %>%
        pluck() %>%
        enframe('rowID') %>%
        unnest_wider(value) %>%
        select(- timeline) %>%
        unnest_wider(globalObjects) %>%
        select(rowID, tweets, users)
    
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
        tweetr:::tidy_(res.tidy$tweets) %>%
          mutate(
            at_GMT_time = tweetr:::parse_datetime(created_at) + 3600,
            at_UTC_time = tweetr:::parse_datetime(created_at)
          )
      
      tw_entity <- suppressMessages(tweetr:::tw_entity_clean(tweets = tw.list))
      
      tw.list %<>%
        select(- c(rowID, created_at, entities, ext, ext_edit_control)) %>%
        arrange(desc(at_GMT_time)) %>%
        relocate(at_GMT_time, at_UTC_time)
      
      if (any(names(tw.list) == 'display_text_range')) {
        tw.list %<>% select(- display_text_range)
      }
      if (any(names(tw.list) == 'extended_entities')) {
        tw.list %<>% select(- extended_entities)
      }
    }
    
    if (all_na(res.tidy$users)) {
      
      users.list <- list()
      user.url   <- list()
      
    } else {
      users.list <-
        tweetr:::tidy_(res.tidy$users) %>%
          mutate(
            created_at = tweetr:::parse_datetime(created_at) + 3600
          )
      
      index_rm <- cRm[which(cRm$to_rm %in% names(users.list)),]$to_rm
      users.list %<>% select(- all_of(index_rm))
      user.url <- suppressMessages(tweetr:::usr_entity_clean(users = users.list))
      users.list %<>% select(- entities)
    }
    
    cat('\n')
    cat('🍿', crayon::green(crayon::bold('Successful')), fill = T)
    
    return(
      list(
        tweets_count       = nrow(tw.list),
        unique_users_count = length(unique(users.list$id_str)),
        tweets             = list(
          items    = tw.list,
          hashtags = tw_entity$hashtags,
          mentions = tw_entity$mentions,
          urls     = tw_entity$tw.urls,
          medias   = tw_entity$tw.media,
          geo      = tw_entity$geo
        ),
        users              = list(
          items = users.list,
          url   = user.url)
      )
    )
    
  }