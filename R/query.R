#' @noRd
query <- function(query, .lat, .long, .radius, .place, .since, .until, .from, .to, .replies, .minLikes,
                  .minReplies, .minRetweets, .verified, .hasImage, .hasVideo, .hasMedia, .hasLinks, .url) {
  
  config <- list()
  
  .sTerm = as.character(query)  #default to NULL
  sTerm  = if (is.na(.sTerm)) '' else .sTerm
  
  #Geocode
  lat    <- as.character(.lat) #"33.575692"
  long   <- as.character(.long) #"-7.625285"
  radius <- as.character(.radius) #"100km"
  
  geo <- c()
  
  config$geo$lat    <- lat
  config$geo$long   <- long
  config$geo$radius <- radius
  index             <- which(is.na(config$geo))
  
  if (na.tools::all_na(config$geo)) {
    geo <- ''
  } else if (length(index) != 0) {
    msg <- if (length(index) > 1) 'are' else 'is'
    cat(paste(shQuote(names(config$geo[index])), collapse = " and "), msg, 'missing', fill = T)
    geo <- ''
  } else {
    geo <- glue::glue("geocode:{config$geo$lat},{config$geo$long},{config$geo$radius}")
    
  }
  
  place_ = as.character(.place) #default to NA
  place  = if (is.na(place_)) '' else { paste0("near:", place_) }
  
  until.date <- as.character(.until)
  if (! is.na(until.date)) {
    until.datetime <- paste0(as.Date(until.date) + 1, 'T00:00:01')
    until          <- glue::glue("until:{until.datetime}")
  } else {
    until <- ''
  }
  
  since.date <- as.character(.since)
  if (! is.na(since.date)) {
    since.datetime <- paste0(as.Date(since.date), 'T00:00:01')
    since          <- glue::glue("since:{since.datetime}")
  } else {
    since <- ''
  }
  
  from_ = as.character(.from) # '@CBCNews' #default NULL
  from  = if (is.na(from_)) '' else paste0('from:', from_)
  
  to_ = as.character(.to) # default NULL
  to  = if (is.na(to_)) '' else paste0('to:', to_)
  
  # Filters
  replies_ = as.logical(toupper(.replies)) #logical
  replies  = if (replies_ == FALSE) '' else { "filter:replies" }
  
  minLikes_ = as.character(.minLikes) #default NULL
  minLikes  = if (is.na(minLikes_)) '' else paste0('min_faves:', minLikes_)
  
  minReplies_ = as.character(.minReplies) #default NULL
  minReplies  = if (is.na(minReplies_)) '' else paste0('min_replies:', minReplies_)
  
  minRetweets_ = as.character(.minRetweets) #default NULL
  minRetweets  = if (is.na(minRetweets_)) '' else paste0('min_retweets:', minRetweets_)
  
  verified_ = as.logical(toupper(.verified)) #logical
  verified  = if (verified_ == FALSE) '' else { "filter:verified" }
  
  hasImage_ <- as.logical(toupper(.hasImage))  #logical
  hasImage  <- if (hasImage_ == FALSE) '' else { "filter:images" }
  
  hasVideo_ <- as.logical(toupper(.hasVideo))  #logical
  hasVideo  <- if (hasVideo_ == FALSE) '' else { "filter:videos" }
  
  hasMedia_ <- as.logical(toupper(.hasMedia))  #logical
  hasMedia  <- if (hasMedia_ == FALSE) '' else { "filter:media" }
  
  hasLinks_ <- as.logical(toupper(.hasLinks))  #logical
  hasLinks  <- if (hasLinks_ == FALSE) '' else { "filter:links" }
  
  url_ = as.character(.url) #default NULL
  url  = if (is.na(url_)) '' else paste0('url:', url_)
  
  q       = paste(sTerm, from, to, until, since, place, geo, minLikes, minReplies, minRetweets, verified, hasImage, hasVideo, hasMedia, hasLinks, url)
  q.clean = stringr::str_replace_all(q, "\\s{2,}", " ") %>% stringr::str_trim("both")
  
  return(q.clean)
  
}