#' @keywords internal
#' @noRd

query <- function(query, lat, long, radius, lang, place, since, until, from, to, replies, minLikes,
                  minReplies, minRetweets, verified, hasImage, hasVideo, hasMedia, hasLinks, url) {
  
  config <- list()
  
  sTerm = as.character(query)  #default to NULL
  sTerm = if (is.na(sTerm)) '' else sTerm
  
  #Geocode
  lat_    <- as.character(lat) #"33.575692"
  long_   <- as.character(long) #"-7.625285"
  radius_ <- as.character(radius) #"100km"
  
  geo <- c()
  
  config$geo$lat_    <- lat_
  config$geo$long_   <- long_
  config$geo$radius_ <- radius_
  index              <- which(is.na(config$geo))
  
  if (na.tools::all_na(config$geo)) {
    geo <- ''
  } else if (length(index) != 0) {
    msg <- if (length(index) > 1) 'are' else 'is'
    cat(paste(shQuote(names(config$geo[index])), collapse = " and "), msg, 'missing', fill = T)
    geo <- ''
  } else {
    geo <- glue::glue("geocode:{config$geo$lat_},{config$geo$long_},{config$geo$radius_}")
    
  }
  
  lan <- if (! is.null(lang)) paste0('lang:', tolower(lang)) else ''
  
  place_ = if (is.na(place)) '' else { paste0("near:", as.character(place)) }
  
  until.date <- as.character(until)
  if (! is.na(until.date)) {
    until.datetime <- paste0(as.Date(until.date) + 1, 'T00:00:01')
    until_         <- glue::glue("until:{until.datetime}")
  } else {
    until_ <- ''
  }
  
  since.date <- as.character(since)
  if (! is.na(since.date)) {
    since.datetime <- paste0(as.Date(since.date), 'T00:00:01')
    since_         <- glue::glue("since:{since.datetime}")
  } else {
    since_ <- ''
  }
  
  from_ = as.character(from) # '@CBCNews' #default NULL
  from_ = if (is.na(from_)) '' else paste0('from:', from_)
  
  to_ = as.character(to) # default NULL
  to_ = if (is.na(to_)) '' else paste0('to:', to_)
  
  # Filters
  replies_ = as.logical(toupper(replies)) #logical
  replies_ = if (replies_ == FALSE) '' else { "filter:replies" }
  
  minLikes_ = as.character(minLikes) #default NULL
  minLikes_ = if (is.na(minLikes_)) '' else paste0('min_faves:', minLikes_)
  
  minReplies_ = as.character(minReplies) #default NULL
  minReplies_ = if (is.na(minReplies_)) '' else paste0('min_replies:', minReplies_)
  
  minRetweets_ = as.character(minRetweets) #default NULL
  minRetweets_ = if (is.na(minRetweets_)) '' else paste0('min_retweets:', minRetweets_)
  
  verified_ = as.logical(toupper(verified)) #logical
  verified_ = if (verified_ == FALSE) '' else { "filter:verified" }
  
  hasImage_ <- as.logical(toupper(hasImage))  #logical
  hasImage_ <- if (hasImage_ == FALSE) '' else { "filter:images" }
  
  hasVideo_ <- as.logical(toupper(hasVideo))  #logical
  hasVideo_ <- if (hasVideo_ == FALSE) '' else { "filter:videos" }
  
  hasMedia_ <- as.logical(toupper(hasMedia))  #logical
  hasMedia_ <- if (hasMedia_ == FALSE) '' else { "filter:media" }
  
  hasLinks_ <- as.logical(toupper(hasLinks))  #logical
  hasLinks_ <- if (hasLinks_ == FALSE) '' else { "filter:links" }
  
  url_ = as.character(url) #default NULL
  url_ = if (is.na(url_)) '' else paste0('url:', url_)
  
  q       = paste(sTerm, from_, to_, until_, since_, lan, place_, geo, minLikes_, minReplies_, minRetweets_, replies_, verified_, hasImage_, hasVideo_, hasMedia_, hasLinks_, url_)
  q.clean = str_replace_all(q, "\\s{2,}", " ") %>% str_trim("both")
  
  return(q.clean)
  
}