#' @import devtools
#' @importFrom urltools url_encode
#' @importFrom glue glue
#' @import chromote
#' @keywords internal
#' @noRd

set_cookies_ <- function(q, s4, s2) {
  
  q.parse = urltools::url_encode(q)
 
  tryCatch({
    b <- chromote::ChromoteSession$new()
  },
    error = function(e) {
      stop("Session expired. Please restart your session and try again!")
    })
  
  tryCatch({
    userAgent <- glue::glue("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/79.0.3945.{s4} Safari/537.{s2}")
    b$Network$setUserAgentOverride(userAgent = userAgent)
    b$Page$navigate(glue::glue("https://twitter.com/search?q={q.parse}&src=typed_query&f=live"));
    b$Page$loadEventFired(wait_ = TRUE)
  },
    error   = function(e) {
     # stop('INTERNET INTERRUPTED - Please check your internet connection and retry!')
      b$Page$navigate(glue::glue("https://twitter.com/search?q={q.parse}&src=typed_query&f=live"));
      b$Page$loadEventFired(wait_ = FALSE)
    },
    finally = {
      cookies_ <- b$Network$getCookies()
      cookies_ <- cookies_$cookies
    }
  )
  try({
    chromote::Chromote$close()
    b$parent$stop()
    b$close()
    rm(b)
  }, silent = T )
  
  nm       <- unlist(lapply(cookies_, '[[', 1))
  val      <- unlist(lapply(cookies_, '[[', 2))
  cookies_ <- cbind(nm = nm, val = val) %>% as.data.frame()
  
  personalization_id <- cookies_[nm == 'personalization_id', 2]
  guest_id_marketing <- cookies_[nm == 'guest_id_marketing', 2]
  guest_id_ads       <- cookies_[nm == 'guest_id_ads', 2]
  guest_id           <- cookies_[nm == 'guest_id', 2]
  ct0                <- cookies_[nm == 'ct0', 2]
  gt                 <- cookies_[nm == 'gt', 2]
  ga                 <- cookies_[nm == '_ga', 2]
  gid                <- cookies_[nm == '__gid', 2]
  
  cookies_data <- c(
    'personalization_id' = personalization_id,
    'guest_id_marketing' = guest_id_marketing,
    'guest_id_ads'       = guest_id_ads,
    'guest_id'           = guest_id,
    'ct0'                = ct0,
    'gt'                 = gt,
    '_ga'                = ga,
    '_gid'               = gid
  )
  
  return(cookies_data)
}