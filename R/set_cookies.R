#' @import devtools
#' @importFrom urltools url_encode
#' @importFrom glue glue
#' @import chromote
#' @keywords internal
#' @noRd

set_cookies_ <- function(q) {
  
  q.parse = urltools::url_encode(q)
  
  tryCatch({
    b                <- chromote::ChromoteSession$new()
    #userAgent <- " Chrome/55.0.2883.87 Safari/537.36"
    userAgent <- readLines('https://raw.githubusercontent.com/younessbahi/agents/main/user-agent.txt')
    b$Network$setUserAgentOverride(userAgent = sample(userAgent, size = 1, replace = T))
  },
    error = function(e) {
      stop("Please restart your session and try again!")
    })
  
  tryCatch({
    b$Page$navigate(glue::glue("https://twitter.com/search?q={q.parse}&src=typed_query&f=live"));
    b$Page$loadEventFired(wait_ = TRUE)
  },
    error   = function(e) {
      stop('INTERNET INTERRUPTED - Please check your internet connection and retry!')
      b$close()
    },
    finally = {
      cookies_ <- b$Network$getCookies()
      cookies_ <- cookies_$cookies
    }
  )
  
  b$close()
  rm(b)
  
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