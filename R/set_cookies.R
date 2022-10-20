#' @import devtools
#' @rdname get_tweets
#' @rdname get_trends
#' @rdname get_score
#' @keywords internal
#' @noRd

set_cookies <- function(q) {
  
  q.parse = urltools::url_encode(q)
  
  #chromote::set_chrome_args(c('--disable-gpu', '--disable-dev-shm-usage', '--no-sandbox'))
  
  tryCatch(
  {
    chromote::ChromoteSession$new
  },
    error   = function(e) {
      stop("Please try again!")
    },
    finally = {
      b                <- chromote::ChromoteSession$new()
      userAgent_mobile <- " Chrome/55.0.2883.87 Safari/537.36"
      b$Network$setUserAgentOverride(userAgent = userAgent_mobile)
      tryCatch(
      {
        b$Page$navigate(glue::glue("https://twitter.com/search?q={q.parse}&src=typed_query&f=live"));
        b$Page$loadEventFired(wait_ = TRUE)
      },
        error   = function(e) {
          stop('INTERNET DISCONNECTED - Please check your internet connection and retry!')
        },
        finally = {
          cookies_ <- b$Network$getCookies()
          cookies_ <- cookies_$cookies
        }
      )
    }
  )
  
  #b$close()
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