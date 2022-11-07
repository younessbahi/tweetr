#' @keywords internal
#' @noRd

header_trends <- function(data, q) {
  
  q.parse = urltools::url_encode(q)
  
  headers <- c(
    `authority`                 = 'twitter.com',
    `accept`                    = '*/*',
    `accept-language`           = 'en-US,en;q=0.9,fr;q=0.8',
    `authorization`             = 'Bearer AAAAAAAAAAAAAAAAAAAAANRILgAAAAAAnNwIzUejRCOuH5E6I8xnZz4puTs%3D1Zv7ttfk8LF81IUq16cHjhLTvJu4FA33AGWWjCpTnA',
    `referer`                   = glue::glue('https://twitter.com/search?q={q.parse}&src=typed_query&f=live'),
    `sec-ch-ua`                 = '"Chromium";v="104", " Not A;Brand";v="99", "Google Chrome";v="104"',
    `sec-ch-ua-mobile`          = '?0',
    `sec-ch-ua-platform`        = '"macOS"',
    `sec-fetch-dest`            = 'empty',
    `sec-fetch-mode`            = 'cors',
    `sec-fetch-site`            = 'same-origin',
    `user-agent`                = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/104.0.0.0 Safari/537.36',
    `x-csrf-token`              = data[['ct0']],
    `x-guest-token`             = data[['_ga']],
    `x-twitter-active-user`     = 'yes',
    `x-twitter-client-language` = 'en'
  )
  
  return(headers)
}

header_score <- function(data, q) {
  
  q.parse = urltools::url_encode(q)
  
  headers <- c(
    `authority`                 = 'twitter.com',
    `accept`                    = '*/*',
    `accept-language`           = 'en-US,en;q=0.9,fr;q=0.8',
    `authorization`             = 'Bearer AAAAAAAAAAAAAAAAAAAAANRILgAAAAAAnNwIzUejRCOuH5E6I8xnZz4puTs%3D1Zv7ttfk8LF81IUq16cHjhLTvJu4FA33AGWWjCpTnA',
    `referer`                   = glue::glue('https://twitter.com/search?q={q.parse}&src=typed_query&f=live'),
    `sec-ch-ua`                 = '"Chromium";v="104", " Not A;Brand";v="99", "Google Chrome";v="104"',
    `sec-ch-ua-mobile`          = '?0',
    `sec-ch-ua-platform`        = '"macOS"',
    `sec-fetch-dest`            = 'empty',
    `sec-fetch-mode`            = 'cors',
    `sec-fetch-site`            = 'same-origin',
    `user-agent`                = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/104.0.0.0 Safari/537.36',
    `x-csrf-token`              = data[['ct0']],
    `x-guest-token`             = data[['gt']],
    `x-twitter-active-user`     = 'yes',
    `x-twitter-client-language` = 'en'
  )
  
  return(headers)
}

header_tweets <- function (data, q) {
  
  q.parse = urltools::url_encode(q)
  
  headers = c(
    `authority`                 = 'twitter.com',
    `sec-ch-ua`                 = '";Not A Brand";v="99", "Google Chrome";v="104"',
    `x-twitter-client-language` = 'en',
    `x-csrf-token`              = data[['ct0']],
    `sec-ch-ua-mobile`          = '?0',
    `authorization`             = 'Bearer AAAAAAAAAAAAAAAAAAAAANRILgAAAAAAnNwIzUejRCOuH5E6I8xnZz4puTs%3D1Zv7ttfk8LF81IUq16cHjhLTvJu4FA33AGWWjCpTnA',
    `user-agent`                = 'Mozilla/4.0 (compatible; MSIE 9.0; Windows NT 6.1)',
    `x-guest-token`             = data[['gt']],
    `x-twitter-active-user`     = 'yes',
    `x-twitter-utcoffset`       = '+0000',
    `sec-ch-ua-platform`        = '"macOS"',
    `accept`                    = '*/*',
    `sec-fetch-site`            = 'same-origin',
    `sec-fetch-mode`            = 'websocket',
    `sec-fetch-dest`            = 'empty',
    `referer`                   = glue::glue('https://twitter.com/search?q={q.parse}&src=typeahead_click&f=live'),
    `accept-language`           = 'en-US,en;q=0.9'
    #`content-type` = 'application/x-www-form-urlencoded'
  )
  
  return(headers)
  
}