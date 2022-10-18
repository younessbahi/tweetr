#' @noRd
tw_scrape <- function(count, header, cookies, params) {
  
  empty  = list()
  result = list()
  
  last.cursor = ''
  cursor      = '-1'
  i           = 0
  
  count_     = as.numeric(count)
  pagination = ifelse(count != '-1', ceiling(count_ / 20), '')
  
  if (count != '-1') {
    for (c in seq_along(1:pagination)) {
      i = i + 1
      
      if (i != 1) {
        last.cursor        = cursor
        params[['cursor']] = cursor
        #cat(cursor, fill = T)
      }
      
      res <-
        httr::GET(
          url   = 'https://twitter.com/i/api/2/search/adaptive.json',
          httr::add_headers(.headers = header),
          query = params,
          httr::set_cookies(.cookies = cookies)
        )
      
      res_ <- content(res)
      
      if (i != 1) {
        
        last <-
          length(res_$timeline$instructions)
        
        cursor <-
          res_$
            timeline$
            instructions[[last]]$
            replaceEntry$
            entry$
            content$
            operation$
            cursor$
            value
        
      } else {
        
        last <-
          length(
            res_$
              timeline$
              instructions[[1]]$
              addEntries$
              entries)
        
        cursor <-
          res_$
            timeline$
            instructions[[1]]$
            addEntries$
            entries[[last]]$
            content$
            operation$
            cursor$
            value
        
      }
      
      result[[i]] <- append(res_, empty)
    }
  } else {
    
    while (cursor != last.cursor) {
      
      i = i + 1
      
      if (i != 1) {
        last.cursor        = cursor
        params[['cursor']] = cursor
        #cat(cursor, fill = T)
      }
      
      res <-
        httr::GET(
          url   = 'https://twitter.com/i/api/2/search/adaptive.json',
          httr::add_headers(.headers = header),
          query = params,
          httr::set_cookies(.cookies = cookies)
        )
      
      res_ <- content(res)
      
      if (i != 1) {
        
        last   <-
          length(res_$timeline$instructions)
        
        cursor <-
          res_$
            timeline$
            instructions[[last]]$
            replaceEntry$
            entry$
            content$
            operation$
            cursor$
            value
        
      } else {
        
        last   <-
          length(
            res_$
              timeline$
              instructions[[1]]$
              addEntries$
              entries)
        
        cursor <-
          res_$
            timeline$
            instructions[[1]]$
            addEntries$
            entries[[last]]$
            content$
            operation$
            cursor$
            value
      }
      
      result[[i]] <- append(res_, empty)
      
    }
  }
  
  return(result)
  
}