#' @importFrom crayon blue white bold red yellow
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @keywords internal
#' @noRd
tw_scrape <- function(count, header, cookies, params) {
  empty  = list()
  result = list()
  
  last.cursor = ''
  cursor      = '-1'
  i           = 0
  iter.count  = 0
  count_      = as.numeric(count)
  
  refresh <- function (iter = 4000, params) {
    #refresh cookies every 4000 iteration
    if (i %% iter == 0) {
      cat('\r');
      cat(crayon::yellow('Refreshing cookies...'))
      q = params[['q']]
      cookies <- set_cookies(q = q)
      header  <- header_tweets(cookies, q = q)
    }
  }
  
  if (count != '-1') {
    pb <- utils::txtProgressBar(0, count_, style = 3, char = '-')
    while (iter.count < count_) {
      
      i = i + 1
      if (i != 1) {
        last.cursor = cursor
        if (length(last.cursor) == 0) break
        params[['cursor']] = cursor
      }
  
      try(refresh(4000, params), silent = T)
      
      res <-
        httr::GET(
          url   = 'https://twitter.com/i/api/2/search/adaptive.json',
          httr::add_headers(.headers = header),
          query = params,
          httr::set_cookies(.cookies = cookies)
        )
      
      res_       <- content(res)
      iter.count <- sum(lengths(res_$globalObjects$tweets, use.names = F) != 0) + iter.count
      if (iter.count > count_) iter.count <- count_
      utils::setTxtProgressBar(pb, iter.count)
      
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
    close(pb)
  } else {

      while (cursor != last.cursor) {
        
        i = i + 1
        
        if (i != 1) {
          last.cursor        = cursor
          params[['cursor']] = cursor
        }
  
        try(refresh(4000, params), silent = T)
        
        res <-
          httr::GET(
            url   = 'https://twitter.com/i/api/2/search/adaptive.json',
            httr::add_headers(.headers = header),
            query = params,
            httr::set_cookies(.cookies = cookies)
          )
        
        res_       <- content(res)
        iter.count <- sum(lengths(res_$globalObjects$tweets, use.names = F) != 0) + iter.count
        cat('\r');
        cat(crayon::blue('So far'), crayon::bold(crayon::white(iter.count)), crayon::blue('tweet'))
        
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
        
        if (length(cursor) == 0)
          break
        result[[i]] <- append(res_, empty)
        
      }
    }
  
  return(result)
  
}