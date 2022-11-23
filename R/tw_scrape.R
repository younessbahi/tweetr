#' @importFrom crayon blue white bold
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @keywords internal
#' @noRd
tw_scrape <- function(count, header, cookies, params) {
  empty  = list()
  result = list()
  
  last.cursor     = ''
  cursor          = '-1'
  i               = 0
  iter.count      = 0
  count_          = as.numeric(count)
  #pagination      = ifelse(count != '-1', ceiling(count_ / 20), '')
  
  
  
  if (count != '-1') {
    pb <- utils::txtProgressBar(0, count_, style = 3, char = '-')
   # for (c in seq_along(1:pagination)) {
    while (iter.count < count_) {
      i = i + 1
      
      if (i != 1) {
        last.cursor = cursor
        if (length(last.cursor) == 0) stop('Internet disconnected')
        params[['cursor']] = cursor
      }
      
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
      utils::setTxtProgressBar(pb, iter.count);close(pb)
      
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
        
      }
      
      res <-
        httr::GET(
          url   = 'https://twitter.com/i/api/2/search/adaptive.json',
          httr::add_headers(.headers = header),
          query = params,
          httr::set_cookies(.cookies = cookies)
        )
      
      res_ <- content(res)
      iter.count <- sum(lengths(res_$globalObjects$tweets, use.names = F) != 0) + iter.count
      Sys.sleep(0.5);
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
      
      result[[i]] <- append(res_, empty)
      
    }
  }
  
  return(result)
  
}