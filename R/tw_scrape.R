#' @importFrom crayon blue white bold red yellow green
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @importFrom httr GET add_headers set_cookies content timeout
#' @keywords internal
#' @noRd
tw_scrape <- function(count, header, cookies, params) {
  count_          = as.numeric(count)
  empty           = list()
  result          = list()
  header_         = header
  cookies_        = cookies
  last.cursor     = ''
  cursor          = '-1'
  i               = 0
  iter.count      = 0
  last.iter       = 0
  res             <- list()
  res$status_code <- 200
  
  
  if (count != '-1') {
    while (iter.count < count_) {
      i         = i + 1
      last.iter = iter.count
      
      if (i != 1) {
        last.cursor        = cursor
        params[['cursor']] = cursor
      }
      
      
      res <-
        httr::GET(
          url   = 'https://twitter.com/i/api/2/search/adaptive.json',
          httr::add_headers(.headers = header_),
          query = params,
          httr::set_cookies(.cookies = cookies_)
        )
      
      if (res$status_code == 429) {
        cat('\r')
        cat('waiting for response... (it may take 3 to 5 min)')
        Sys.sleep(10)
        
      } else if (res$status_code == 403) break
  
      if (res$status_code == 200) {
      res_       <- httr::content(res)
      try({ iter.count <- sum(lengths(res_$globalObjects$tweets, use.names = F) != 0) + iter.count }, silent = T)
      
      if (last.iter == iter.count) break
      
      cat('\r')
      cat(
        crayon::blue('So far'),
        crayon::bold(crayon::white(iter.count)),
        crayon::blue('tweet'),
        crayon::blue('| Status:'),
        if (res$status_code == 200) crayon::green(res$status_code) else crayon::red(res$status_code)
      )
      
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
                entries
            )
          
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
  } else {
    
    while (cursor != last.cursor & res$status_code == 200) {
      i = i + 1
      
      if (i != 1) {
        last.iter          = iter.count
        last.cursor        = cursor
        params[['cursor']] = cursor
      }
      
      res <-
        httr::GET(
          url   = 'https://twitter.com/i/api/2/search/adaptive.json',
          httr::add_headers(.headers = header),
          query = params,
          httr::set_cookies(.cookies = cookies)
          #httr::timeout(100)
        )
      
      res_       <- httr::content(res)
      iter.count <- sum(lengths(res_$globalObjects$tweets, use.names = F) != 0) + iter.count
      
      if (last.iter == iter.count & res$status_code == 200) break
      
      cat('\r')
      cat(
        crayon::blue('So far'),
        crayon::bold(crayon::white(iter.count)),
        crayon::blue('tweet'),
        crayon::blue('| Status:'),
        if (res$status_code == 200) crayon::green(res$status_code) else crayon::red(res$status_code)
      )
      if (res$status_code == 200) {
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
  }
  
  return(result)
  
}