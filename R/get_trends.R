#' Get trends
#'
#' @importFrom purrr pluck is_empty
#' @importFrom tibble enframe
#' @import chromote
#' @importFrom tidyr unnest_wider unnest
#' @importFrom dplyr relocate select arrange mutate rename pull filter
#' @import httr
#' @importFrom magrittr %<>% %>%
#'
#' @param id Location id, Default to 1 **worldwide**. Get list of available locations from `data('locID', package = 'tweetr')`.
#' @return A dataframe.
#' @examples
#' get_trends('1')
#' @export

get_trends <- function(id = '1') {
  
  t <- tweetr::trends_(id)
  trend.df <-
    t[[1]] %>%
      pluck('trends') %>%
      enframe(name = "rowID") %>%
      unnest_wider(value) %>%
      arrange(desc(tweet_volume)) %>%
      mutate(time = Sys.time()) # track ranking overtime
  
  return(trend.df)

}

#todo: add get_location()