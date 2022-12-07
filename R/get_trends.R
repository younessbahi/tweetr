#' Get trends
#'
#' @importFrom purrr pluck
#' @importFrom tibble enframe
#' @importFrom tidyr unnest_wider
#' @importFrom dplyr arrange mutate
#' @importFrom magrittr %>%
#'
#' @param id Location id, Default to 1 **worldwide**. Get list of available locations from `data('locID', package = 'tweetr')`.
#' @return A dataframe.
#' @export

get_trends <- function(id = '1') {
  
  t <- trends_(id)
  trend.df <-
    t[[1]] %>%
      purrr::pluck('trends') %>%
      tibble::enframe(name = "rowID") %>%
      tidyr::unnest_wider(value) %>%
      dplyr::arrange(desc(tweet_volume)) %>%
      dplyr::mutate(time = Sys.time()) # track ranking overtime
  
  return(trend.df)

}