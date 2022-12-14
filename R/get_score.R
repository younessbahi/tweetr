#' Get topics and, influence authors scores based on a search term or a hashtag.
#'
#' @param keyword Your target search-term or hashtag to get the influence score of similar topics and users.

#' @importFrom na.tools all_na
#' @importFrom operator.tools %!in%
#' @importFrom magrittr %<>% %>%
#' @importFrom purrr map_depth pluck is_empty
#' @importFrom tibble enframe
#' @importFrom tidyr unnest_wider
#' @import dplyr
#' @importFrom glue glue
#' @import chromote
#'
#' @return A dataframe.
#' @export

get_score <- function(keyword) {
  
  ScoreTbl <- score_(keyword)

  if (purrr::is_empty(ScoreTbl$topics)) {
    topicScoreTbl <- list()
  } else {
    topicScoreTbl <- # tidy topic table
      ScoreTbl %>%
        purrr::pluck('topics') %>%
        tibble::enframe(name = "rowID") %>%
        tidyr::unnest_wider(value) %>%
        dplyr::select(- c(tokens, inline))
    
    if (any('result_context' %!in% names(topicScoreTbl))) {
      topicScoreTbl %<>% dplyr::mutate(result_context = NA)
    }
    
    topicScoreTbl$context.type <-
      if (all_na(topicScoreTbl$result_context)) '' else {
        purrr::map_depth(
          topicScoreTbl$result_context, 1, ~ .$types %>% unlist(use.names = F)
        ) %>% #list to vec
          lapply(., function(e) { if (purrr::is_empty(e) | is.null(e)) NA else e }) %>%
          unlist()
      }
    
    topicScoreTbl$context.string <-
      if (all_na(topicScoreTbl$result_context)) '' else {
        purrr::map_depth(
          topicScoreTbl$result_context, 1, ~ .$display_string %>% unlist(use.names = F)
        ) %>% #list to vec
          lapply(., function(e) { if (purrr::is_empty(e) | is.null(e)) NA else e }) %>%
          unlist()
      }
    
    topicScoreTbl %<>%
      select(- result_context) %>%
      relocate(rowID, topic, rounded_score, context.type, context.string) %>%
      arrange(desc(rounded_score)) %>%
      mutate(time = Sys.time())
}
  
  
  if (purrr::is_empty(ScoreTbl$users)) {
    userScoreTbl <- list()
  } else {
    userScoreTbl <-
      ScoreTbl %>%
        purrr::pluck('users') %>%
        tibble::enframe(name = "rowID") %>%
        unnest_wider(value)
    
    userScoreTbl$tokens <-
      userScoreTbl %>%
        purrr::pluck('tokens') %>%
        tibble::enframe(name = "rowID") %>%
        unnest(value) %>%
        mutate(value = unlist(value, use.names = F)) %>%
        group_by(rowID) %>%
        summarise(
          tokens = list(value)
        ) %>%
        pull(tokens)
  
    if (any('result_context' %!in% names(userScoreTbl))) {
      userScoreTbl %<>% mutate(result_context = NA)
    }
    
    userScoreTbl$context.type <-
      if (all_na(userScoreTbl$result_context)) '' else {
        purrr::map_depth(userScoreTbl$result_context, .depth = 1, ~ .$types %>% unlist(use.names = F)) %>% #list to vec
          lapply(., function(e) { if (purrr::is_empty(e) | is.null(e)) NA else e }) %>%
          unlist()
      }
    
    userScoreTbl$context.string <-
      if (all_na(userScoreTbl$result_context)) '' else {
        purrr::map_depth(userScoreTbl$result_context, .depth = 1, ~ .$display_string %>% unlist(use.names = F)) %>% #list to vec
          lapply(., function(e) { if (purrr::is_empty(e) | is.null(e)) NA else e }) %>%
          unlist()
      }
    
    userScoreTbl %<>%
      select(- c(social_context, result_context, inline)) %>%
      relocate(rowID, screen_name, rounded_score) %>%
      arrange(desc(rounded_score)) %>%
      mutate(time = Sys.time())
    
  }
  
  return(
    list(
      query  = ScoreTbl$query,
      count  = ScoreTbl$num_results,
      topics = topicScoreTbl,
      users  = userScoreTbl
    )
  )
}