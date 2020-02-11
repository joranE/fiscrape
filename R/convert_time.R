ms_hms_fun <- function(i,tm){
  fun_list <- list(function(x) NA_real_,
                   compose(lubridate::period_to_seconds,lubridate::ms),
                   compose(lubridate::period_to_seconds,lubridate::hms))
  fun_list[[i]](tm)
}

time_to_seconds <- function(times){
  times <- stringr::str_replace_all(string = times,c("[hms]" = "","^:" = "","," = "\\."))
  colon_count <- stringr::str_count(times,":") + 1
  secs <- purrr::map2_dbl(.x = colon_count,.y = times,.f = ms_hms_fun)
  secs
}