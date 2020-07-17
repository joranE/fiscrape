ms_hms_fun <- function(i,tm){
  fun_list <- list(function(x) as.numeric(x),
                   compose(lubridate::period_to_seconds,lubridate::ms),
                   compose(lubridate::period_to_seconds,lubridate::hms))
  fun_list[[i]](tm)
}

time_to_seconds <- function(times){
  times[is.na(times)] <- ""
  times <- stringr::str_replace_all(string = times,c("[hms]" = "","^:" = "","," = "\\."))
  tcd <- trailing_colon_digits(times)
  if (any(tcd >= 60)){
    message("Detected MM:SS:ss format and converting to MM:SS.ss...")
    times <- stringi::stri_replace_last_fixed(str = times,pattern = ":",replacement = ".")
  }
  colon_count <- stringr::str_count(times,":") + 1
  secs <- purrr::map2_dbl(.x = colon_count,.y = times,.f = ms_hms_fun)
  secs
}

trailing_colon_digits <- function(times){
  x <- times[!is.na(times) & times != ""]
  digits <- stringr::str_extract(string = x,pattern = ":[^:]+$")
  digits <- as.numeric(gsub(pattern = ":",replacement = "",x = digits))
  digits
}