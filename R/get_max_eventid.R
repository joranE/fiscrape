#' @export
get_max_eventid <- function(){
  src_dst_event <- dplyr::tbl(src = conl,
                              dbplyr::in_schema(options()$fiscrape.schema,"dst_event"))
  src_spr_event <- dplyr::tbl(src = conl,
                              dbplyr::in_schema(options()$fiscrape.schema,"spr_event"))
  src_stg_event <- dplyr::tbl(src = conl,
                              dbplyr::in_schema(options()$fiscrape.schema,"stg_event"))
  
  mx_dst <- src_dst_event %>%
    filter(eventid == max(eventid,na.rm = TRUE)) %>%
    collect() %>%
    pull(eventid)
  mx_spr <- src_spr_event %>%
    filter(eventid == max(eventid,na.rm = TRUE)) %>%
    collect() %>%
    pull(eventid)
  mx_stg <- src_stg_event %>%
    filter(eventid == max(eventid,na.rm = TRUE)) %>%
    collect() %>%
    pull(eventid)
  
  mx <- max(c(mx_dst,mx_spr,mx_stg),na.rm = TRUE)
  
  if (is.na(mx)){
    stop("Problems finding max eventid.")
  }
  mx
}