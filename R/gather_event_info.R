#' @export
gather_event_info <- function(){
  while(TRUE){
    cat("\nEnter race info: \n\n")
    
    # DATE
    dt <- ""
    while (is.na(possibly_date(dt))){
      dt <- readline(prompt = "Date: ")
    }
    
    # GENDER
    gender <- switch(menu(c("Men","Women")),"Men","Women")
    
    # LOCATION
    location <- toupper(readline(prompt = "Location: "))
    
    # Event tags
    primary_tag <- ""
    while (primary_tag == ""){
      primary_tag <- toupper(readline(prompt = "Primary tag: "))
    }
    other_tags <- toupper(readline(prompt = "Other tags: "))
    
    if (toupper(other_tags) %in% c("NA","")){
      other_tags <- NULL
    }
    
    primary_tag <- tolower(primary_tag)
    other_tags_list <- unlist(strsplit(x = tolower(other_tags),split = ","))
    
    # EVENT TYPE
    type <- switch(menu(c("Distance","Sprint","Stage")),
                   "Distance","Sprint","Stage")
    
    # EVENT FORMAT
    if (type == "Distance"){
      format <- switch(menu(c("Interval","Mass","Skiathlon","Pursuit","Pursuit Break")),
                       "Interval","Mass","Skiathlon","Pursuit","Pursuit Break")
    }else{
      format <- NA_character_
    }
    
    # EVENT TECH
    if ((format %in% c("Skiathlon","Pursuit Break") && !is.na(format)) || type == "Stage"){
      tech <- "FC"
    }else{
      if (type == "Sprint" || (type == "Distance" && !is.na(format) && !format %in% c("Skiathlon","Pursuit Break"))){
        tech <- switch(menu(c("Freestyle","Classic","Unknown")),"F","C",NA_character_)
      }else{
        tech <- switch(menu(c("Freestyle","Classic","Classic/Freestyle")),"F","C","FC")
      }
    }
    
    # EVENT LENGTH
    len <- NA_real_
    while (is.na(len)){
      len <- as.numeric(readline(prompt = "Length: "))
    }
    if (len < 0){
      len <- NA_real_
    }
    
    # URLs
    linked_pur_eventid <- NA_integer_
    if (type != "Sprint"){
      url <- readline(prompt = "URL: ")
      
      # LINKED PURSUIT EVENT
      if (!is.na(format) && format %in% c("Pursuit","Pursuit Break")){
        min_dt <- as.character(as.Date(dt) - 2)
        src_event <- tbl(conl,dbplyr::in_schema(options()$fiscrape.schema,"v_event"))
        .gender <- gender
        .primary_tag <- primary_tag
        potential_pur <- src_event %>%
          filter(date >= min_dt & 
                   date <= dt & 
                   event_type != "Stage" & 
                   gender == .gender & 
                   primary_tag == .primary_tag & 
                   format != "Pursuit") %>%
          select(eventid,date,location,site,primary_tag,length,tech,format) %>%
          collect() %>%
          mutate_if(.predicate = bit64::is.integer64,.funs = as.integer) %>%
          arrange(date)
        print(potential_pur)
        
        chcs <- as.character(seq_len(nrow(potential_pur)))
        sel <- select.list(choices = chcs,
                           multiple = FALSE,
                           title = "Which event was the first part of this pursuit? (Type '0' for none.)",
                           graphics = FALSE)
        if (length(sel) > 0 && sel != ""){
          linked_pur_eventid <- potential_pur$eventid[as.integer(sel)]
        }else{
          linked_pur_eventid <- NA_character_
        }
      }
      
      live_url <- readline(prompt = "Live URL: ")
      if (live_url == "") live_url <- NA_character_
      
      url <- list(url = url,
                  live_url = live_url)
    }else{
      url_spr_qual <- readline(prompt = "Qualification URL: ")
      if (url_spr_qual == "") url_spr_qual <- NA_character_
      
      n_finals <- NA
      while (is.na(n_finals)){
        n_finals <- readline(prompt = " Number of Finals: ")
        if (n_finals == "") n_finals <- 1
        n_finals <- as.integer(n_finals)
      }
      
      
      if (n_finals > 0){
        url_spr_fin <- rep(NA_character_,times = n_finals)
        url_spr_fin_heat <- rep(NA_character_,times = n_finals)
        spr_fin_cat <- rep(NA_character_,times = n_finals)
        for (i in seq_len(n_finals)){
          url_spr_fin[i] <- readline(prompt  = sprintf("     Finals URL %s: ",i))
          while (is.na(spr_fin_cat[i]) | spr_fin_cat[i] == ""){
            spr_fin_cat[i] <- toupper(readline(prompt  = sprintf("  Spr Final Cat %s: ",i)))
            if (spr_fin_cat[i] == "") spr_fin_cat[i] <- "SR"
          }
          url_spr_fin_heat[i] <- readline(prompt  = sprintf("Finals Heats URL %s: ",i))
          if (url_spr_fin_heat[i] %in% c("","na","NA")) url_spr_fin_heat[i] <- NA_character_
        }
      }else {
        url_spr_fin <- NA_character_
        url_spr_fin_heat <- NA_character_
        spr_fin_cat <- NA_character_
      }
      
      url <- list(qual = url_spr_qual,
                  final = url_spr_fin,
                  heats = url_spr_fin_heat,
                  cat = spr_fin_cat)
    }
    
    # LINKED STAGE EVENTS
    linked_stg_eventid <- NA_integer_
    if (type == "Stage"){
      #Look for events within the past 2 weeks with the same:
      # -gender
      # -cat1
      # or cat2 = SWC or STG
      min_dt <- as.character(as.Date(dt) - 14)
      src_event <- tbl(conl,dbplyr::in_schema(options()$fiscrape.schema,"v_event"))
      .gender <- gender
      .primary_tag <- primary_tag
      potential_stg <- src_event %>%
        filter(date >= min_dt & 
                 date <= dt & 
                 event_type != "Stage" & 
                 gender == .gender & 
                 primary_tag == .primary_tag) %>%
        select(eventid,date,location,site,primary_tag,length,tech,format) %>%
        collect() %>%
        arrange(date)
      print(potential_stg)
      
      chcs <- c(as.character(seq_len(nrow(potential_stg))),"All")
      sel <- select.list(choices = chcs,
                         multiple = TRUE,
                         title = "Which events were stages of this series? (Type 'enter' for none.)",
                         graphics = FALSE)
      if (length(sel) == 1 && sel == "All"){
        linked_stg_eventid <- potential_stg$eventid
      }
      if (length(sel) >= 1 && "All" %ni% sel){
        linked_stg_eventid <- potential_stg$eventid[as.integer(sel)]
      }
    }
    
    event_info <- list(primary_tag = primary_tag,
                       other_tags = other_tags_list,
                       cat1 = primary_tag,
                       cat2 = NA_character_,
                       location = location,
                       type = type,
                       format = format,
                       tech = tech,
                       length = len,
                       date = dt,
                       gender = gender,
                       url = url,
                       linked_pursuit = linked_pur_eventid,
                       linked_stages = linked_stg_eventid)
    
    cat("\n\nIs this correct?\n")
    cat("\nPrimary tag =",event_info$primary_tag)
    cat("\nOther tags  =",event_info$other_tags)
    cat("\nLocation    =",event_info$location)
    cat("\nDate        =",event_info$date)
    cat("\nGender      =",event_info$gender)
    cat("\nType        =",event_info$type)
    cat("\nFormat      =",event_info$format)
    cat("\nTechnique   =",event_info$tech)
    cat("\nLength      =",event_info$length)
    
    if (type != "Sprint"){
      cat("\nURL         =",event_info$url$url)
      if (!is.na(event_info$url$live_url)){
        cat("\nLive URL  =",event_info$url$live_url)
      }
    }else{
      cat("\nQual URL  =",event_info$url$qual)
      if (length(url$final) == 1 && is.na(url$final)){
        cat("\nFinal URL = None")
      }else {
        for (i in seq_along(url$final)){
          fin_url_prnt <- sprintf("\n%s Final URL%s =",event_info$url$cat[i],i)
          cat(fin_url_prnt,event_info$url$final[i])
          
          heat_url_prnt <- sprintf("\n%s Final Heat URL%s =",event_info$url$cat[i],i)
          cat(heat_url_prnt,event_info$url$heats[i])
        }
      }
    }
    
    if (!is.na(format) && format %in% c("Pursuit","Pursuit Break")){
      cat("\nLinked pursuit event =",event_info$linked_pursuit)
    }
    if (type == "Stage"){
      cat("\nLinked stage events =",paste(event_info$linked_stages,collapse = ", "))
    }
    
    cat("\n")
    result <- menu(c("Correct","Incorrect"))
    if (result == 1){ 
      return(event_info)
    }
  }
}
