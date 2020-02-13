#' @export
gather_event_info <- function(){
  while(TRUE){
    cat("\nEnter race info: \n\n")
    
    # DATE
    dt <- ""
    while (!grepl("[0-9]{4}-[0-9]{2}-[0-9]{2}",dt)){
      dt <- readline(prompt = "Date: ")
    }
    
    # GENDER
    gender <- switch(menu(c('Men','Women')),'Men','Women')
    
    # LOCATION
    location <- toupper(readline(prompt = "Location: "))
    
    # CAT1 - CAT2
    cat1 <- ''
    while (cat1 == ''){
      cat1 <- toupper(readline(prompt = "Cat1: "))
    }
    cat2 <- toupper(readline(prompt = "Cat2: "))
    
    if (cat2 == 'JUNIOR'){
      cat2 <- 'Junior'
    }
    if (toupper(cat2) %in% c('NA','')){
      cat2 <- NA_character_
    }
    
    # EVENT TYPE
    type <- switch(menu(c('Distance','Sprint','Stage')),'Distance','Sprint','Stage')
    
    if (type == 'Distance'){
      format <- switch(menu(c('Interval','Mass','Skiathlon','Pursuit')),'Interval','Mass','Skiathlon','Pursuit')
    }else{
      format <- NA_character_
    }
    
    # EVENT FORMAT (Dst)
    if ((format == 'Skiathlon' & !is.na(format)) | type == "Stage"){
      tech <- 'FC'
    }else{
      tech <- switch(menu(c('Freestyle','Classic','Classic/Freestyle')),'F','C','FC')
    }
    
    # EVENT LENGTH
    len <- NA_real_
    while (is.na(len)){
      len <- as.numeric(readline(prompt = "Length: "))
    }
    
    # URLs
    linked_pur_raceid <- NA_integer_
    if (type != 'Sprint'){
      url <- readline(prompt = "URL: ")
      
      # LINKED PURSUIT EVENT
      if (type == "Pursuit"){
        min_dt <- as.character(as.Date(dt) - 2)
        src_event <- tbl(conl,"event")
        .gender <- gender
        .cat1 <- cat1
        potential_pur <- src_event %>%
          filter(date >= min_dt & 
                   date <= dt & 
                   event_type != "Stage" & 
                   gender == .gender & 
                   cat1 == .cat1 & 
                   format != "Pursuit") %>%
          select(raceid,date,location,cat1,cat2,length,tech,format) %>%
          collect() %>%
          arrange(date)
        print(potential_pur)
        
        chcs <- as.character(seq_len(nrow(potential_stg)))
        sel <- select.list(choices = chcs,
                                     multiple = FALSE,
                                     title = "Which event was the first part of this pursuit? (Type 'enter' for none.)",
                                     graphics = FALSE)
        if (length(sel) > 0){
          linked_pur_raceid <- potential_pur$raceid[as.integer(sel)]
        }
      }
      
      live_url <- readline(prompt = "Live URL:")
      if (live_url == "") live_url <- NA_character_
      
      url <- list(url = url,
                  live_url = live_url)
      # if (grepl(pattern = "^https",x = url)){
      #   url <- gsub(pattern = "^https",replacement = "http",x = url)
      # }
    }else{
      url_spr_qual <- readline(prompt = "Qualification URL: ")
      if (url_spr_qual == '') url_spr_qual <- NA_character_
      
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
          }
          url_spr_fin_heat[i] <- readline(prompt  = sprintf("Finals Heats URL %s: ",i))
          if (url_spr_fin_heat[i] == "") url_spr_fin_heat[i] <- NA_character_
        }
      }else {
        url_spr_fin <- NA_character_
        spr_fin_cat <- NA_character_
      }
      
      # if (grepl(pattern = "^https",x = url_spr_qual)){
      #   url_spr_qual <- gsub(pattern = "^https",
      #                   replacement = "http",
      #                   x = url_spr_qual)
      # }
      # 
      # if (grepl(pattern = "^https",x = url_spr_fin)){
      #   url_spr_fin <- gsub(pattern = "^https",
      #                  replacement = "http",
      #                  x = url_spr_fin)
      # }
      
      url <- list(qual = url_spr_qual,
                  final = url_spr_fin,
                  heats = url_spr_fin_heat,
                  cat = spr_fin_cat)
    }
    
    # LINKED STAGE EVENTS
    linked_stg_raceid <- NA_integer_
    if (type == "Stage"){
      #Look for events within the past 2 weeks with the same:
      # -gender
      # -cat1
      # or cat2 = SWC or STG
      min_dt <- as.character(as.Date(dt) - 14)
      src_event <- tbl(conl,"event")
      .gender <- gender
      .cat1 <- cat1
      potential_stg <- src_event %>%
        filter(date >= min_dt & 
                 date <= dt & 
                 event_type != "Stage" & 
                 gender == .gender & 
                 cat1 == .cat1) %>%
        select(raceid,date,location,cat1,cat2,length,tech,format) %>%
        collect() %>%
        arrange(date)
      print(potential_stg)
      
      chcs <- c(as.character(seq_len(nrow(potential_stg))),"All")
      sel <- select.list(choices = chcs,
                         multiple = TRUE,
                         title = "Which events were stages of this series? (Type 'enter' for none.)",
                         graphics = FALSE)
      if (length(sel) == 1 && sel == "All"){
        linked_stg_raceid <- potential_stg$raceid
      }
      if (length(sel) >= 1 && "All" %ni% sel){
        linked_stg_raceid <- potential_stg$raceid[as.integer(sel)]
      }
    }
    
    event_info <- list(cat1 = cat1,
                     cat2 = cat2,
                     location = location,
                     type = type,
                     format = format,
                     tech = tech,
                     length = len,
                     date = dt,
                     gender = gender,
                     url = url,
                     linked_pursuit = linked_pur_raceid,
                     linked_stages = linked_stg_raceid)
    
    cat("\n\nIs this correct?\n")
    cat("\nCat1      =",event_info$cat1)
    cat("\nCat2      =",event_info$cat2)
    cat("\nLocation  =",event_info$location)
    cat("\nDate      =",event_info$date)
    cat("\nGender    =",event_info$gender)
    cat("\nType      =",event_info$type)
    cat("\nFormat    =",event_info$format)
    cat("\nTechnique =",event_info$tech)
    cat("\nLength    =",event_info$length)
    
    if (type != 'Sprint'){
      cat("\nURL       =",event_info$url)
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
    
    if (!is.na(format) & format == "Pursuit"){
      cat("\nLinked pursuit event =",event_info$linked_pursuit)
    }
    if (type == "Stage"){
      cat("\nLinked stage events =",paste(event_info$linked_stages,collapse = ", "))
    }
    
    cat("\n")
    result <- menu(c('Correct','Incorrect'))
    if (result == 1){return(event_info)}
  }
}
