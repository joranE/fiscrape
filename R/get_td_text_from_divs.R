get_td_text_dst2 <- function(x){
  cn_raw <- x[[1]]
  cn <- c("rank","bib","fisid","name","yob","nation","time","time_back","fispoints")
  df <- x[-1] %>%
    do.call(rbind,.) %>% 
    as.data.frame(.,stringsAsFactors = FALSE)
  colnames(df) <- cn_raw
  
  if (!"Bib" %in% cn_raw){
    df <- df %>%
      mutate(Bib = NA) %>%
      select(Rank,Bib,everything())
  }
  if (!"FIS Points" %in% cn_raw){
    df <- df %>%
      mutate(`FIS Points` = NA)
  }
  if ("BC Points" %in% cn_raw){
    df[["BC Points"]] <- NULL
  }
  if ("SC Points" %in% cn_raw){
    df[["SC Points"]] <- NULL
  }
  if ("NAC Points" %in% cn_raw){
    df[["NAC Points"]] <- NULL
  }
  if ("EEC Points" %in% cn_raw){
    df[["EEC Points"]] <- NULL
  }
  if ("Cup Points" %in% cn_raw){
    df[["Cup Points"]] <- NULL
  }
  if ("OPA Points" %in% cn_raw){
    df[["OPA Points"]] <- NULL
  }
  colnames(df) <- cn
  df
}

get_td_text_pur2 <- function(x){
  cn_raw <- x[[1]]
  cn <- c("rank","bib","fisid","name","yob","nation","time",
          "time_back","fispoints_time","fispoints_rank","fispoints")
  df <- x[-1] %>%
    do.call(rbind,.) %>% 
    as.data.frame(.,stringsAsFactors = FALSE)
  colnames(df) <- cn_raw
  
  if (!"Bib" %in% cn_raw){
    df <- df %>%
      mutate(Bib = NA) %>%
      select(Rank,Bib,everything())
  }
  if (!"FIS Points" %in% cn_raw){
    df <- df %>%
      mutate(`FIS Points` = NA)
  }
  if ("NAC Points" %in% cn_raw){
    df[["NAC Points"]] <- NULL
  }
  if ("EEC Points" %in% cn_raw){
    df[["EEC Points"]] <- NULL
  }
  if ("Cup Points" %in% cn_raw){
    df[["Cup Points"]] <- NULL
  }
  if ("OPA Points" %in% cn_raw){
    df[["OPA Points"]] <- NULL
  }
  colnames(df) <- cn
  df
}

get_td_text_spr_final2 <- function(x){
  cn_raw <- x[[1]]
  cn <- c("rank","bib","fisid","name","yob","nation")
  df <- x[-1] %>%
    do.call(rbind,.) %>% 
    as.data.frame(.,stringsAsFactors = FALSE)
  colnames(df) <- cn_raw
  
  if (!"Bib" %in% cn_raw){
    df <- df %>%
      mutate(Bib = NA) %>%
      select(Rank,Bib,everything())
  }
  if ("NAC Points" %in% cn_raw){
    df[["NAC Points"]] <- NULL
  }
  if ("EEC Points" %in% cn_raw){
    df[["EEC Points"]] <- NULL
  }
  if ("Cup Points" %in% cn_raw){
    df[["Cup Points"]] <- NULL
  }
  if ("OPA Points" %in% cn_raw){
    df[["OPA Points"]] <- NULL
  }
  if ("Time" %in% cn_raw){
    df[["Time"]] <- NULL
  }
  if ("Diff. Time" %in% cn_raw){
    df[["Diff. Time"]] <- NULL
  }
  
  colnames(df) <- cn
  df
}

get_td_text_stage2 <- function(x){
  cn_raw <- x[[1]]
  cn <- c("rank","bib","fisid","name","yob","nation","time","time_back","fispoints")
  df <- x[-1] %>%
    do.call(rbind,.) %>% 
    as.data.frame(.,stringsAsFactors = FALSE)
  colnames(df) <- cn_raw
  
  if (!"Bib" %in% cn_raw){
    df <- df %>%
      mutate(Bib = NA) %>%
      select(Rank,Bib,everything())
  }
  if (!"FIS Points" %in% cn_raw){
    df <- df %>%
      mutate(`FIS Points` = NA)
  }
  colnames(df) <- cn
  df
}