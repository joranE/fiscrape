#' Create Updated Version of XC_FAC
#' 
#' Creates an updated version of xc_fac used for
#' standardizing median percent back by gender and
#' race type.
#' 
#' @export
#' @return A data frame
update_xc_fac <- function(){
  results <- tbl(src = options()$statskier_src,"maj_int") %>%
    select(raceid,date,season,type,gender,start,rank,time) %>%
    filter(type == 'Distance') %>%
    collect() %>%
    statskier2::mpb() %>%
    statskier2::standardize_mpb()
  
  u_season <- sort(unique(results$season))
  n_season <- n_distinct(results$season)
  XC_FAC <- setNames(vector("list",n_season),u_season)
  new_season <- paste(substr(u_season[n_season],6,9),
                      as.integer(substr(u_season[n_season],6,9))+1,
                      sep = "-")
  
  for (i in seq_len(n_season)){
    if (i == 1){
      XC_FAC[[i]] <- filter(results,season %in% head(u_season,3))
    }
    if (i == n_season){
      XC_FAC[[i]] <- filter(results,season %in% tail(u_season,3))
    }else{
      XC_FAC[[i]] <- filter(results,season %in% u_season[(i-1):(i+1)])
    }
    
    XC_FAC[[i]] <- XC_FAC[[i]] %>%
      group_by(gender,start) %>%
      do(conv_factor(.))
  }
  
  XC_FAC <- bind_rows(XC_FAC,.id = "season")
  to_append <- filter(XC_FAC,season == max(season))
  to_append$season <- new_season
  
  bind_rows(XC_FAC,to_append)
}

conv_factor <- function(x)
{
  err_fun <- function(qn,vec){
    qq <- quantile(vec,qn,na.rm = TRUE)
    if (qn >= 0.5){
      ind <- which(vec <= qq)
    } else{
      ind <- which(vec >= qq)
    } 
    
    mn <- mean(vec[ind],na.rm = TRUE)
    med <- median(vec[ind],na.rm = TRUE)
    return(abs(mn-med))
  }
  
  sn <- with(x,sign(mean(mpb,na.rm = TRUE) - median(mpb,na.rm = TRUE)))
  int <- switch(sn+2,c(0,0.3),c(0,1),c(0.7,1))
  rs <- optimize(f=err_fun,interval=int,vec=x$mpb)
  
  if (rs$minimum >= 0.5){
    ind <- which(x$mpb <= quantile(x$mpb,rs$minimum,na.rm=TRUE))
  }else{
    ind <- which(x$mpb >= quantile(x$mpb,rs$minimum,na.rm=TRUE))
  }
  mu <- mean(x$mpb[ind],na.rm=TRUE)
  sigma <- sd(x$mpb[ind],na.rm=TRUE)
  return(data.frame(mu=mu,sigma=sigma))
}