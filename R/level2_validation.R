# Check nsa and sa are univariate time series of same period
check_nsa_sa_ts <- function(nsa,sa){
  if(!stats::is.ts(nsa)){stop("nsa is not a time series object")}
  if(!stats::is.ts(sa)){stop("sa is not a time series object")}
  if(!is.null(dim(nsa))){stop("nsa series is not a univariate time series")}
  if(!is.null(dim(sa))){stop("sa series is not a univariate time series")}
  if(stats::frequency(nsa)!=stats::frequency(sa)){stop("frequency of nsa and sa series are different")}
  if(!all(stats::start(nsa)==stats::start(sa))){stop("start periods of nsa and sa are different")}
  if(!all(stats::end(nsa)==stats::end(sa))){stop("end periods of nsa and sa are different")}
  if(!any(is.na(nsa))){stop("missing data in nsa time series")}
  if(!any(is.na(sa))){stop("missing data in sa time series")}
}



#Plot nsa and sa

nsa_sa_plot <- function(nsa,sa,title=NULL){
  check_nsa_sa_ts(nsa,sa)

  p <- dplyr::tibble(Date = as.Date(time(sa)),NSA=nsa,SA=sa) |>
    tidyr::pivot_longer(cols=c("NSA","SA"),
                 names_to = "Series",
                 values_to = "Value") |>
    ggplot2::ggplot(aes(x=Date,y=Value,color=Series))+
    ggplot2::geom_line() +
    ggplot2::ggtitle(title)
  return(p)
}


#Plot adjustment factor

adjust_fact_plot <- function(nsa,sa, title = NULL,easter_lag = 6,julian_easter=FALSE, default_type = "X13", default_spec_nsa="RSA2c"){

  check_nsa_sa_ts(nsa,sa)
  if(!default_type%in%c("X13","TS")){stop(paste(default_type),"is not recognised, change to X13 or TS")}
  if(default_type=="X13"){
    nsa_mod <- RJDemetra::x13(nsa,spec = default_spec_nsa)
    decomp_mode <- nsa_mod$decomposition$mode
  }
  if(default_type=="TS"){
    nsa_mod <- RJDemetra::tramoseats(nsa,spec = default_spec_nsa)
    decomp_mode <- nsa_mod$decomposition$mode
  }

  start_yr <- start(nsa)[1]
  end_yr <- end(nsa)[1]

  #  easter_info_q <- dplyr::tibble(easter_date=as.Date(rjd3toolkit::easter_dates(start_yr,end_yr))) |>
  #    mutate(quarter=quarter(easter_date),
  #           year = year(easter_date))

  easter_window_dates <- rjd3toolkit::easter_dates(start_yr,end_yr,julian = julian_easter) |>
    as.Date()|>
    lapply(function(x){x-c(1:easter_lag)}) |>
    unlist() |>
    as.Date()

  easter_info_q <- dplyr::tibble(easter_date = easter_window_dates) |>
    dplyr::mutate(quarter=quarter(easter_date),
           year = year(easter_date)) |>
    dplyr::group_by(year,quarter) |>
    dplyr::summarise(Easter_w=length(easter_date))

  p <- dplyr::tibble(Date = as.Date(time(sa)),NSA=nsa,SA=sa,decomp=decomp_mode) |>
    dplyr::mutate(Adjustment= ifelse(decomp=="Multiplicative",NSA/SA,NSA-SA),
           hline = ifelse(decomp=="Multiplicative",1,0),
           quarter = lubridate::quarter(Date),
           year = lubridate::year(Date),
           Quarter = paste0("Q",quarter)) |>
    tidyr::pivot_longer(cols=c("NSA","SA"),
                 names_to = "Series",
                 values_to = "Value") |>
    dplyr::left_join(easter_info_q)|>
    dplyr::mutate(`Easter window days in quarter` = as.factor(ifelse(is.na(Easter_w),0,Easter_w))) |>
    ggplot2::ggplot(aes(x=Date,y=Adjustment,color = `Easter window days in quarter`))+
      ggplot2::geom_point()+
      ggplot2::geom_line(aes(y=hline),color="black",linetype=2) +
      ggplot2::facet_grid(.~Quarter)+
      ggplot2::ggtitle(paste("Derived adjustment factors for series", title))

  return(p)

}


annual_totals_plot <- function(nsa,sa, title = NULL){

  check_nsa_sa_ts(nsa,sa)

  mid_yr <- mean(c(start(nsa)[1],end(nsa)[1]))
  p <- dplyr::tibble(Date = as.Date(time(sa)),NSA=nsa,SA=sa) |>
    dplyr::mutate(year = lubridate::year(Date),
           quarter = lubridate::quarter(Date))|>
    dplyr::group_by(year) |>
    dplyr::summarise(nsa_total = sum(NSA),
              sa_total = sum(SA),
              full_yr_check = sum(quarter)) |>
    dplyr::filter(full_yr_check == 10)|>
    dplyr::mutate(`Relative difference` = abs(nsa_total-sa_total)/((4/sqrt(length(nsa)))*sqrt(sum(nsa^2))) ) |>
    ggplot2::ggplot(aes(x=year,y=`Relative difference`))+
      ggplot2::geom_hline(yintercept = c(0.05),linetype=1)+
      ggplot2::geom_hline(yintercept = c(0.01),linetype=2)+
      ggplot2::geom_label(x=mid_yr,y=0.01,label = "Level 1 warning threshold")+
      ggplot2::geom_label(x=mid_yr,y=0.05,label = "Bad annual totals")+
      ggplot2::geom_point()+
      ggplot2::ggtitle(paste("Relative difference of NSA and SA annual totals for series",title))

  return(p)
}


cal_effect_plot <- function(nsa,sa,sa_mod, title = NULL, default_type = "X13", default_spec_nsa="RSA2c"){

  check_nsa_sa_ts(nsa,sa)
  if(!default_type%in%c("X13","TS")){stop(paste(default_type),"is not recognised, change to X13 or TS")}
  if(default_type=="X13"){
    nsa_mod <- RJDemetra::x13(nsa,spec = default_spec_nsa)
    decomp_mode <- nsa_mod$decomposition$mode
  }
  if(default_type=="TS"){
    nsa_mod <- RJDemetra::tramoseats(nsa,spec = default_spec_nsa)
    decomp_mode <- nsa_mod$decomposition$mode
  }



  p <- dplyr::tibble(Date = as.Date(time(sa)),
              NSA=nsa,
              SA=sa,
              decomp=decomp_mode,
              `Calendar effect`=sa_mod$user_defined$preprocessing.model.cal) |>
    dplyr::mutate(Adjustment= ifelse(decomp=="Multiplicative",NSA/SA,NSA-SA),
           hline = ifelse(decomp=="Multiplicative",1,0)) |>
    dplyr::select(Date,Adjustment,`Calendar effect`,hline)|>
    tidyr::pivot_longer(cols=c("Adjustment","Calendar effect"),
                 names_to = "Series",
                 values_to = "Value") |>
    ggplot2::ggplot(aes(x=Date,y=Value,color = Series))+
    ggplot2::geom_point()+
    ggplot2::geom_line(aes(y=hline),color="black",linetype=2) +
    ggplot2::ggtitle(paste("Derived adjustment factors and residual calendar effect", title))

  return(p)

}

get_series_to_check <- function(series_name,NSA_df,SA_df){
  start_date <- as.Date(NSA_df[1,1])
  start_yr <- lubridate::year(start_date)
  start_qr <- lubridate::quarter(start_date)

  nsa <- stats::ts(NSA_df[,i],start=c(start_yr,start_qr),frequency = 4)
  sa <- stats::ts(SA_df[,i],start=c(start_yr,start_qr),frequency = 4)
  return(list(name=series_name,
              nsa=nsa,
              sa=sa))
}



level2_validation <- function(nsa,sa,series_name,code_dir,dashboard_template,
                              start_date="1999-01-01",
                              default_type = "X13",
                              default_spec_nsa="RSA2c",
                              default_spec_sa="RSA2c"){
  check_nsa_sa_ts(nsa,sa)

  ts_start <- stats::start(nsa)
  ts_freq <- stats::frequency(nsa)
  quarto::quarto_render(dashboard_template,
                execute_params =  list(
                  code_dir = code_dir,
                  nsa = nsa,
                  sa = sa,
                  name = series_name,
                  ts_start = ts_start,
                  ts_freq = ts_freq,
                  start_date = start_date,
                  default_type = default_type,
                  default_spec_nsa = default_spec_nsa,
                  default_spec_sa = default_spec_sa
                ))
}



