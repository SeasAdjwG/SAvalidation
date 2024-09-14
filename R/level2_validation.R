#' Check nsa and sa are univariate time series of same period
#'
#' @param nsa ts object
#' @param sa  ts object
#'
#' @return stops if problem with nsa and sa time series
#' @export
#'
#' @examples
#' data(data_to_check)
#' check_nsa_sa_ts(data_to_check$nsa,data_to_check$sa)
check_nsa_sa_ts <- function(nsa,sa){
  if(!stats::is.ts(nsa)){stop("nsa is not a time series object")}
  if(!stats::is.ts(sa)){stop("sa is not a time series object")}
  if(!is.null(dim(nsa))){stop("nsa series is not a univariate time series")}
  if(!is.null(dim(sa))){stop("sa series is not a univariate time series")}
  if(stats::frequency(nsa)!=stats::frequency(sa)){stop("frequency of nsa and sa series are different")}
  if(!all(stats::start(nsa)==stats::start(sa))){stop("start periods of nsa and sa are different")}
  if(!all(stats::end(nsa)==stats::end(sa))){stop("end periods of nsa and sa are different")}
  if(any(is.na(nsa))){stop("missing data in nsa time series")}
  if(any(is.na(sa))){stop("missing data in sa time series")}
}




#' Plot nsa and sa
#'
#' @param nsa ts object (usually unadjusted time series)
#' @param sa  ts object (usually seasonally adjusted time series)
#' @param title Optional title
#'
#' @return ggplot of nsa and sa series
#' @export
#'
#' @examples
#' data(data_to_check)
#' nsa_sa_plot(data_to_check$nsa,data_to_check$sa)
nsa_sa_plot <- function(nsa,sa,title=NULL){
  check_nsa_sa_ts(nsa,sa)

  p <- dplyr::tibble(Date = zoo::as.Date(time(sa)),NSA=nsa,SA=sa) |>
    tidyr::pivot_longer(cols=c("NSA","SA"),
                 names_to = "Series",
                 values_to = "Value") |>
    ggplot2::ggplot(ggplot2::aes(x=Date,y=Value,color=Series))+
    ggplot2::geom_line() +
    ggplot2::ggtitle(title)
  return(p)
}



#' Plot adjustment factor
#'
#' @param nsa ts object (usually unadjusted time series)
#' @param sa ts object (usually seasonally adjusted time series)
#' @param title optional title
#' @param easter_lag integer defining the number of days before Easter Sunday to create the Easter window
#' @param julian_easter logical, should EAster be a Julian Easter, default FALSE
#' @param default_type values should be "X13" or "TS" to define whether X13 or TRAMO-SEATS is used to test for decomposition mode
#' @param default_spec_nsa name of a default JDemetra+ specification to use for determining decomposition mode (default is  "RSA2c")
#'
#' @return A plot of derived adjustment factors
#' @export
#'
#' @examples
#' data(data_to_check, package = "SAvalidation")
#' adjust_fact_plot(data_to_check$nsa,data_to_check$sa)
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

  start_yr <- stats::start(nsa)[1]
  end_yr <- stats::end(nsa)[1]


  easter_window_dates <- rjd3toolkit::easter_dates(start_yr,end_yr,julian = julian_easter) |>
    zoo::as.Date()|>
    lapply(function(x){x-c(1:easter_lag)}) |>
    unlist() |>
    zoo::as.Date()

  easter_info_q <- dplyr::tibble(easter_date = easter_window_dates) |>
    dplyr::mutate(quarter=lubridate::quarter(easter_date),
           year = lubridate::year(easter_date)) |>
    dplyr::group_by(year,quarter) |>
    dplyr::summarise(Easter_w=length(easter_date))

  p <- dplyr::tibble(Date = zoo::as.Date(time(sa)),NSA=nsa,SA=sa,decomp=decomp_mode) |>
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
    ggplot2::ggplot(ggplot2::aes(x=Date,y=Adjustment,color = `Easter window days in quarter`))+
    ggplot2::geom_point()+
    ggplot2::geom_line(ggplot2::aes(y=hline),color="black",linetype=2) +
    ggplot2::facet_grid(.~Quarter)+
    ggplot2::ggtitle(paste("Derived adjustment factors for series", title)) +
    ggplot2::theme(legend.position="bottom") +
    ggplot2::scale_colour_discrete(guide = ggplot2::guide_legend(title.position = "top"))

  return(p)

}


#' Plot of relative difference of annual totals
#'
#' @param nsa ts object (usually unadjusted time series)
#' @param sa ts object (usually seasonally adjusted time series)
#' @param title optional title
#'
#' @return A plot of the relative difference of annual totals between nsa and sa
#' @export
#'
#' @examples
#' data(data_to_check, package = "SAvalidation")
#' annual_totals_plot(data_to_check$nsa,data_to_check$sa)
annual_totals_plot <- function(nsa,sa, title = NULL){

  check_nsa_sa_ts(nsa,sa)

  mid_yr <- mean(c(stats::start(nsa)[1],stats::end(nsa)[1]))
  p <- dplyr::tibble(Date = zoo::as.Date(time(sa)),NSA=nsa,SA=sa) |>
    dplyr::mutate(year = lubridate::year(Date),
           quarter = lubridate::quarter(Date))|>
    dplyr::group_by(year) |>
    dplyr::summarise(nsa_total = sum(NSA),
              sa_total = sum(SA),
              full_yr_check = sum(quarter)) |>
    dplyr::filter(full_yr_check == 10)|>
    dplyr::mutate(`Relative difference` = abs(nsa_total-sa_total)/((4/sqrt(length(nsa)))*sqrt(sum(nsa^2))) ) |>
    ggplot2::ggplot(ggplot2::aes(x=year,y=`Relative difference`))+
      ggplot2::geom_hline(yintercept = c(0.05),linetype=1)+
      ggplot2::geom_hline(yintercept = c(0.01),linetype=2)+
      ggplot2::geom_label(x=mid_yr,y=0.01,label = "Level 1 warning threshold")+
      ggplot2::geom_label(x=mid_yr,y=0.05,label = "Bad annual totals")+
      ggplot2::geom_point()+
      ggplot2::ggtitle(paste("Relative difference of NSA and SA annual totals for series",title))

  return(p)
}


#' Plot of calendar effects
#'
#' @param nsa ts object (usually unadjusted time series)
#' @param sa ts object (usually seasonally adjusted time series)
#' @param sa_mod An SA object from RJDemetra
#' @param title optional title
#' @param default_type values should be "X13" or "TS" to define whether X13 or TRAMO-SEATS is used to test for decomposition mode
#' @param default_spec_nsa name of a default JDemetra+ specification to use for determining decomposition mode (default is  "RSA2c")
#'
#' @return A plot of calendar effects and derived adjusted effects
#' @export
#'
#' @examples
#' test_sa_mod <- RJDemetra::x13(data_to_check$sa,
#' spec="RSA2c",
#' userdefined = RJDemetra::user_defined_variables("X13-ARIMA"))
#' cal_effect_plot(data_to_check$nsa,data_to_check$sa,test_sa_mod)
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



  p <- dplyr::tibble(Date = zoo::as.Date(stats::time(sa)),
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
    ggplot2::ggplot(ggplot2::aes(x=Date,y=Value,color = Series))+
    ggplot2::geom_point()+
    ggplot2::geom_line(ggplot2::aes(y=hline),color="black",linetype=2) +
    ggplot2::ggtitle(paste("Derived adjustment factors and residual calendar effect", title))

  return(p)

}

# get_series_to_check <- function(series_name,NSA_df,SA_df){
#   start_date <- zoo::as.Date(NSA_df[1,1])
#   start_yr <- lubridate::year(start_date)
#   start_qr <- lubridate::quarter(start_date)
#
#   nsa <- stats::ts(NSA_df[,i],start=c(start_yr,start_qr),frequency = 4)
#   sa <- stats::ts(SA_df[,i],start=c(start_yr,start_qr),frequency = 4)
#   return(list(name=series_name,
#               nsa=nsa,
#               sa=sa))
# }



#' Title
#'
#' @param nsa ts object (usually unadjusted time series)
#' @param sa ts object (usually seasonally adjusted time series)
#' @param series_name a name for the time series to be analysed
#' @param dataset_name a name for the dataset
#' @param title title
#' @param output_directory optional output directory for dashboard (default uses getwd())
#' @param dashboard_template name of dashboard template to use
#' @param start_date Character defining start date in format "YYYY-MM-DD"
#' @param default_type values should be "X13" or "TS" to define whether X13 or TRAMO-SEATS is used to test for decomposition mode
#' @param default_spec_nsa name of a default JDemetra+ specification to use for tests on NSA series (default is  "RSA2c")
#' @param default_spec_sa name of a default JDemetra+ specification to use for tests on SA series (default is  "RSA2c")
#' @param java_home JAVA_HOME environment variable
#'
#' @return creates an html dashboard with series name in given output directory
#' @export
#'
#' @examples
#' \donttest{
#' data(data_to_check, package = "SAvalidation")
#' level2_validation(data_to_check$nsa,data_to_check$sa,data_to_check$name)
#' }
level2_validation <- function(nsa,sa,series_name,
                              dataset_name = "National Accounts Main Aggregates",
                              title = series_name,
                              output_directory = NULL,
                              dashboard_template="skeleton.qmd",
                              start_date="1999-01-01",
                              default_type = "X13",
                              default_spec_nsa="RSA2c",
                              default_spec_sa="RSA2c",
                              java_home = Sys.getenv("JAVA_HOME")){
  if(is.null(output_directory)){
    output_directory <- getwd()
  }
  if(!dir.exists(output_directory)){
    dir.create(output_directory,recursive = TRUE)
  }

  dashboard_template_to_copy <- file.path(system.file("rmarkdown/templates/level2_report/skeleton",
                                                      package="SAvalidation"),dashboard_template)
  if(!file.exists(dashboard_template_to_copy)){stop("no dashboard template found of that name in the system files")}

  dashboard_to_create <- file.path(output_directory,paste0(series_name,".qmd"))

  file.copy(dashboard_template_to_copy,dashboard_to_create,overwrite = TRUE)


  check_nsa_sa_ts(nsa,sa)

  ts_start <- stats::start(nsa)
  ts_freq <- stats::frequency(nsa)
  quarto::quarto_render(dashboard_to_create,
                execute_params =  list(
                  nsa = nsa,
                  sa = sa,
                  name = series_name,
                  dataset_name = dataset_name,
                  title = title,
                  ts_start = ts_start,
                  ts_freq = ts_freq,
                  start_date = start_date,
                  default_type = default_type,
                  default_spec_nsa = default_spec_nsa,
                  default_spec_sa = default_spec_sa,
                  java_home = java_home
                ))
}



