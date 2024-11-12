#' Level 3 validation
#'
#' @param nsa ts object (usually unadjusted time series)
#' @param sa ts object (usually seasonally adjusted time series)
#' @param series_name a name for the time series to be analysed
#' @param vintages a list object with two elements, nsa_vert and sa_vert, each of which is a matrix with columns as dates and rows as vintages
#' @param dataset_name a name for the dataset
#' @param title title
#' @param output_directory optional output directory for dashboard (default uses getwd())
#' @param dashboard_template name of dashboard template to use
#' @param default_type values should be "X13" or "TS" to define whether X13 or TRAMO-SEATS is used to test for decomposition mode
#' @param default_spec_nsa name of a default JDemetra+ specification to use for tests on NSA series (default is  "RSA2c")
#' @param java_home JAVA_HOME environment variable
#'
#' @return creates an html document with series name in given output directory
#' @export
#'
#' @examples
#' \donttest{
#' data(vintages, package = "SAvalidation")
#' nsa <- ts(vintages$nsa_vert[,"2024-01-01"], start=1999, frequency = 4)
#' sa <- ts(vintages$sa_vert[,"2024-01-01"], start=1999, frequency = 4)
#' level3_validation(nsa,sa,"test",vintages)
#' }
level3_validation <- function(nsa,sa,series_name,vintages=NULL,
                              dataset_name = NULL,
                              title = series_name,
                              output_directory = NULL,
                              dashboard_template="skeleton3.qmd",
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

  dashboard_template_to_copy <- file.path(system.file("rmarkdown/templates/level3_report/skeleton",
                                                      package="SAvalidation"),dashboard_template)
  if(!file.exists(dashboard_template_to_copy)){stop("no dashboard template found of that name in the system files")}

  dashboard_to_create <- file.path(output_directory,paste0(series_name,".qmd"))

  file.copy(dashboard_template_to_copy,dashboard_to_create,overwrite = TRUE)

  vint_file <- file.path(output_directory,"vintages.RDS")
  saveRDS(vintages,file = vint_file)

  check_nsa_sa_ts(nsa,sa)

  ts_start <- stats::start(nsa)
  ts_freq <- stats::frequency(nsa)

  quarto::quarto_render(dashboard_to_create,
                        execute_params =  list(
                          nsa = nsa,
                          sa = sa,
                          vintages = vint_file,
                          name = series_name,
                          dataset_name = dataset_name,
                          title = title,
                          ts_start = ts_start,
                          ts_freq = ts_freq,
                          default_type = default_type,
                          default_spec_nsa = default_spec_nsa,
                          default_spec_sa = default_spec_sa,
                          java_home = java_home
                        ))
  file.remove(vint_file)

}



#' Level 3  comparison plot
#'
#'
#' @param nsa ts object (usually unadjusted time series)
#' @param sa ts object (usually seasonally adjusted time series)
#' @param sa_mod An SA object from RJDemetra
#' @param default_spec_nsa name of a default JDemetra+ specification to use for determining decomposition mode (default is  "RSA1")
#' @param test_level significance level for tests (default is 0.01)
#' @param java_home
#'
#' @return a ggplot object showing the published SA series and some approximate confidence intervals
#' @export
#'
#' @examples'
#' data(data_to_check, package = "SAvalidation")
#' level3_comparisons_plot(nsa = SAvalidation::data_to_check$nsa,
#' sa = SAvalidation::data_to_check$sa,
level3_comparisons_plot <- function(nsa,sa,series_name,
                             default_spec_nsa="RSA1",
                             test_level = 0.01,
                             java_home = Sys.getenv("JAVA_HOME")){

  if(any(is.na(nsa))){stop("nsa series has missing data")}
  if(any(is.na(sa))){stop("sa series is missing")}

  if(!is.ts(nsa)){stop(paste("nsa is not a time series it has class",class(nsa),nsa))}
  if(!is.ts(sa)){stop(paste("sa is not a time series it has class",class(sa),sa))}

  nsa_mod <- RJDemetra::tramoseats(nsa,
                                     spec = default_spec_nsa,
                                     userdefined = RJDemetra::user_defined_variables("TRAMO-SEATS"))

  if(nsa_mod$regarima$model$spec_rslt[,"Log transformation"]){
    mod0_spec <- RJDemetra::tramoseats_spec(spec = "RSA0",
                                 transform.function = "Log")
  }else{
    mod0_spec <- RJDemetra::tramoseats_spec(spec = "RSA0")
  }

  nsa_mod_0 <- RJDemetra::tramoseats(nsa,
                                   spec = mod0_spec,
                                   userdefined = RJDemetra::user_defined_variables("TRAMO-SEATS"))

  sa_se_est <- nsa_mod_0$user_defined$decomposition.sa_cmp_e


  data_to_compare <- data.frame("Date" =  zoo::as.Date(time(sa)),
                                "Published" = sa,
                                "Default" = nsa_mod$final$series[,"sa"],
                                "Lower" = nsa_mod$final$series[,"sa"]-qnorm(1-test_level/2)*sa_se_est,
                                "Upper" = nsa_mod$final$series[,"sa"]+qnorm(1-test_level/2)*sa_se_est)

  ggplot2::ggplot(data_to_compare,ggplot2::aes(x=Date,y = Published))+
    ggplot2::geom_line()+
    ggplot2::geom_ribbon(ggplot2::aes(ymin=Lower,ymax=Upper),alpha=0.5)+
    ggplot2::labs(title=series_name)

}


