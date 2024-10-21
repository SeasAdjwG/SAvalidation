#' Example data for validation
#'
#' @name data_to_check
#' @docType data
#' @format list object with name, nsa and sa time series
#' @keywords data
NULL

#' Extract combined test for seasonality on linearised series
#'
#' @param sa_mod An SA object from RJDemetra
#'
#' @return A character vector stating the final result of the combined test for seasonality
#' @export
#'
#' @importFrom stats window time
#' @examples
#' data(data_to_check)
#' nsa <- data_to_check$nsa
#' jd_mod <- RJDemetra::jx13(nsa)
#' get_combined_seasonality_test(jd_mod)
get_combined_seasonality_test <- function(sa_mod){
  if(inherits(sa_mod, "jSA")) {
    result <- RJDemetra::get_indicators(sa_mod, "diagnostics.combined.all.summary")[[1]]
  } else {
    result <- sa_mod$diagnostics$combined_test$combined_seasonality_test
  }
  return(result)
}

#' Extract test for calendar effects
#'
#' @param sa_mod An SA object from RJDemetra
#'
#' @return A logical indicating whether calendar regressors found in the SA model object
#' @export
#'
#' @examples
#' data(data_to_check)
#' nsa <- data_to_check$nsa
#' jd_mod <- RJDemetra::x13(nsa)
#' check_for_calendar_vars(jd_mod)
check_for_calendar_vars <- function(sa_mod){
  if(inherits(sa_mod, "jSA")) {
    regression.coefficients <- RJDemetra::get_indicators(sa_mod, "model.coefficients")[[1]]
    if (!is.null(regression.coefficients)){
      regression.tstat <- regression.coefficients[,1]/regression.coefficients[,2]
      regression.coefficients <- cbind(regression.coefficients,regression.tstat)
      rownames(regression.coefficients) <- RJDemetra::result(jrobj,"model.description")[[1]]
      colnames(regression.coefficients) <- c("Estimate","Std. Error","T-stat")
    }
  } else {
    regression.coefficients <- sa_mod$regarima$regression.coefficients
  }
  regressors <- regression.coefficients |>
    rownames() |>
    tolower()
  has_easter <- length(grep("easter",regressors))!=0
  has_td <- length(grep("day",regressors))!=0
  has_calendar <- any(has_easter,has_td)
  return(has_calendar)
}

#' Check if two series are identical
#'
#' @param nsa A ts object
#' @param sa  A ts object
#'
#' @return Logical TRUE if all(nsa==sa)
#' @export
#'
#' @examples
#' data(data_to_check)
#' check_identical(data_to_check$nsa,data_to_check$sa)
check_identical <- function(nsa,sa){
  check <- all(nsa==sa)
  return(check)
}



#' Check for negatives
#'
#' @param series_to_check A numeric vector (or matrix) to check for any negative values
#'
#' @return Logical TRUE if any negative values exist
#' @export
#'
#' @examples
#' data(data_to_check)
#' check_negatives(data_to_check$nsa)
check_negatives <- function(series_to_check){
  result <- any(series_to_check < 0)
  return(result)
}


#' Check for over adjustment
#'
#' @param sa_mod An SA object from RJDemetra generated with userdefined="preprocessing.model.y_lin"
#' @param pval numeric value used to define a p-value to use as a threshold for accepting/rejecting null hypothesis (default 0.05)
#'
#' @return Logical evidence of
#' @export
#'
#'@description
#'Tests null hypothesis that acf at lag 4 is greater or equal to zero
#'
#' @examples
#' data(data_to_check)
#' sa <- data_to_check$sa
#' jd_mod <- RJDemetra::x13(sa)
#' check_over_adjustment(jd_mod)
check_over_adjustment <- function(sa_mod,pval=0.05){
  if(inherits(sa_mod, "jSA")) {
    y_lin <- RJDemetra::get_indicators(sa_mod, "preprocessing.model.y_lin")[[1]]
  } else {
    y_lin <- sa_mod$regarima$model$effects[,"y_lin"]
  }
  lin_sa <- y_lin |>
    diff()
  n <- length(lin_sa)
  p <- stats::frequency(lin_sa)

  acf_to_lag_p <- stats::acf(lin_sa,lag.max = p,plot=FALSE)
  acf_lag_4 <- acf_to_lag_p$acf[p+1,1,1]


  result <- acf_lag_4 < stats::qnorm(pval)/sqrt(n)
  return(result)
}

#' Level 1 validation check
#'
#' @param nsa A ts object
#' @param sa  A ts object
#' @param default_type Character must be either "X13" (default) or "TS" determining whether X13 or TRAMO-SEATS is used for testing
#' @param default_spec_nsa Character defining the JDemetra+ specification for tests on the nsa series (default="RSA1")
#' @param default_spec_sa Character defining the JDemetra+ specification for tests on the sa series (default="RSA2c")
#'
#' @return Message about the level 1 validation (pass, pass with warnings or fail)
#' @export
#'
#' @examples
#' data(data_to_check)
#' level1_validation(data_to_check$nsa,data_to_check$sa)
level1_validation <- function(nsa,sa, default_type = "X13", default_spec_nsa="RSA1",default_spec_sa="RSA2c"){

  if(!default_type%in%c("X13","TS")){stop("default_type must be `X13` or `TS`")}

  if(default_type=="TS"){
    nsa_mod <- RJDemetra::jtramoseats(nsa,
                                     spec = default_spec_nsa)
    sa_mod <- RJDemetra::jtramoseats(sa,
                                    spec = default_spec_sa)
  }
  if(default_type=="X13"){
    nsa_mod <- RJDemetra::jx13(nsa,
                              spec = default_spec_nsa)
    sa_mod <- RJDemetra::jx13(sa,
                             spec = default_spec_sa)
  }


  is_nsa_seasonal <- tolower(get_combined_seasonality_test(nsa_mod))
  is_nsa_equal_sa <- check_identical(nsa,sa)
  if(is_nsa_seasonal=="none"){
    not_seasonal_message <- ifelse(is_nsa_equal_sa,
                                   "PASS: SERIES HAS NO EVIDENCE OF SEASONALITY AND NSA = SA",
                                   "FAIL: NO EVIDENCE OF SEASONALITY IN NSA BUT SERIES ADJUSTED")
    return(not_seasonal_message)
  } else{
    if(is_nsa_equal_sa){
      return("FAIL: EVIDENCE OF SEASONALITY IN NSA BUT SA IS NOT ADJUSTED")
    }else{
      is_sa_seasonal <- tolower(get_combined_seasonality_test(sa_mod))!="none"
      is_sa_calendar <- check_for_calendar_vars(sa_mod)
      if(is_sa_seasonal|is_sa_calendar){
        return("FAIL: EVIDENCE OF RESIDUAL SEASONALITY OR CALENDAR EFFECTS IN SA SERIES")
      }else{
        max_diff_relative_to_rmse <- rjd3toolkit::compare_annual_totals(nsa,sa)

        has_overadjustment <- check_over_adjustment(sa_mod)


        annual_totals_message <- ifelse(max_diff_relative_to_rmse > 0.01,
                                        "WARNING: ANNUAL TOTALS CHECK FAILED",
                                        "ANNUAL TOTALS CHECK PASSED")
        if (check_negatives(nsa)) {
          has_negatives <- FALSE
          negatives_message <- NULL
        } else {
          has_negatives <- check_negatives(sa)
          if (check_negatives(sa)) {
            negatives_message <- "WARNING: SA SERIES HAS NEGATIVE VALUES"
          } else {
            negatives_message <- "NO NEGATIVE VALUES IN THE SA SERIES"
          }
        }
        over_adjustment_message <- ifelse(has_overadjustment,
                                          "WARNING: SA SERIES HAS EVIDENCE OF OVER-ADJUSTMENT",
                                          "OVER-ADJUSTMENT CHECK PASSED")
        return(c("PASS:",
                 "NSA SERIES HAS EVIDENCE OF SEASONALITY AND NSA != SA",
                 "SA SERIES HAS NO EVIDENCE OF RESIDUAL SEASONAL OR CALENDAR EFFECTS",
                 annual_totals_message,
                 negatives_message,
                 over_adjustment_message))
      }
    }
  }
}


