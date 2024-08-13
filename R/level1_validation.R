### Extract test for seasonality on linearised series
get_combined_seasonality_test <- function(sa_mod){
  result <- sa_mod$diagnostics$combined_test$combined_seasonality_test
  return(result)
}

### Extract test for calendar effects
check_for_calendar_vars <- function(sa_mod){
  regressors <- sa_mod$regarima$regression.coefficients |>
    rownames() |>
    tolower()
  has_easter <- length(grep("easter",regressors))!=0
  has_td <- length(grep("day",regressors))!=0
  has_calendar <- any(has_easter,has_td)
  return(has_calendar)
}

### Check if two series are identical
check_identical <- function(nsa,sa){
  check <- all(nsa==sa)
  return(check)
}


### Check for negatives
check_negatives <- function(series_to_check){
  result <- any(series_to_check < 0)
  return(result)
}

### Check for over adjustment

check_over_adjustment_qs <- function(sa_mod){
  result <- sa_mod$user_defined$`diagnostics.seas-lin-qs`[2] <= 0.05
  return(result)
}

check_over_adjustment <- function(sa_mod){
  lin_sa <- sa_mod$user_defined$`preprocessing.model.y_lin` |>
    diff()
  n <- length(lin_sa)
  p <- frequency(lin_sa)

  acf_to_lag_p <- acf(lin_sa,lag.max = p,plot=FALSE)
  acf_lag_4 <- acf_to_lag_p$acf[p+1,1,1]


  result <- acf_lag_4 < qnorm(0.025)/sqrt(n)
  return(result)
}

### Level 1 validation check

level1_validation <- function(nsa,sa, default_type = "X13", default_spec_nsa="RSA1",default_spec_sa="RSA2c"){

  if(!default_type%in%c("X13","TS")){stop("default_type must be `X13` or `TS`")}

  if(default_type=="TS"){
    nsa_mod <- RJDemetra::tramoseats(nsa,
                                     spec = default_spec_nsa,
                                     userdefined = user_defined_variables("TRAMO-SEATS"))
    sa_mod <- RJDemetra::tramoseats(sa,
                                    spec = default_spec_sa,
                                    userdefined = user_defined_variables("TRAMO-SEATS"))
  }
  if(default_type=="X13"){
    nsa_mod <- RJDemetra::x13(nsa,
                              spec = default_spec_nsa,
                              userdefined = user_defined_variables("X13-ARIMA"))
    sa_mod <- RJDemetra::x13(sa,
                             spec = default_spec_sa,
                             userdefined = user_defined_variables("X13-ARIMA"))
  }


  is_nsa_seasonal <- tolower(get_combined_seasonality_test(nsa_mod))

  if(is_nsa_seasonal=="none"){
    is_nsa_equal_sa <- check_identical(nsa,sa)
    not_seasonal_message <- ifelse(is_nsa_equal_sa,
                                   "PASS: SERIES HAS NO EVIDENCE OF SEASONALITY AND NSA = SA",
                                   "FAIL: NO EVIDENCE OF SEASONALITY IN NSA BUT SERIES ADJUSTED")
    return(not_seasonal_message)
  }
  else{
    is_nsa_equal_sa <- check_identical(nsa,sa)
    if(is_nsa_equal_sa){
      return("FAIL: EVIDENCE OF SEASONALITY IN NSA BUT SA IS NOT ADJUSTED")
    }
    else{
      is_sa_seasonal <- tolower(get_combined_seasonality_test(sa_mod))!="none"
      is_sa_calendar <- check_for_calendar_vars(sa_mod)
      if(is_sa_seasonal|is_sa_calendar){
        return("FAIL: EVIDENCE OF RESIDUAL SEASONALITY OR CALENDAR EFFECTS IN SA SERIES")
      }
      else{
        max_diff_relative_to_rmse <- rjd3toolkit::compare_annual_totals(nsa,sa)
        has_negatives <- check_negatives(sa)
        has_overadjustment <- check_over_adjustment(sa_mod)


        annual_totals_message <- ifelse(max_diff_relative_to_rmse > 0.05,
                                        "WARNING: ANNUAL TOTALS CHECK FAILED",
                                        "ANNUAL TOTALS CHECK PASSED")
        negatives_message <- ifelse(has_negatives,
                                    "WARNING: SA SERIES HAS NEGATIVE VALUES",
                                    "NON-NEGATIVE CHECK PASSED")
        over_adjustment_message <- ifelse(has_overadjustment,
                                          "WARNING: SA SERIES HAS EVIDENCE OF OVER-ADJUSTMENT",
                                          "OVER-ADJUSTMENT CHECK PASSED")
        return(cat("PASS: \n NSA SERIES HAS EVIDENCE OF SEASONALITY \n SA SERIES HAS NO EVIDENCE OF RESIDUAL SEASONAL OR CALENDAR EFFECTS\n",annual_totals_message,"\n",negatives_message,"\n",over_adjustment_message))
      }
    }
  }
}


