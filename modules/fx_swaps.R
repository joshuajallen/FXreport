# # source environmentironment files
# source("\\\\istdba/BuildArchive/ShinyApps/EnvironmentScript/EnvironmentScript_Latest/LOCAL/Artifactory/environment.R")
# 
# # boe checkpoint
# checkpoint_date <- "2022-01-01"
# message(paste0("checkpoint_date: ", checkpoint_date))
# boeCheckpoint(checkpoint_date, scanForPackages = FALSE)
# 
# # load packages -----------------------------------------------------
# 
# 
# library(DT)
# library(formattable)
# library(flexdashboard)
# library(httr)
# library(lubridate)
# library(purrr)
# library(plotly)
# library(scales)
# library(stringr)
# library(tidyr)
# library(zoo)
# 
# # Load a library so that boeCheckpoint won't complain about it being missing
# FIRVr_str <- "library(FIRVr, lib.loc = 'N:/Offdata/RM/_R code repository/RMPackages/_R4.0')"
# eval(parse(text=FIRVr_str))
# 
# options(digits = 2)
# options(DT.fillContainer = T)
# options(scipen = 999)
# 
# source(file.path("./functions/functions.R"))
# 

#set base directories
config_filename <-
  file.path(
    "\\\\markets/DATA/DATA/Offdata/FX/FX Dealers/_FX_jobs/FXScheduledJobs/tasks/BREreport",
    "config",
    "fx_swaps_config.csv"
  )

# set conventions --------------------------------------------------

static_fields <- c("MATURITY", "SECURITY_CURRENCY_PAIR", "VOL_MATURITY")

# bbg connection ---------------------------------------------------


bbg_result <- tryCatch({
    Rblpapi::blpConnect()
    bdp("IBM US Equity", "NAME")
  },
  error = function(e) {
    return(data.frame())
  }
)

if(nrow(bbg_result) == 0){
  
  fx_dt <- data.frame()
  fx_swap_returns <- data.frame()
  
} else{
  
  # set config -------------------------------------------------------
  
  config <- readr::read_csv(file = config_filename, show_col_types = FALSE) %>% 
    dplyr::distinct(.keep_all = T) %>% 
    tidyr::drop_na()
  
  currency_pairs <- config$currency %>% unique()
  unique_fields <- config$field %>% unique()
  
  tickers_maturity <-
    config %>% 
    dplyr::filter(type %in% c("forwards")) %>% 
    dplyr::pull(ticker) %>% 
    unique()
  
  # get data ----------------------------------------------------------------
  # pull static data for all securities 
  
  all_frames <- list()
  
  for (fields in unique_fields){
    
    sec_data <- config %>%
      dplyr::filter(field == fields)
    
    all_frames[[fields]] <-
      bloomberg_query(sec_data$ticker,
                      c(fields),
                      Sys.Date(),
                      Sys.Date())
  }
  
  maturity <-
    bloomberg_query_static(securities = tickers_maturity,
                           fields = static_fields,
                           tidy_data = F) %>% 
    dplyr::rename("currency" = "SECURITY_CURRENCY_PAIR", 
                  "maturity" = "MATURITY", 
                  "term" = "VOL_MATURITY") %>% 
    dplyr::mutate(currency = stringr::str_replace(string = currency, replacement = "", pattern = "/"), 
                  maturity = as.Date(maturity) - 1) %>% 
    dplyr::select(-Security)  
  
  
  df <- all_frames %>%
    data.table::rbindlist() %>%
    dplyr::group_by(Security, Date, Field) %>%
    dplyr::slice(1) %>% 
    dplyr::ungroup() %>% 
    dplyr::left_join(config, by = c("Security" = "ticker", "Field" = "field")) %>%
    dplyr::left_join(maturity, by = c("currency", "term")) %>% 
    dplyr::mutate(
      day_convention = dplyr::if_else(grepl(
        pattern = c("CAD"),
        x = currency,
        ignore.case = T
      ), 360, 365),
      fwd_convention = dplyr::if_else(grepl(
        pattern = c("JPY"),
        x = currency,
        ignore.case = T
      ), 100, 10000), 
      fc_notional = dplyr::if_else(grepl(
        pattern = c("JPY"),
        x = currency,
        ignore.case = T
      ), 10e9, 100e6),
      day_count = dplyr::if_else(grepl(
        pattern = c("CAD"),
        x = currency,
        ignore.case = T
      ), as.numeric(difftime(maturity,  as.Date(t_plus_date(1)))), 
         as.numeric(difftime(maturity,  as.Date(t_plus_date(2))))
      )
      # fx_base_convention = dplyr::if_else(
      #   stringr::str_starts(string = currency, pattern = "USD"), 1, -1)
    )  %>%
    dplyr::distinct(.keep_all = T) %>% 
    dplyr::select(type,
                  base, 
                  term, 
                  maturity, 
                  day_count, 
                  Value,
                  currency,
                  day_convention,
                  fwd_convention, 
                  fc_notional, 
                  fx_base_convention) %>%
    tidyr::spread(key = type, value = Value) %>% 
    dplyr::group_by(currency) %>% 
    dplyr::mutate(spot = imputeTS::na_locf(spot) #,day_count = as.numeric(difftime(maturity, settle_date)) 
                  ) %>% 
    tidyr::drop_na(forwards, funding, investment, spot) 
  
  
  fx_swap_returns <- df %>%
    dplyr::group_by(currency) %>% 
    dplyr::mutate(
      forward_rate = dplyr::if_else(
        stringr::str_starts(string = currency, pattern = "USD"),
        spot + (forwards / fwd_convention),
        1 / (spot + (forwards / fwd_convention))
      ),
      #forward_rate = spot + (forwards / fwd_convention),
      proceeds = fc_notional / spot,
      forward_amt = fc_notional / forward_rate,
      fx_swap_rate = fx_base_convention * 100 * ((( forward_amt - proceeds) / proceeds) / (day_count)) * day_convention,
      total_return = (fx_swap_rate - funding + investment) * (365 / day_convention) * 100,
      return_usd = (proceeds * total_return / fwd_convention *
        (day_count / day_convention)), 
      return_usd = dplyr::if_else(grepl(
        pattern = c("JPY"),
        x = currency,
        ignore.case = T
      ), return_usd/(spot*10), return_usd/spot)
    )
  
  fx_dt <- fx_swap_returns %>% 
    dplyr::arrange(currency, maturity) %>%
    dplyr::select(currency, 
                  term,
                  base, 
                  funding,
                  investment,
                  fx_swap_rate,
                  total_return,
                  return_usd)
  
  
  
  # time series -------------------------------------------------------------
  
  
  # all_frames <- list()
  # 
  # for (fields in unique_fields){
  #   
  #   sec_data <- config %>%
  #     dplyr::filter(field == fields)
  #   
  #   all_frames[[fields]] <-
  #     bloomberg_query(sec_data$ticker,
  #                     c(fields),
  #                     Sys.Date() - 365*5,
  #                     Sys.Date())
  # }
  # 
  # df <- all_frames %>%
  #   data.table::rbindlist() %>%
  #   unique() %>%
  #   dplyr::left_join(config, by = c("Security" = "ticker", "Field" = "field")) %>%
  #   dplyr::mutate(
  #     day_convention = dplyr::if_else(grepl(
  #       pattern = c("CAD"),
  #       x = currency,
  #       ignore.case = T
  #     ), 360, 365),
  #     fwd_convention = dplyr::if_else(grepl(
  #       pattern = c("JPY"),
  #       x = currency,
  #       ignore.case = T
  #     ), 100, 10000), 
  #     fc_notional = dplyr::if_else(grepl(
  #       pattern = c("JPY"),
  #       x = currency,
  #       ignore.case = T
  #     ), 100e9, 100e6), 
  #     fx_base_convention = dplyr::if_else(
  #       stringr::str_starts(string = currency, pattern = "USD"), 1, -1)
  #   )  %>%
  #   dplyr::distinct(.keep_all = T) %>% 
  #   dplyr::select(Date, 
  #                 type,
  #                 term, 
  #                 mat_days, 
  #                 Value,
  #                 currency,
  #                 day_convention,
  #                 fwd_convention, 
  #                 fc_notional, 
  #                 fx_base_convention) %>%
  #   tidyr::spread(key = type, value = Value) %>% 
  #   dplyr::group_by(currency, Date) %>% 
  #   dplyr::mutate(spot = imputeTS::na_locf(spot), 
  #                 day_count = as.numeric(mat_days)
  #   ) %>% 
  #   tidyr::drop_na(forwards, funding, investment, spot) 
  # 
  # 
  # fx_swap_returns <- df %>%
  #   dplyr::group_by(currency) %>% 
  #   dplyr::mutate(
  #     forward_rate = dplyr::if_else(
  #       stringr::str_starts(string = currency, pattern = "USD"),
  #       spot + (forwards / fwd_convention),
  #       1 / (spot + (forwards / fwd_convention))
  #     ),
  #     forward_rate = spot + (forwards / fwd_convention),
  #     proceeds = fc_notional / spot,
  #     forward_amt = fc_notional / forward_rate,
  #     fx_swap_rate = fx_base_convention * 100 * ((( forward_amt - proceeds) / proceeds) / (day_count)) * day_convention,
  #     total_return = (fx_swap_rate - funding + investment) * (365 / day_convention) * 100,
  #     return_usd = proceeds * total_return / fwd_convention *
  #       (day_count / day_convention)
  #   )
  # 
  # 
  # plot_data <- fx_swap_returns %>% dplyr::mutate(currency = paste0(currency, " - ", term)) %>% dplyr::filter(term %in% c("1W", "1M", "3M", "1Y"))
  # make_line_plot(plot_data, xlab = "Date", ylab = "total_return", fill = "currency", name = "currency")
  # 
  # 
  
  
}

