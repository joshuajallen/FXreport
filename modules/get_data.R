nostro_report <- file.path("G:/DB_Data/OLFLIVE/outdir/reports", folder_date, "RTPE_Cash_By_Nostro_Export.csv")
nostro <- readxl::read_xlsx(nostro_report)

nostro <- nostro %>%
  tail(-1) %>%
  dplyr::mutate(
    account = ifelse(is.na(account), "Total", account),
    Currency = na.locf(Currency),
    fromLast = TRUE
  ) %>%
  dplyr::filter(Currency %in% currencies) %>%
  dplyr::select(-account_name,-account_id,-ordr) %>%
  dplyr::mutate(
    Minimum_Balance = dplyr::if_else(is.na(Minimum_Balance), 0, Minimum_Balance),
    WithinLimit = ifelse(
      Current_Balance <= Maximum_Balance &
        Current_Balance >= Minimum_Balance,
      "Yes",
      "No"
    )
  )


df <- nostro %>%
  dplyr::filter(account == "Total") %>% 
  dplyr::select(-WithinLimit, -fromLast) %>%
  tidyr::gather(
    key = date,
    value = position,
    -account,
    -Currency,
    -Minimum_Balance,
    -Maximum_Balance,
    -Opening_Balance
  ) %>% 
  tidyr::drop_na(Maximum_Balance, Minimum_Balance) %>% 
  dplyr::group_by(account, Currency) %>% 
  dplyr::mutate(
    date = ifelse(date == "Current_Balance", format(Sys.Date(), "%d-%b-%y"), date), 
    date = stringr::str_extract(pattern = "[[:digit:]]{1,2}-[[:alpha:]]{1,3}-[[:digit:]]{1,2}", string = date), 
    warning_flag = ifelse((position < 0.1*Minimum_Balance | position > 0.9*Maximum_Balance), TRUE, FALSE), 
    error_flag = ifelse((position < Minimum_Balance | position > Maximum_Balance), TRUE, FALSE), 
    warning_flag = ifelse(error_flag == TRUE, FALSE, warning_flag), 
    date_formatted = as.Date(date, format = "%d-%b-%y")) %>% 
  dplyr::filter(date_formatted < Sys.Date() + 12) %>% 
  tidyr::drop_na(date)


warnings <- df %>%
  dplyr::group_by(Currency) %>% 
  dplyr::filter(warning_flag == TRUE)

if(nrow(warnings) < 1){
  
  nostro_warnings <- c()
  
} else{
  
  nostro_warnings <- paste0(
    "There are nostro WARNINGS (within 10% of limit) on the following account(s): ",
    paste0(warnings$Currency, " - ", warnings$account),
    " for the date: ",
    paste0(warnings$date),
    ". The balance is: ",
    paste0(comma(warnings$position)), 
    " . The min/max balances are: ", 
    paste0(comma(warnings$Minimum_Balance), " - ", comma(warnings$Maximum_Balance))
  )
  
}

errors <-  df %>% 
  dplyr::group_by(Currency) %>% 
  dplyr::filter(error_flag == TRUE)

if(nrow(errors) < 1){
  
  nostro_errors <- c()
  
} else{
  
  nostro_errors <- paste0(
    "There are nostro BREAKS on the following account(s): ",
    paste0(errors$Currency, " - ", errors$account),
    " for the date: ",
    paste0(errors$date),
    ". The balance is: ",
    paste0(comma(errors$position)), 
    " . The min/max balances are: ", 
    paste0(comma(errors$Minimum_Balance), " - ", comma(errors$Maximum_Balance))
  )

}



# nostro DT  --------------------------------------------------------------


nostro_dt <- nostro %>% 
  dplyr::mutate(
    Minimum_Balance = scales::comma(Minimum_Balance),
    Maximum_Balance = scales::comma(Maximum_Balance),
    Opening_Balance = scales::comma(Opening_Balance),
  ) %>%
  dplyr::select(Currency,
                account,
                contains("balance"),
                WithinLimit,
                Current_Balance,
                everything()) %>% 
  tidyr::drop_na()

