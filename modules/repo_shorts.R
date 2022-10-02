# get data  ---------------------------------------------------------------


OL_trade_data <- openlink_repo_query() 
delta_dt <- delta_date_tbl(trade_data = OL_trade_data)

date <- t_plus_date(10)

#get net positions on tplus, want only short ones  
short_net <- delta_dt %>%
  dplyr::filter(DATE <= date) %>% 
  dplyr::group_by(TICKER, INS_NUM, ISIN, CURRENCY) %>%
  dplyr::summarise(POSITION = sum(`Change on day`), .groups = "keep") %>%
  dplyr::ungroup() %>%
  dplyr::filter(POSITION < -0.00001)

#find bonds with outflows on day, and join table of short positions to get shorts that need covering  
shorts <- delta_dt %>%
  dplyr::filter(DATE <= date, `Change on day` < 0, DATE >= Sys.Date()) %>% 
  dplyr::inner_join(short_net, by = c("TICKER", "INS_NUM", "ISIN", "CURRENCY")) %>% 
  dplyr::arrange(DATE) %>% 
  dplyr::mutate(T_PLUS = paste0("T + ", purrr::map2_dbl(Sys.Date(), DATE, date_diff_excluding_wekeends)), 
                DATE = format(DATE, "%d-%b-%y")
                ) %>% 
  dplyr::select(DATE, T_PLUS, everything())



