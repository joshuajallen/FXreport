# get data  ---------------------------------------------------------------

source("../modules/openlink_data.R")
OL_trade_data <- openlink_repo_query() 

tplus_repo <- 0

#table breaking out the holdings for each portfolio
allhold <-
  position_tplus(OL_trade_data, t_plus_day = t_plus_date(as.numeric(tplus_repo))) %>%
  dplyr::filter(abs(Position) >  0.00001) %>%
  dplyr::mutate(
    PORTFOLIO_TYPE = case_when(
      INTERNAL_PORTFOLIO %in% c(
        "ABM - AUD$ Bond Mgt",
        "CBM - CAN$ Bond Mgt",
        "UBM - US$ Bond Mgt",
        "EBM - European Bond Mgt",
        "YBM - YEN Bond Mgt"
      ) ~ "Management",
      INTERNAL_PORTFOLIO %in% c(
        "ABA - AUD$ Bond Trading",
        "CBA - CAN$ Bond Trading",
        "UBA - US$ Bond Trading",
        "EBA - European Bond Trading",
        "YBA - Yen Bond Trading"
      ) ~ "Active",
      INTERNAL_PORTFOLIO %in% c(
        "ANRL",
        "ENRL - \u20AC Net Reserves Liability",
        "UNRL",
        "CNRL",
        "YNRL"
      ) ~ "Unhedged",
      TRUE ~ "Hedged"
    )
  ) %>%
  dplyr::select(-c(INTERNAL_PORTFOLIO)) %>%
  tidyr::spread(PORTFOLIO_TYPE, Position) 

#Putting zeros in for NAs in portfolios with no holdings and add empty col if no holdings in that portfolio 
if(!("Unhedged" %in% colnames(allhold))){allhold <- allhold %>% dplyr::mutate(Unhedged = 0)}
if(!("Hedged" %in% colnames(allhold))){allhold <- allhold %>% dplyr::mutate(Hedged = 0)}
if(!("Management" %in% colnames(allhold))){allhold <- allhold %>% dplyr::mutate(Management = 0)}
if(!("Active" %in% colnames(allhold))){allhold <- allhold %>% dplyr::mutate(Active = 0)}
allhold <- tidyr::replace_na(allhold, replace = list(Active = 0, Management = 0, Hedged = 0, Unhedged = 0))

#create a table of maturities by ISIN (could be turned into a function)
instr_mat_table <- OL_trade_data %>%
  dplyr::filter(TRAN_TYPE == "Trading") %>%
  dplyr::select(c(ISIN, MATURITY_DATE)) %>%
  unique() %>%
  na.omit() %>%
  dplyr::mutate(MATURITY_DATE = as.Date(MATURITY_DATE))

# filter to get a table of repo collateral only 
dfr_repo <- allhold %>%
  dplyr::filter(TRAN_TYPE == "Repo Coll") %>%
  dplyr::mutate(Repo = Active + Management + Hedged + Unhedged) %>%
  dplyr::select(c(TICKER, INS_NUM, ISIN, Repo))

#filter table for holdings then join repo collateral table and format 
dfr_both <- allhold %>%
  dplyr::filter(TRAN_TYPE == "Trading") %>%
  dplyr::mutate(Done = Active + Management + Hedged + Unhedged) %>%
  dplyr::full_join(dfr_repo, by = c("TICKER", "INS_NUM", "ISIN")) %>%
  tidyr::replace_na(replace = list(
    Active = 0,
    Management = 0,
    Hedged = 0,
    Unhedged = 0,
    Repo = 0,
    Done = 0
  )) %>%
  dplyr::mutate(Total = Done + Repo) %>%
  dplyr::select(-c(TRAN_TYPE, CURRENCY)) %>%
  dplyr::mutate_if(is.numeric, round, digits = 3) %>%
  dplyr::arrange(TICKER) %>%
  dplyr::left_join(instr_mat_table, by = "ISIN")  %>%  # add in maturity dates for instruments
  dplyr::select(
    c(
      TICKER,
      INS_NUM,
      MATURITY_DATE,
      ISIN,
      Active,
      Management,
      Unhedged,
      Hedged,
      Done,
      Repo,
      Total
    )
  ) %>%
  dplyr::mutate(MATURITY_DATE = dplyr::if_else(
    is.na(MATURITY_DATE),
    str_extract(TICKER, "\\d{6}$") %>% as.Date("%d%m%y"),
    MATURITY_DATE
  )) %>%
  dplyr::filter(Repo < 0)

# calculate change table -----------------------------------------------------

delta_table <-
  delta_date_tbl(trade_data = OL_trade_data) %>% 
  dplyr::filter(DATE > Sys.Date() - 2, 
                DATE < Sys.Date() + 14, 
                INS_NUM %in% unique(dfr_both$INS_NUM), 
                abs(`Change on day`) > 0) %>% 
  dplyr::arrange(DATE) %>% 
  dplyr::mutate(INS_NUM = as.character(INS_NUM)) %>% 
  dplyr::select(TICKER, INS_NUM, CURRENCY, DATE, `Change on day`)

unique_specials <- strategy_trade_tbl %>%
  dplyr::filter(grepl(
    pattern = paste0(c("special", "repo", "RSGC"), collapse = "|"),
    ignore.case = T,
    x = Strategy
  )) %>%
  tidyr::drop_na() %>%
  dplyr::pull(DEAL_TRACKING_NUM) 

OL_specials <- openlink_deal_info(OL_deal_number = unique_specials) %>%
  dplyr::mutate(INS_NUM = as.character(INS_NUM), DEAL_TRACKING_NUM = as.character(DEAL_TRACKING_NUM)) %>% 
  dplyr::filter(MATURITY_DATE > Sys.Date() - 2) %>%  #MATURITY_DATE < Sys.Date() + 20) %>% #BUY_SELL == "Sell"
  dplyr::select(
    DEAL_TRACKING_NUM,
    INS_NUM, 
    INS_TYPE,
    BUY_SELL,
    EXTERNAL_BUNIT,
    TRADE_DATE,
    MATURITY_DATE,
    POSITION,
    REFERENCE, 
    INTERNAL_CONTACT
  ) %>% 
  dplyr::arrange(MATURITY_DATE) %>%
  dplyr::distinct() %>% 
  dplyr::right_join(delta_table, by = c("INS_NUM"))  %>% 
  dplyr::left_join(strategy_trade_tbl, by = c("DEAL_TRACKING_NUM")) 

specials <- OL_specials %>% 
  dplyr::group_by(CURRENCY, TICKER, Strategy, EXTERNAL_BUNIT, DEAL_TRACKING_NUM, INTERNAL_CONTACT, BUY_SELL, MATURITY_DATE, POSITION, DATE, `Change on day`) %>% 
  dplyr::summarise(POSITION = sum(POSITION, na.rm = T), .groups = "keep") %>% 
  dplyr::distinct() %>% 
  tidyr::drop_na(DEAL_TRACKING_NUM) %>% 
  dplyr::group_by(CURRENCY, TICKER, Strategy, EXTERNAL_BUNIT, DEAL_TRACKING_NUM, INTERNAL_CONTACT, BUY_SELL, MATURITY_DATE, POSITION, DATE) %>% 
  dplyr::summarise(`Change on day` = sum(`Change on day`, na.rm = T), .groups = "keep") %>% 
  dplyr::arrange(CURRENCY, DATE) %>% 
  dplyr::filter(MATURITY_DATE < Sys.Date() + 10, `Change on day` > 0)

