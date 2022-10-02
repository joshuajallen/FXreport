
# Older versions of this file can be found at the RMDashboard repo
# This includes legacy (pre-API) functions  
# https://almplatform/tfs/BankCollection/_git/FEDReserveManagers?path=%2FRMDashboard%2Ffunctions%2Fopenlink_query.R&version=GBmaster



#
# Convert vectors into the format expected by the API
# "'a', 'b', 'c'"
#

openlink_query_vector_to_api_string <- function(v) {
  paste(shQuote(v, type = "sh"), collapse=",")
}


#
# Wrapper for querying the RM view over the Openlink API
#

openlink_query_api <- function(instrument_types = NULL,
                               portfolios = NULL,
                               tran_statuses = NULL,
                               deal_number_min = NULL,
                               deal_number_max = NULL,
                               trade_date_min = NULL,
                               trade_date_max = NULL) {
  
  #
  # Add optional parameters to the query if relevant
  #
  
  query <- list()
  
  if ( ! is.null(instrument_types) ) {
    query <- append(
      query, 
      list(instrument_types = openlink_query_vector_to_api_string(instrument_types))
    )
  }
  
  if ( ! is.null(portfolios) ) {
    query <- append(
      query, 
      list(portfolios = openlink_query_vector_to_api_string(portfolios))
    )
  }
  
  if ( ! is.null(tran_statuses) ) {
    query <- append(
      query, 
      list(tran_statuses = openlink_query_vector_to_api_string(tran_statuses))
    )
  }
  
  if ( ! is.null(deal_number_min) ) {
    query <- append(
      query, 
      list(deal_number_min = deal_number_min)
    )
  }
  
  if ( ! is.null(deal_number_max) ) {
    query <- append(
      query, 
      list(deal_number_max = deal_number_max)
    )
  }
  
  if ( ! is.null(trade_date_min) ) {
    query <- append(
      query, 
      list(trade_date_min = trade_date_min)
    )
  }
  
  if ( ! is.null(trade_date_max) ) {
    query <- append(
      query, 
      list(trade_date_max = trade_date_max)
    )
  }
  
  #
  # Setup the query
  #
  
  url <- "https://olf-plw-api01/RmView"
  
  auth = authenticate(user = ":", password = "", type = "gssnegotiate")
  
  column_types <- cols(
    .default = col_character(),
    DEAL_TRACKING_NUM	= col_double(),
    TRAN_NUM	= col_double(),
    INS_NUM	= col_double(),
    POSITION = col_double(),
    COUNTER_POSITION = col_double(),
    PRICE = col_double(),
    PROCEEDS = col_double(),
    YIELD	= col_double(),
    FUTURES_NOTNL = col_double(),
    BROKER_ID	= col_double(),
    TRADE_DATE = col_date(),
    SETTLE_DATE = col_date(),
    MATURITY_DATE = col_date(),
    START_DATE = col_date()
  )
  
  
  result <- tryCatch({
    
    response <- POST(url,
                     config = auth,
                     use_proxy(""),
                     encode = "json",
                     body = query)
    
    response_str <- httr::content(response, type = "text/plain", encoding = "UTF-8")
    
    
    #
    # Convert the response to a data frame
    #
    
    read_csv(response_str, na = "<NULL>", col_types = column_types)
    
  },
  error = function (e) {
    flog.error("Failed to get data from Openlink API: %s", e)
    tibble()
  })
  
  result  
  
}



#
# Open and return a connection to the Oracle database server.
# 



openlink_rm_bond_holdings <- function(
  bond_isins = "",
  portfolios = c(
    "ABA - AUD$ Bond Trading",
    "ABM - AUD$ Bond Mgt",
    "CBA - CAN$ Bond Trading",
    "CBM - CAN$ Bond Mgt",
    "UBM - US$ Bond Mgt",
    "UBA - US$ Bond Trading",
    "EBA - European Bond Trading",
    "EBM - European Bond Mgt",
    "YBA - Yen Bond Trading",
    "YBM - YEN Bond Mgt",
    "ANRL",
    "CNRL",
    "ENRL - \u20AC Net Reserves Liability",
    "UNRL",
    "YNRL",
    "ASL - \u00A3/AUD$ Swapping Liability",
    "CSL - \u00A3/CAN$ Swapping Liability",
    "ESL - \u00A3/\u20AC Swapping Liability",
    "USL - \u00A3/$ Swapping Liability",
    "YSL - \u00A3/Yen Swapping Liability"
  )
) {
  
  result <- openlink_query_api(portfolios = portfolios,
                               instrument_types = "GBND",
                               tran_statuses = "Validated") %>%
    select(
      DEAL_TRACKING_NUM,
      INS_NUM,
      INTERNAL_PORTFOLIO,
      REFERENCE,
      ISIN,
      PRICE,
      TRADE_DATE,
      SETTLE_DATE,
      MATURITY_DATE,
      CURRENCY,
      POSITION
    ) %>% 
    arrange(INS_NUM)
  
  if ( length(bond_isins) == 1 & bond_isins[1] == "" ) {
    return(result) 
  } else {
    return(result %>% filter(ISIN %in% bond_isins))
  }
  
}




#
# Recover a list of RM trades. Note that position size in OpenLink
# is in millions for bonds. Adjust the output so that all positions
# are in millions (saves on division operations when visualising)
#


openlink_rm_trades <- function(from_date = "2008-01-01",
                               to_date = Sys.Date(),
                               portfolios = c(
                                 "ABA - AUD$ Bond Trading", 
                                 "ABM - AUD$ Bond Mgt",
                                 "CBA - CAN$ Bond Trading",
                                 "CBM - CAN$ Bond Mgt",
                                 "UBM - US$ Bond Mgt",
                                 "UBA - US$ Bond Trading",
                                 "EBA - European Bond Trading",
                                 "EBM - European Bond Mgt",
                                 "YBA - Yen Bond Trading",
                                 "YBM - YEN Bond Mgt"
                               ),
                               ins_types = c(
                                 "GBND",      # Bonds
                                 "IBOND",     # Inf-bonds
                                 "MM-G-Bill", # Bills
                                 "IRS",       # Swaps
                                 "DFUT",      # IR Futures
                                 "BONDFUT"    # Bond Futures
                               ) ){
  
  openlink_query_api(portfolios = portfolios,
                     instrument_types = ins_types,
                     trade_date_min = from_date,
                     trade_date_max = to_date) %>%
    select(
      DEAL_TRACKING_NUM,
      INS_NUM,
      INS_TYPE,
      INTERNAL_PORTFOLIO,
      REFERENCE,
      ISIN,
      PRICE,
      TRADE_DATE,
      SETTLE_DATE,
      MATURITY_DATE,
      CURRENCY,
      POSITION,
      INTERNAL_CONTACT,
      EXTERNAL_LENTITY,
      FUTURES_NOTNL,
      TICKER,
      TRAN_TYPE
    ) %>% 
    mutate(POSITION = ifelse(INS_TYPE == "IRS", POSITION/1e6, POSITION)) %>%
    # Already have a ticker field in the trading volumes module (for BBG query)
    rename(INS_REFERENCE = TICKER)
  
}



#' Pulls all the data for any input instrument number from OL database
#' Should only be used if there is a database connection in place.
#' 
#' 
#' @param OL_tran_number The OL transaction number   
#'  
#' @return data frame with transaction data 
#'
#' @examples
#' openlink_deal_info(4842021)
#' 
#' 
#' Required packages:
#' dplyr, dbplyr, (OL connection function) 
#'  
#' 

openlink_deal_info <- function(OL_deal_number) {
  
  url <- "https://olf-plw-api01/SQL"
  
  auth = httr::authenticate(user = ":", password = "", type = "gssnegotiate")
  
  column_types <- cols(
    .default = col_character(),
    DEAL_TRACKING_NUM	= col_double(),
    TRAN_NUM	= col_double(),
    INS_NUM	= col_double(),
    POSITION = col_double(),
    COUNTER_POSITION = col_double(),
    PRICE = col_double(),
    PROCEEDS = col_double(),
    YIELD	= col_double(),
    FUTURES_NOTNL = col_double(),
    BROKER_ID	= col_double(),
    TRADE_DATE = col_date(format = "%d-%b-%y"),
    SETTLE_DATE = col_date(format = "%d-%b-%y"),
    MATURITY_DATE = col_date(format = "%d-%b-%y"),
    START_DATE = col_date(format = "%d-%b-%y")
  )
  
  deal_nums <- paste0("(", paste(OL_deal_number, collapse = ","), ")")
  
  query = paste(
    "SELECT * FROM OLF_MASTER.USER_DATA_MODEL_DEAL_VIEW WHERE DEAL_TRACKING_NUM IN",
    deal_nums
  )
  
  result <- tryCatch({
    
    response <- POST(url,
                     config = auth,
                     use_proxy(""),
                     encode = "raw",
                     body = query)
    
    response_str <- httr::content(response, type = "text/plain", encoding = "UTF-8")
    
    #
    # Convert the response to a data frame
    #
    
    read_csv(response_str, na = "<NULL>", col_types = column_types)
    
  },
  error = function (e) {
    # flog.error("Failed to get data from Openlink API: %s", e)
    tibble()
  })
  
}

#' Opens or refreshes the Openlink connection
#' Then returns a logical depending on whether it has failed or succeeded.
#' If succeeded also returns a connection object
#' 
#'  
#' @return dataframe of openlink trades  
#'
#' @examples openlink_repo_query()
#' 
#' 
#' Reuired packages:
#' dplyr, dbplyr,
#'  
#' 

openlink_repo_query <- function(
  portfolios = c(
    "ABA - AUD$ Bond Trading",
    "ABM - AUD$ Bond Mgt",
    "CBA - CAN$ Bond Trading",
    "CBM - CAN$ Bond Mgt",
    "UBM - US$ Bond Mgt",
    "UBA - US$ Bond Trading",
    "EBA - European Bond Trading",
    "EBM - European Bond Mgt",
    "YBA - Yen Bond Trading",
    "YBM - YEN Bond Mgt",
    "ANRL",
    "CNRL",
    "ENRL - \u20AC Net Reserves Liability",
    "UNRL",
    "YNRL",
    "ASL - \u00A3/AUD$ Swapping Liability",
    "CSL - \u00A3/CAN$ Swapping Liability",
    "ESL - \u00A3/\u20AC Swapping Liability",
    "USL - \u00A3/$ Swapping Liability",
    "YSL - \u00A3/Yen Swapping Liability"
  ),
  ins_types = c("GBND", "IBOND", "MM-G-Bill", "MM-ECP", "BONDFUT"),
  tran_statuses = "Validated") {
  
  openlink_query_api(portfolios = portfolios,
                     instrument_types = ins_types,
                     tran_statuses = tran_statuses) %>%
    select(
      DEAL_TRACKING_NUM,
      TRAN_NUM,
      INS_NUM,
      TRAN_STATUS,
      TRAN_TYPE,         
      ASSET_TYPE,
      TOOLSET,
      INS_TYPE,
      BASE_INS_TYPE,
      BUY_SELL,          
      INTERNAL_LENTITY,
      INTERNAL_BUNIT,
      INTERNAL_PORTFOLIO,
      EXTERNAL_LENTITY,
      EXTERNAL_BUNIT,
      EXTERNAL_PORTFOLIO,
      TRADE_DATE,
      SETTLE_DATE,
      MATURITY_DATE,
      CURRENCY,
      POSITION,
      PRICE,
      PROCEEDS,
      YIELD,
      REFERENCE,
      ISIN,
      INTERNAL_CONTACT,
      FUTURES_NOTNL,
      TICKER,
      SHORT
    ) %>%
    mutate(POSITION = ifelse(INS_TYPE == "IRS", POSITION/1e6, POSITION)) %>%
    arrange(INS_NUM)
  
}

