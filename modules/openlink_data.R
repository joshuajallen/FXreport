

# Use this method to avoid drive mappings in dynamic loading of content
RM_PATH <- function(subpath = "") {
  file.path("\\\\markets-nwsrv/DATA/Offdata/RM/_Dashboard", subpath)
}

# configuration
DASHBOARD_ROOT <- RM_PATH() 
home_dir <- file.path(DASHBOARD_ROOT, "P&L Config")
configuration_path <-file.path(home_dir, "config")
openlink_data_path <- file.path("G:", "DB_Data", "OLFLIVE", "outdir", "Daily_Pnl_Reports")
openlink_path <- file.path(home_dir, "openlink_data")

strategy_table_path <- file.path(configuration_path, "strategy.csv")
strategy_trade_table_path <-file.path(configuration_path, "strategy_trade_mapping.csv")

openlink_files <-
  file.path(list.files(openlink_path,
                       full.names = TRUE,
                       pattern = "*.csv"))

basename <-
  list.files(openlink_path,
             full.names = FALSE,
             pattern = "*.csv")

# Load each file with the correct types
is_Deal <- map_lgl(strsplit(basename, "_"), ~ .[2] == "Deal")
is_Agg <- map_lgl(strsplit(basename, "_"), ~ .[2] == "Agg")
is_Recon <- map_lgl(strsplit(basename, "_"), ~ .[2] == "Recon")
is_FX <- map_lgl(strsplit(basename, "_"), ~ .[1] == "FX")

openlink_data <-
  structure(rep(list(tibble()), length(openlink_files)), names = basename)

for (i in 1:length(which(is_Deal))) {
  openlink_data[[which(is_Deal)[i]]] <- readr::read_csv(
    file = file.path(openlink_path, basename[[which(is_Deal)[i]]]),
    col_types = cols(
      `Instrument Type` = "c",
      Currency = "c",
      `Deal No` = "d",
      `Ticker / Reference` = "c",
      `Trade Date` = "D",
      `Settle Date` = "D",
      `Maturity Date` = "D",
      Position = "d",
      `Trade or P.EOM` = "d",
      `Proceeds or P.EOM MTM` = "d",
      `Today Price` = "d",
      `Today MTM` = "d",
      `Total P&L` = "d",
      `o/w MTM` = "d",
      `o/w Funding` = "d",
      PV01 = "d",
      IE01 = "d",
      `PV01 Error` = "c"
    )
  )
}

for (i in 1:length(which(is_Agg))) {
  openlink_data[[which(is_Agg)[i]]] <- readr::read_csv(
    file = file.path(openlink_path, basename[[which(is_Agg)[i]]]),
    col_types = cols(
      `Instrument Type` = col_character(),
      Currency = col_character(),
      `Ins No` = col_integer(),
      Ticker = col_character(),
      `Maturity Date` = col_date(format = ""),
      Position = col_double(),
      Proceeds = col_double(),
      `P.EOM Price` = col_double(),
      `P.EOM MTM` = col_number(),
      `Today Price` = col_double(),
      `Today MTM` = col_number(),
      `Total P&L` = col_double(),
      `o/w MTM` = col_double(),
      `o/w Funding` = col_double()
    )
  )
}

openlink_data[[which(is_FX)]] <-
  readr::read_csv(
    file = file.path(openlink_path, basename[[which(is_FX)]]),
    col_types = cols(
      Date = "D",
      EUR = "d",
      GBP = "d",
      JPY = "d",
      CAD = "d"
    )
  )


for (i in 1:length(which(is_Recon))) {
  openlink_data[[which(is_Recon)[i]]] <-
    readr::read_csv(
      file = file.path(openlink_path, basename[[which(is_Recon)[i]]]),
      col_types = cols(
        Currency = col_character(),
        `Instrument Type` = col_character(),
        `MTD P&L` = col_double()
      )
    )
  
}

openlink_deal <- get_openlink_deal(openlink_data)


# get strategy tables  ----------------------------------------------------


strategy_table <- readr::read_csv(file = strategy_table_path,  col_types = "icccccclccccD")
attr(strategy_table, "spec") <- NULL

# reactive allocation configuration table
strategy_trade_mapping_tbl <-  readr::read_csv(file = strategy_trade_table_path,  col_types = "cciccd") 
attr(strategy_trade_mapping_tbl, "spec") <- NULL

# table of strategy id and deal numbers
strategy_trade_tbl <- strategy_table %>%
  dplyr::select(StrategyID, Strategy) %>%
  dplyr::left_join(strategy_trade_mapping_tbl %>% select(StrategyID, `Deal No`),
                   by = "StrategyID") %>% 
  dplyr::select(-StrategyID) %>% 
  dplyr::rename("DEAL_TRACKING_NUM" = "Deal No") %>% 
  dplyr::mutate(DEAL_TRACKING_NUM = as.numeric(DEAL_TRACKING_NUM) - 1, 
                DEAL_TRACKING_NUM = as.character(DEAL_TRACKING_NUM))


# join trades to strategies
openlink_deal <- openlink_deal %>%
  dplyr::mutate(`Deal No` = `Deal No` - 1) %>% 
  dplyr::select(
    Portfolio,
    `Instrument Type`,
    `Deal No`,
    `Ticker / Reference`,
    `Trade Date`,
    Position,
    `PV01 ($s)`,
    `Total P&L ($s)`
  ) %>%
  dplyr::filter(`Trade Date` >= Sys.Date() - 270, `Instrument Type` %in% c("Repo", "RevRepo"))

