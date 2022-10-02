app_config <- new.env()

app_config$boe_palette <- rep(c(
  "#4a7e8f", # teal
  "#cf395c", # bright pink
  "#a9c9d3", # light blue
  "#b25395", # pink
  "#3b855f", # green
  "#2f4f5b", # very dark teal
  "#b65e19", # orange
  "#0f7cbf", # blue
  "#555555"  # dark grey
), 3)

make_fx_datatable <- function(data){
  
  if(nrow(data) == 0){
    
    return(datatable(
      data.frame("No bloomberg connection availavle"),
      options = list(pageLength = 20,
                     dom = 't'),
      rownames = FALSE, 
      colnames = ""
    ))
    
  } else{
  
  ism <- data %>% lapply(class) %>% unlist == "numeric"
  
  DT:: datatable(
    data, 
    extensions = c("RowGroup", "Buttons", "Scroller"),
    rownames = FALSE,
    filter = "top", 
    escape = FALSE,
    selection = "none",
    options = list(
      pageLength = 20,
      selection = "none",
      rowGroup = list(
        dataSrc = 0
      ),
      fnDrawCallback = htmlwidgets::JS(
        "function() { HTMLWidgets.staticRender(); }"
      )
    )
  ) %>%
    formatRound(ism, digits = 2) %>% 
    formatStyle(ism, color = styleInterval(0, c("red", "black"))) %>%
    formatStyle("funding", backgroundColor = lightred) %>%
    formatStyle("investment", backgroundColor = lightgreen) %>%
    formatStyle("fx_swap_rate", backgroundColor = styleInterval(0, c(lightred, lightgreen))) %>%
    formatStyle("total_return", backgroundColor = styleInterval(0, c(lightred, lightgreen))) %>%
    formatStyle("return_usd", backgroundColor = styleInterval(0, c(lightred, lightgreen))) %>%
    formatStyle("return_usd",
                fontWeight = "bold") %>%
    formatStyle("total_return",
                fontWeight = "bold")
  
  }
  
  
  
}



plot_term_structure <- function(data, xlab, ylab, fill, polynomial_order = 3){
  
  
  
  if (nrow(data) > 0 & all(c(paste0(xlab), paste0(ylab), paste0(fill)) %in% colnames(data))) {
    
    tickers <- unique(data[[fill]])
    
    p <- plotly::plot_ly(data = data)
    
    for ( i in seq_len(length(tickers)) ) {
      
      plot_data <- data %>%
        dplyr::filter(!!rlang::sym(fill) == tickers[i]) %>% 
        tidyr::drop_na(!!rlang::sym(xlab), !!rlang::sym(ylab))
      
      if(nrow(plot_data) >0){
        
        formula <- as.formula(paste0(ylab, " ~ ", "poly(", xlab, ", ", polynomial_order, ")"))
        fit <- lm(formula, data = plot_data)
        
        p <- p %>%
          add_trace(
            x = plot_data[[xlab]],
            y = plot_data[[ylab]],
            name = plot_data[[fill]][1],
            type = "scatter",
            mode = "markers",
            marker = list(
              color = app_config$boe_palette[i],
              type = 3
            ),
            hovertext =   paste(
              "TERM:",
              plot_data$term,
              "<br> FUNDING:",
              plot_data$funding,
              "<br> INVESTMENT:",
              plot_data$investment,
              "<br> FX swap return:",
              plot_data$fx_swap_rate,
              "<br> TOTAL:",
              plot_data$total_return
            )
          ) %>%
          add_trace(
            x = sort(plot_data[[xlab]]),
            y = fitted(fit)[order(plot_data[[xlab]])],
            name = paste0(plot_data[[fill]][1], " fitted"),
            type = "scatter",
            mode = "lines",
            line = list(color = app_config$boe_palette[i], type = 3, smoothing = 1.3, shape = "spline"),
            showlegend = T
          )
      }
      
      
    }
    
  } else {
    
    text <- paste("No rows in DF")
    p <- ggplot2::ggplot() +
      ggplot2::theme_bw() +
      ggplot2::theme(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank())
    
  }
  
  return(p)
  
}


make_line_plot <- function(df, xlab, ylab, fill, name, visible = NULL){
  
  
  p <- plotly::plot_ly(data = df)
  tickers <- df[[fill]] %>% unique()
  
  for ( i in seq_len(length(tickers)) ) {
    
    current <- df %>%
      dplyr::filter(!! rlang::sym(fill) == tickers[i])
    
    p <- p %>%
      add_trace(
        x = current[[xlab]],
        y = current[[ylab]],
        name = current[[name]][1],
        type = "scatter",
        mode = "lines",
        line = list(color = app_config$boe_palette[i]), 
        visible = visible
      )
  }
  
  return(p)
  
}


#' 
#' Function to find the date a number of weekdays from now
#'
#' packages required: lubridate
#'
#' @param t_plus_days number of weekdays to go from the reference date, integer
#' @param ref_day the date from which calculate the number of days
#'
#' @return the date t_plus_days weekdays from the reference date 
#'

t_plus_date <- function(t_plus_days = 1L, ref_day = Sys.Date()){
  
  if(t_plus_days < 0) flog.error("t_plus_days(): t_plus less than zero")
  if(t_plus_days != round(t_plus_days, digits = 0)) flog.error("t_plus_days(): t_plus is not an integer")
  if(!(is.Date(ref_day))) flog.error("t_plus_days(): ref_date is not a date")
  if(wday(ref_day) == 7 | wday(ref_day) == 1) flog.warn("t_plus_days(): ref_date is a weekend")
  
  t_plus_days_mod <- t_plus_days %% 5 
  add_days <- ((t_plus_days - t_plus_days_mod) / 5) * 7
  
  wday_t_plus <- wday(ref_day + t_plus_days_mod) %% 7
  if (wday_t_plus < t_plus_days_mod) add_days <- add_days + 2
  
  t_plus_date <- ref_day + add_days + t_plus_days_mod
  
  if(wday(t_plus_date) == 7 | wday(t_plus_date) == 1)  flog.error("t_plus_days(): output a weekend")
  
  return(t_plus_date)
}

date_diff_excluding_wekeends <- function(x, y) {
  if(is.na(x) || is.na(y)) return(NA)
  sum(!format(seq(x, y - 1, by = '1 day'), '%u') %in% 6:7)
}


#' Function to find the position by portfolio by bond ticker
#' This is only for Bonds, T bills, ECP  and futures  
#'
#' packages required: dplyr
#'
#' @param position_data full OL data frame of validated trades
#' @param t_plus_day the date at which to calculate the position
#'
#' @return portfolio holding positions in tidy table on the date specified by t_plus_day
#'


position_tplus <- function(position_data, t_plus_day = Sys.Date()){
  
  # Sort relevant trades
  posit_dfr <- position_data %>% 
    filter(INS_TYPE %in% c("GBND", "MM-G-Bill", "MM-ECP", "BONDFUT", "IBOND", "DFUT")) %>% #not including repo, cash depos or swaps
    filter(SETTLE_DATE <= t_plus_day, MATURITY_DATE > t_plus_day) %>%   # filter for unmatured instruments that have settled on day t_plus_day 
    select(TICKER, INS_NUM, ISIN, INTERNAL_PORTFOLIO, TRAN_TYPE, CURRENCY, POSITION) %>%      #select columns wanted from the 30
    group_by(TICKER, INS_NUM, ISIN, CURRENCY, INTERNAL_PORTFOLIO, TRAN_TYPE) %>%         
    summarise(Position = sum(POSITION), .groups = "keep") %>%              # sum across all positions to get net position by portfolio 
    ungroup()
  
  posit_dfr 
}

shorts_for_w_plus <- function(delta_dt, w_plus) {
  shorts_to_cover(delta_dt, tplus = t_plus_date(w_plus)) %>%
    format_shortcover_datatable()
}

#' Generates the flow by bond on every day
#' 
#'    This includes repo collateral, bond transaction and maturity flows for each bond, bill, ECP and bond future
#'    If there is no null values, only vlaues if a flow occurs, (completing with zeros creates a ~100mb dat frame given the time horizon)
#'    Starts from first trade in 1999 until the maturity of teh last cond owned 
#'
#' packages required: dplyr
#'
#' @param trade_data full OL data frame of validated trades
#'
#' @return portfolio holding positions in tidy table on the date specified by t_plus_day
#'


delta_date_tbl <- function(trade_data){
  
  #filter to get inflows by using settle date
  dfr_set <- trade_data %>%
    filter(INS_TYPE %in% c("GBND", "MM-G-Bill", "MM-ECP", "BONDFUT", "IBOND", "DFUT")) %>%
    select(TRAN_NUM, TICKER, INS_NUM, ISIN, CURRENCY, SETTLE_DATE, POSITION) %>%
    mutate(DELTA = POSITION, DATE = SETTLE_DATE) %>%
    select(-c(SETTLE_DATE, POSITION))
  
  #filter to get outflows by maturity date, then bind the inflows on to the bottom
  dfr_mat <- trade_data %>%
    filter(INS_TYPE %in% c("GBND", "MM-G-Bill", "MM-ECP", "BONDFUT", "IBOND", "DFUT")) %>%
    select(TRAN_NUM, TICKER, INS_NUM, ISIN, CURRENCY, MATURITY_DATE, POSITION) %>%
    mutate(DELTA = -POSITION, DATE = MATURITY_DATE) %>% 
    select(-c(MATURITY_DATE, POSITION)) %>%
    bind_rows(dfr_set) %>%    #adding inflows
    group_by(TICKER, INS_NUM, ISIN, CURRENCY, DATE) %>%
    summarise(DELTA = sum(DELTA), .groups = "keep") %>%    # condense multiple entries for same bond on smae day
    ungroup() %>%
    rename(`Change on day` = DELTA) %>%
    filter(DATE >= "2000-01-01")  #transactions pre-2000 offset exactly
  
  return(dfr_mat)
}


#' 
#' Function to find the short position in bonds on any specific day
#' 
#' packages required: dplyr
#' 
#' @param delta_table the table of flows generated by delta_date_tbl function 
#' @param tplus the date for which shorts may need to be covered, Date format
#' 
#' @return data frame of shorts to cover on tplus
#' 

shorts_to_cover <- function(delta_table, tplus){
  
  #get net positions on tplus, want only short ones  
  short_net <- delta_table %>%
    filter(DATE <= tplus) %>% 
    group_by(TICKER, INS_NUM, ISIN, CURRENCY) %>%
    summarise(POSITION = sum(`Change on day`), .groups = "keep") %>%
    ungroup() %>%
    filter(POSITION < -0.00001)
  
  #find bonds with outflows on day, and join table of short positions to get shorts that need covering  
  shorts <- delta_table %>%
    filter(DATE == tplus, `Change on day` < 0) %>% 
    inner_join(short_net, by = c("TICKER", "INS_NUM", "ISIN", "CURRENCY"))
  
  shorts
  
}

#' Format short cover datatables 
#'
#' @param dfr
#'
#' @return formatted datatable
#' 
format_shortcover_datatable <- function(dfr) {
  
  # add totals to first visible column
  x <- dfr
  
  # identify numeric rows for formatting
  final_col <- rep(FALSE, ncol(x))
  final_col[[length(final_col)]] <- TRUE
  
  datatable(
    x,
    rownames = FALSE,
    options = list(
      pageLength = nrow(x),
      paging = FALSE, 
      ordering = FALSE,
      lengthChange = FALSE,
      searching = FALSE,
      info = FALSE
    )
  ) %>% 
    formatStyle(final_col, color = styleInterval(0, c("red", "black"))) %>% 
    formatStyle(final_col, backgroundColor = "palegreen")
}

#' Format holdings datatable 
#'
#' @param dfr
#'
#' @return formatted datatable
#' 
format_holdings_datatable <- function(dfr) {
  
  # add totals to first visible column
  x <- dfr
  
  
  # identify cols for formatting
  isn <- unlist(sapply(x, is.numeric))
  totals <- colnames(x) == "Total"
  
  datatable(
    x,
    rownames = FALSE,
    options = list(
      pageLength = nrow(x),
      paging = FALSE,
      info = FALSE,
      search = list(regex = TRUE)
    )
  ) %>% 
    formatStyle(isn, color = styleInterval(0, c("red", "black"))) %>% 
    formatStyle(totals, fontWeight = "bold") 
}


#' Calculate PV01 at strategy level
#'
#' @param filtered_tbl calculated strategy trade table
#'
#' @return tibble
#' @examples
strategy_PV01 <- function(calculated_tbl) {
  calculated_tbl %>% 
    filter(`Instrument Type` %in% c("Bond", "IRS", "ILS", "ILB", "Xccy", "Bill", "ECP", "RateFuture", "BondFuture")) %>% 
    group_by(StrategyID, `Ticker / Reference`) %>%
    summarise(`PV01 ($s)` = sum(`PV01 ($s)`, na.rm = TRUE), .groups = "drop_last") %>%
    ungroup() %>%
    mutate(PV01_long = if_else(`PV01 ($s)` < 0, `PV01 ($s)`, 0, missing = 0),
           PV01_short = if_else(`PV01 ($s)` > 0, `PV01 ($s)`, 0, missing = 0)) %>%
    group_by(StrategyID) %>% 
    summarise(long_sum = sum(PV01_long),
              short_sum = sum(PV01_short),
              .groups = "drop_last") %>% 
    ungroup() %>% 
    mutate(`PV01 ($s)` = (abs(long_sum) + short_sum) / 2 ) %>% 
    select(StrategyID, `PV01 ($s)`)
}


#' Calculate IE01 at strategy level
#'
#' @param filtered_tbl calculated strategy trade table
#'
#' @return tibble
#' @examples
strategy_IE01 <- function(calculated_tbl) {
  calculated_tbl %>% 
    filter(`Instrument Type` %in% c("Bond", "IRS", "ILB", "ILS", "Xccy", "Bill", "ECP", "RateFuture", "BondFuture")) %>% 
    group_by(StrategyID, `Ticker / Reference`) %>%
    summarise(`IE01 ($s)` = sum(`IE01 ($s)`, na.rm = TRUE), .groups = "drop_last") %>%
    ungroup() %>%
    mutate(IE01_long = if_else(`IE01 ($s)` < 0, `IE01 ($s)`, 0, missing = 0),
           IE01_short = if_else(`IE01 ($s)` > 0, `IE01 ($s)`, 0, missing = 0)) %>%
    group_by(StrategyID) %>% 
    summarise(long_sum = sum(IE01_long),
              short_sum = sum(IE01_short),
              .groups = "drop_last") %>% 
    ungroup() %>% 
    mutate(`IE01 ($s)` = (abs(long_sum) + short_sum) / 2 ) %>% 
    select(StrategyID, `IE01 ($s)`)
}


#' Get internal trade table
#'
#' Load from disk with consistent structure. Uses an Excel date format for storage.
#' @param internal_trade_table_path path to table
#'
#' @return tibble
#' @examples 
#' ex <- new.env()
#' sys.source("modules/pl_management_module.R", envir = ex)
#' ex$openlink_data <- ex$get_openlink_csv(ex$openlink_path)
#' ex$get_internal_trade_table(ex$internal_trade_table_path, ex$openlink_data)
get_internal_trade_table <- function(internal_trade_table_path) {
  flog.debug("Loading internal trade table ...")
  dfr <- read_csv(internal_trade_table_path, col_types = cols(AdjustmentID = "i",
                                                              AdjustmentDate = col_date("%d/%m/%Y"), Portfolio = "c", FromStrategyID = "i",
                                                              ToStrategyID = "i", Currency = "c", AdjustmentAmount = "d", Comment = "c"))
  
  dfr
}


#' Get openlink fx tibble
#'
#' @param openlink_data list of openlink tibbles
#'
#' @return tibble
get_openlink_fx <- function(openlink_data) {
  
  num_fx <- which(sapply(strsplit(names(openlink_data), "_"), function(x) x[1] == "FX"))
  openlink_data[[num_fx]] %>% 
    dplyr::mutate(Date = as.Date(Date, format = "%d/%m/%Y")) %>% 
    dplyr::slice(n()) %>% 
    tidyr::gather(Currency, FX, -Date) %>% 
    dplyr::select(-Date) %>% 
    dplyr::bind_rows(tibble(Currency = "USD", FX = 1.0))
}

#' Apply internal trades
#'
#' Apply internal trades to tibble aggregated to Strategy.
#' @param strategy_tbl tibble aggregated to Strategy
#' @param internal_trade_tbl tibble of internal trades in USD
#'
#' @return tibble
apply_internal_trades <- function(strategy_tbl, internal_trade_usd_tbl) {
  
  # calculate PL outflows for each strategy
  from_strategy <- internal_trade_usd_tbl %>% 
    dplyr::select(FromStrategyID, `AdjustmentAmount ($s)`) %>% 
    dplyr::group_by(FromStrategyID) %>% 
    dplyr::summarise(`AdjustmentAmount ($s)` = sum(`AdjustmentAmount ($s)`, na.rm = TRUE), .groups = "drop_last") %>% 
    dplyr::ungroup()
  
  # calculate PL inflows for each strategy
  to_strategy <- internal_trade_usd_tbl %>% 
    dplyr::select(ToStrategyID, `AdjustmentAmount ($s)`) %>% 
    dplyr::group_by(ToStrategyID) %>% 
    dplyr::summarise(`AdjustmentAmount ($s)` = sum(`AdjustmentAmount ($s)`, na.rm = TRUE), .groups = "drop_last") %>% 
    dplyr::ungroup()
  
  # apply aggregated adjustments
  strategy_tbl %>% 
    dplyr::left_join(from_strategy, by = c("StrategyID" = "FromStrategyID")) %>% 
    dplyr::mutate(`P&L ($s)` = if_else(is.na(`AdjustmentAmount ($s)`), `P&L ($s)`, `P&L ($s)` - `AdjustmentAmount ($s)`)) %>% 
    dplyr::select(-`AdjustmentAmount ($s)`) %>% 
    dplyr::left_join(to_strategy, by = c("StrategyID" = "ToStrategyID")) %>% 
    dplyr::mutate(`P&L ($s)` = if_else(is.na(`AdjustmentAmount ($s)`), `P&L ($s)`, `P&L ($s)` + `AdjustmentAmount ($s)`)) %>%
    dplyr::select(-`AdjustmentAmount ($s)`)
  
}

#' Order data using another table
#'
#' Order data using the ordering in another table. Missing columns are ignored.
#' @param dfr tibble
#' @param ordered_table tibble 
#'
#' @return tibble
#' @examples
order_table <- function(dfr, ordered_table) {
  for (cn in names(ordered_table)) {
    if (cn %in% names(dfr)) dfr[[cn]] <- factor(dfr[[cn]], levels = unique(ordered_table[[cn]]))
  }
  dfr %>% arrange_if(names(.) %in% names(ordered_table))
}


# Get today's date
# can be used for running historical P&Ls in case of a break 
# Goes back to Friday so dashboard can open on the weekend
get_today <- function() {
  
  #date <- as.Date("2021-10-28")
  if(wday(Sys.Date()) == 7){date <- Sys.Date() - 1}else if(wday(Sys.Date()) == 1){date <- Sys.Date() - 2}else{date <- Sys.Date()}
  return(date)
}

#' Calculate one day PL, from historical file, 
#' 
#' Parameters need to be cleaned up 
#' 
#' Add one day PL to tibble based on saved data.
#' 
#' @param calculated_tbl tibble with calculated P&L data
#' @param strategy_tbl tibble with strategy data
#' @param strategy_trade_mapping_tbl, tibble with strategy trade mappings
#' @param internal_trade_usd_tbl tibble with *active* USD internal trades
#'
#' @return last working day P&L
historical_pl <- function(calculated_tbl,
                          strategy_tbl,
                          strategy_trade_mapping_tbl,
                          internal_trade_usd_tbl,
                          today = get_today()) {
  
  current_dfr <- calculated_tbl %>% 
    dplyr:: select(Portfolio, Level0, Level1, StrategyID, Strategy, `Deal No`, `P&L ($s)`) %>% 
    dplyr::group_by(Portfolio, Level0, Level1, StrategyID, Strategy) %>% 
    dplyr::summarise(`P&L ($s)` = sum(`P&L ($s)`, na.rm = TRUE), .groups = "drop_last") %>% 
    dplyr::ungroup() %>%
    apply_internal_trades(internal_trade_usd_tbl) %>%
    add_column(Date = today, .before = 1) %>% 
    order_table(strategy_tbl %>% select(Portfolio, Level0, Level1, Level2, Level3, Strategy)) %>%
    dplyr::select(StrategyID, `P&L ($s)`)
  
  # Find last saved results
  file_list <- list.files(historical_path, pattern = "^PL")
  sorted_pl_file_names <- sort(file_list, decreasing = TRUE)
  historical_file_path <- file.path(historical_path, 
                                    sorted_pl_file_names[1])
  
  # Check whether we are looking at day one of the month for PL. If so, just show today's PL as
  # thr change rather than difference between end-of-month value and new first day of the month PL.
  
  last_update_df <- read_csv(
    file.path(openlink_path, "FX.last_update"),
    col_types = cols("D", "D")
  )
  last_pl_month <- last_update_df$LastPLDate %>% month()
  historical_pl_month <- str_extract(historical_file_path, "\\d{4}-\\d{2}-\\d{2}") %>% month()
  start_of_month <- if_else(last_pl_month == historical_pl_month, FALSE, TRUE)
  
  if ( start_of_month ) {
    
    full_dfr <- current_dfr %>%
      select(StrategyID, `P&L ($s)`) %>%
      rename(`P&L ($s) 1d change` = `P&L ($s)`)
    
  } else {
    
    dfr <- read_csv(historical_file_path, col_types = "Dcccicddd")
    
    # Add on last working day P&L using StrategyID as key
    last_dfr <- dfr %>% select(StrategyID, `P&L ($s)`)
    
    full_dfr <- current_dfr %>%
      select(StrategyID, `P&L ($s)`) %>%
      left_join(last_dfr %>% select(StrategyID, `P&L ($s)`), by = "StrategyID") %>% 
      mutate(`P&L ($s) 1d change` = `P&L ($s).x` - `P&L ($s).y`) %>% 
      select(-`P&L ($s).y`, -`P&L ($s).x`)
    
  }
  
  full_dfr
  
}


#' Save down all data from Dashboard as it was at the end of the previous day
#' Currently three data sets could be expanded
#' P&L of all allocated strategys; Strategy information table; Trade allocation table.
#' No return 
#' {Revamped on 19 March 2019, P&L data before this date slightly erroneous.}
#' {Updated on 1 May to include lifetime P&L function at the end.}
#' {Updated on 29 Oct 21 to make historical P&L days easier to run.}
#' 
#' Add one day PL to tibble based on saved data. Optionally save the current PL, strategy table &
#' strategy mapping table if these are not present.
#' @param openlink_deal set of tibbles with OL data 
#' @param strategy_tbl tibble with strategy data
#' @param strategy_trade, tibble with strategy trade mappings
#' @param internal_trade_usd_tbl tibble with *active* USD internal trades
#' @param write_path the path that the files will be saved to
#' @param save_history logical
#' @param update_lifetime_pl whether the update lifetime P&L function should be called at the end of the function
#'
#' @return last working day P&L

save_historical_data <- function(openlink_deal,
                                 openlink_path,
                                 strategy_tbl,
                                 strategy_trade_mapping_tbl,
                                 internal_trade_usd_tbl,
                                 write_path = historical_path, 
                                 save_history = TRUE,
                                 update_lifetime_pl = TRUE, 
                                 overwrite = FALSE, 
                                 run_day = Sys.Date()) {
  
  #get calculated table
  calculated_tbl <- get_calculated_tbl(strategy_tbl, strategy_trade_mapping_tbl, openlink_deal)
  
  last_update_df <- read_csv(
    file.path(openlink_path, "FX.last_update"),
    col_types = cols("D", "D")
  )
  
  last_run_day <- last_update_df$LastUpdateDate
  last_pl_date <- last_update_df$LastPLDate
  
  # build file name
  historical_file_path <- file.path(write_path, paste0("PL_", last_pl_date, ".csv"))  
  
  if (last_run_day == run_day & !overwrite) {
    
    flog.debug("%s already exists", historical_file_path)
    
  } else {
    # generate trade specific PV01s to join 
    pv01_hist <- strategy_PV01(calculated_tbl)
    ie01_hist <- strategy_IE01(calculated_tbl)
    
    # today's P&L aggregated over Deal No
    dfr <- calculated_tbl %>% 
      select(Portfolio, Level0, Level1, StrategyID, Strategy, `Deal No`, `P&L ($s)`) %>% 
      group_by(Portfolio, Level0, Level1, StrategyID, Strategy) %>% 
      summarise(`P&L ($s)` = sum(`P&L ($s)`, na.rm = TRUE), .groups = "drop_last") %>% 
      ungroup() %>%
      # rbind here adds back in strategies with no trades allocated to them (but that may have P&L adjustments (internal trades)) that are missed by calced_tbl
      rbind(strategy_tbl %>% 
              select(Portfolio, Level0, Level1, StrategyID, Strategy) %>%
              filter(!(StrategyID %in% unlist(calculated_tbl %>% distinct(StrategyID)))) %>%
              mutate(`P&L ($s)` = 0)
      ) %>% 
      apply_internal_trades(internal_trade_usd_tbl) %>%
      add_column(Date = last_pl_date, .before = 1) %>% 
      left_join(pv01_hist, by = "StrategyID") %>%
      left_join(ie01_hist, by = "StrategyID") %>%
      replace_na(list(`PV01 ($s)` = 0, `IE01 ($s)` = 0)) %>%
      order_table(strategy_tbl %>% select(Portfolio, Level0, Level1, Level2, Level3, Strategy))
    
    if (save_history) {
      flog.debug("Acquiring historical lock ...")
      historical_lock <<- flock::lock(historical_lock_path)
      if (flock::is.locked(historical_lock)) {
        flog.debug("Locking %s", historical_lock_path)
        
        historical_pl_path <- file.path(
          write_path,
          paste0("PL_", last_pl_date, ".csv")
        )
        
        historical_strategy_path <- file.path(
          write_path,
          paste0("Strategy_", last_pl_date, ".csv")
        )
        
        historical_strategy_mapping_path <- file.path(
          write_path,
          paste0("Strategy_Trade_Mapping_", last_pl_date, ".csv")
        )
        
        if ( ! file.exists(historical_pl_path) ) { 
          write_csv(dfr, historical_pl_path)
        }
        
        if ( ! file.exists(historical_strategy_path) ) { 
          write_csv(strategy_tbl, historical_strategy_path)
        }
        
        if ( ! file.exists(historical_strategy_mapping_path) ) {
          write_csv(strategy_trade_mapping_tbl, historical_strategy_mapping_path)
        }
        
      }
      flog.debug("Unlocking %s", historical_lock_path)
      flock::unlock(historical_lock)
      historical_lock <<- NULL
    }
    
    # Save lifetime P&L at end of month
    if(update_lifetime_pl) update_strategy_lifetime_pl(write_path, dfr, last_run_day)
    
  }
  
}



#' Add calculated fields to strategies
#'
#' Joins data tables. Isolates calculations on strategy trade data.
#' @param strategy_tbl tibble of strategies 
#' @param strategy_trade_mapping_tbl  tibble of strategy trade mappings
#' @param openlink_deal tibble of openlink deals
#'
#' @return tibble
#' @examples 
#' ex <- new.env()
#' sys.source("modules/pl_management_module.R", envir = ex)
#' ex$get_calculated_tbl(ex$get_strategy_table(ex$strategy_table_path))
get_calculated_tbl <- function(strategy_tbl, strategy_trade_mapping_tbl,
                               openlink_deal) {
  
  # join tables
  strategy_trade_tbl <- strategy_tbl %>% 
    filter(!is.na(StrategyID)) %>% 
    left_join(strategy_trade_mapping_tbl %>% select(Portfolio, StrategyID, `Deal No`, Percentage), by = c("Portfolio", "StrategyID")) %>%
    filter(!is.na(`Deal No`)) %>% 
    left_join(openlink_deal %>% select(Portfolio, `Instrument Type`, `Ticker / Reference`, `Deal No`, `Total P&L ($s)`, `PV01 ($s)`, `IE01 ($s)`, `Today MTM`), 
              by = c("Portfolio", "Deal No")) %>% 
    mutate(`P&L ($s)` = `Total P&L ($s)` * Percentage / 100)  %>% 
    mutate(`PV01 ($s)` = `PV01 ($s)` * Percentage / 100)  %>%
    mutate(`IE01 ($s)` = `IE01 ($s)` * Percentage / 100) %>%
    mutate(`Today MTM` = `Today MTM` * Percentage/ 100) %>%
    select(-`Total P&L ($s)`) %>%
    rename( `Cash Pos` = `Today MTM`)
  
  # add on summary table metrics
  calculated_tbl <- strategy_trade_tbl %>% 
    mutate(`PV01 short ($s)` = pmax(`PV01 ($s)`, 0)) %>% 
    mutate(`PV01 long ($s)` = pmin(`PV01 ($s)`, 0))  %>%
    mutate(`IE01 short ($s)` = pmax(`IE01 ($s)`, 0)) %>% 
    mutate(`IE01 long ($s)` = pmin(`IE01 ($s)`, 0))
  
  calculated_tbl
}

#' Get openlink deal tibble
#'
#' Converts a list of openlink deal data to USD 
#' @param openlink_data list of openlink tibbles 
#'
#' @return one tibble for all portfolios

get_openlink_deal <- function(openlink_data) {
  
  # find the deal csvs
  is_Deal <- map_lgl(strsplit(basename(names(openlink_data)), "_"), ~ .[2] == "Deal")
  openlink_deal_list <- openlink_data[is_Deal]
  names(openlink_deal_list) <- str_split(names(openlink_deal_list), "_", simplify = TRUE)[,1]
  
  # one tibble for all deals
  openlink_deal <- bind_rows(openlink_deal_list, .id = "Portfolio")
  
  # convert currencies
  get_openlink_fx(openlink_data) %>% 
    right_join(openlink_deal, by = "Currency") %>% 
    mutate(`Total P&L ($s)` = `Total P&L` / FX) %>%
    mutate(`PV01 ($s)` = PV01 / FX) %>%
    mutate(`IE01 ($s)`= IE01 / FX) %>%
    select(-Currency, -FX, -PV01, -IE01, -`Total P&L`)
}


#' Get Openlink Csvs
#'
#' The csvs are returned as a list of correctly typed tibbles.
#' @param openlink_path path to openlink directory
#'
#' @return list of tibbles
#' @examples 
#' ex <- new.env()
#' sys.source("modules/pl_management_module.R", envir = ex)
#' ex$get_openlink_csv(ex$openlink_path)
get_openlink_csv <- function(openlink_path) {
  
  openlink_files <- list.files(openlink_path, full.names = TRUE, pattern = glob2rx("*.csv"))
  
  # Load each file with the correct types
  is_Deal <- map_lgl(strsplit(basename(openlink_files), "_"), ~ .[2] == "Deal")
  is_Agg <- map_lgl(strsplit(basename(openlink_files), "_"), ~ .[2] == "Agg")
  is_Recon <- map_lgl(strsplit(basename(openlink_files), "_"), ~ .[2] == "Recon")
  is_FX <- map_lgl(strsplit(basename(openlink_files), "_"), ~ .[1] == "FX")
  
  openlink_data <- structure(rep(list(tibble()), length(openlink_files)), names = basename(openlink_files))
  openlink_data[is_Deal] <- lapply(
    openlink_files[is_Deal], 
    read_csv, 
    col_types = cols(`Instrument Type` = "c", Currency = "c", `Deal No` = "d", `Ticker / Reference` = "c",
                     `Trade Date` = "D", `Settle Date` = "D", 
                     `Maturity Date` = "D", Position = "d",
                     `Trade or P.EOM` = "d", `Proceeds or P.EOM MTM` = "d", `Today Price` = "d", `Today MTM` = "d",
                     `Total P&L` = "d", `o/w MTM` = "d", `o/w Funding` = "d", PV01 = "d", IE01 = "d", `PV01 Error` = "c")
  )
  
  #openlink_data[is_Deal][["Proceeds or P.EOM MTM"]] <- sapply(openlink_data[is_Deal][["Proceeds or P.EOM MTM"]], function(x) unlist(as.numeric(gsub(",", "", x))))
  #openlink_data[is_Deal][["Proceeds or P.EOM MTM"]] <- lapply(openlink_data[is_Deal][["Proceeds or P.EOM MTM"]], as.numeric)
  #openlink_data[is_Deal][["Today MTM"]] <- sapply(openlink_data[is_Deal][["Today MTM"]], function(x) unlist(as.numeric(gsub(",", "", x))))
  #openlink_data[is_Deal][["Today MTM"]] <- lapply(openlink_data[is_Deal][["Today MTM"]], as.numeric)
  
  openlink_data[is_Agg] <- lapply(
    openlink_files[is_Agg], 
    read_csv, 
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
    
    #col_types = cols(`Instrument Type` = "c", Currency = "c", `Ins No` = "d", `Ticker` = "c",
    #                 `Maturity Date` = "D", Position = "d",
    #                 `Proceeds` = "d", `P.EOM Price` = "d", `P.EOM MTM` = "d",
    #                 `Today Price` = "d", `Today MTM` = "d", `Total P&L` = "d", `o/w MTM` = "d",
    #                 `o/w Funding` = "d")
  )
  
  openlink_data[is_FX] <- lapply(
    openlink_files[is_FX], 
    read_csv, 
    col_types = cols(Date = "D", EUR = "d", GBP = "d", JPY = "d", CAD = "d")
  )
  
  # not currently used
  openlink_data[is_Recon] <- lapply(
    openlink_files[is_Recon], 
    read_csv,
    col_types = cols(
      Currency = col_character(),
      `Instrument Type` = col_character(),
      `MTD P&L` = col_double()
    )
  )
  
  openlink_data
}

#' Get strategy table
#'
#' Load from disk with consistent structure.
#' @param strategy_table_path  path to table
#'
#' @return tibble
#' @examples 
#' ex <- new.env()
#' sys.source("modules/pl_management_module.R", envir = ex)
#' ex$get_strategy_table(ex$strategy_table_path)
get_strategy_table <- function(strategy_table_path) {
  flog.debug("Loading strategy table ...")
  dfr <- read_csv(strategy_table_path, col_types = "icccccclccccD") 
  attr(dfr, "spec") <- NULL
  dfr
}

#' Get strategy trade mappings table
#'
#' load from disk with consistent structure.
#' @param strategy_trade_table_path path to table
#'
#' @return tibble
#' @examples 
#' ex <- new.env()
#' sys.source("modules/pl_management_module.R", envir = ex)
#' ex$get_strategy_trade_mapping(ex$strategy_trade_table_path)
get_strategy_trade_mapping <- function(strategy_trade_table_path) {
  flog.debug("Loading mappings table ...")
  dfr <- read_csv(strategy_trade_table_path, col_types = "ccicdd")
  attr(dfr, "spec") <- NULL
  dfr
}

#' Get lifetime P&L to date for all strategies from historic file 
#'
#' 
#' @param historic_file_path path of historical files  
#'
#' @return datatable with strategy number and lifetime P&L to end last month  
#' 
#' @examples 
#' get_strategy_lifetime_pl("C:/temp/P&L Config/historical/")
#' 
#' 

get_strategy_lifetime_pl_lst_mth <- function(historic_file_path){
  flog.debug("Loading lifetime P&Ls ...")
  read_csv(
    file.path(historic_file_path, "lifetime_strategy_pl.csv"),
    col_types = cols(StrategyID = col_integer(), Date = col_date(), PL = col_double())
  ) %>%
    filter(Date == max(Date)) %>%
    rename(LifePL = PL) %>%
    select(StrategyID, LifePL)
}




#' Write end of month strategy P&L  to file cumalitively
#'
#' load from disk with consistent structure.
#' @param historic_file_path_input path with location of historical data to be aggregated
#' @param calculated_table A frame of PLs for the current day
#' 
#' @examples 
#' update_strategy_lifetime_pl("C:/temp/P&L Config/historical/", calculated_table)
#' 
update_strategy_lifetime_pl <- function(historic_file_path_input,
                                        calculated_table,
                                        last_run_day) {
  
  file_list <- list.files(historic_file_path_input, pattern = glob2rx("PL*.csv"))
  pl_dates <- str_extract(file_list, "\\d{4}-\\d{2}-\\d{2}")
  previous_pl_date <- as.Date(max(pl_dates))
  
  # Only update if we have started a new month
  if ( month(last_run_day) != month(previous_pl_date) ) {
    
    # Format out current PLs appropriately
    new_eom_pl <- calculated_table %>%
      select(StrategyID, Date, `P&L ($s)`) %>%
      rename(PL = `P&L ($s)`)
    
    # Load the previous version of the lifetime PL frame
    previous_lifetime_pl <- read_csv(
      file.path(historic_file_path_input, "lifetime_strategy_pl.csv"),
      col_types = cols(StrategyID = col_integer(), Date = col_date(), PL = col_double())
    )
    
    # Check that the update was not done earlier in the day
    if ( max(new_eom_pl$Date) != max(previous_lifetime_pl$Date) ) {
      
      # Select the lifetime PLs for the last month
      last_lifetime_pl <- previous_lifetime_pl %>% filter(Date == max(Date))
      
      # Add on the previous lifetime PL to get the new lifetime PL
      new_lifetime_pl <- new_eom_pl %>%
        left_join(
          last_lifetime_pl %>% select(-Date),
          by = "StrategyID",
          suffix = c("_new", "_previous")
        ) %>%
        replace(is.na(.), 0) %>%
        mutate(PL = PL_new + PL_previous) %>%
        select(StrategyID, Date, PL)
      
      # Join the new lifetime PL onto the end of the previous lifetime PL frame ...  
      total_lifetime_pl <- previous_lifetime_pl %>%
        bind_rows(new_lifetime_pl) %>%
        arrange(Date, StrategyID)
      
      # ... and write the output to disk
      total_lifetime_pl %>% write_csv(
        file.path(historic_file_path_input, "lifetime_strategy_pl.csv")
      )
      
      flog.debug("Updated lifetime PLs")
      
    } else {
      flog.debug("Lifetime PLs already updated")
    }
    
  } else {
    flog.debug("No update of lifetime PLs required")
  }
  
}


#' Create table of CIXs to query 
#'
#' load from disk with consistent structure
#' pull CIX last price values 
#' 
#' @param strategy_tbl tibble of current strategies from which the CIX fields can be extracted
#'
#' @return tibble with strategyID, CIX and current level  
#' 
#' @examples 
#' strategy_CIX_table(strategy_tbl, FALSE)
#' 
#' 

strategy_CIX_table <- function(strategy_tbl){
  
  dfr <- strategy_tbl %>% 
    select(c(StrategyID, CIX)) %>%
    mutate(Live = bloomberg_CIX(CIX))
  
  dfr
  
}