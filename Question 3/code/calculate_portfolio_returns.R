calculate_portfolio_returns <- function(data, sector_name, fund_name) {
    library(tidyverse)
    library(rmsfuns)
    library(glue)

    # Filter data for the specified sector
    sector_wts <- data %>%
        filter(Sector == sector_name) %>%
        group_by(date) %>%
        mutate(!!as.symbol(fund_name) := !!as.symbol(fund_name) / sum(!!as.symbol(fund_name))) %>%
        ungroup()

    # Separate weights for the specified fund
    wts <- sector_wts %>%
        select(date, Tickers, !!as.symbol(fund_name)) %>%
        spread(Tickers, !!as.symbol(fund_name)) %>%
        tbl_xts()
    wts[is.na(wts)] <- 0

    # Create an xts data frame for returns
    returns <- sector_wts %>%
        select(date, Tickers, Return) %>%
        spread(Tickers, Return) %>%
        tbl_xts()
    returns[is.na(returns)] <- 0

    # Calculate Safe Portfolio returns
    portfolio_rts <- Safe_Return.portfolio(returns, wts, lag_weights = TRUE, contribution = TRUE, verbose = TRUE, value = 1000, geometric = TRUE)

    # Extract relevant components
    cont <- portfolio_rts$"contribution" %>% xts_tbl()
    BPwts <- portfolio_rts$"BOP.Weight" %>% xts_tbl()
    value <- portfolio_rts$BOP.Value %>% xts_tbl()

    # Bind all components together
    result <- left_join(
        sector_wts %>% select(date, Tickers, Return),
        BPwts %>% gather(Tickers, weights, -date),
        by = c("date", "Tickers")
    ) %>%
        left_join(., value %>% gather(Tickers, value_held, -date), by = c("date", "Tickers")) %>%
        left_join(., cont %>% gather(Tickers, Contribution, -date), by = c("date", "Tickers")) %>%
        group_by(date) %>%
        summarise(PortfolioReturn = sum(Return * weights, na.rm = TRUE)) %>%
        filter(PortfolioReturn != 0)

    return(result)
}
