#Function
library(tidyverse)
library(rmsfuns)

rebalance_and_calculate_returns <- function(data, fund_name, w_cap) {
    library(tidyverse)
    library(rmsfuns)


     rebalance_col <- data %>%
        rename("weight" = {{ fund_name }}) %>%
        filter(date %in% Rebalance_days) %>%
        mutate(RebalanceTime = format(date, "%Y%B%A")) %>%
        group_by(RebalanceTime) %>%
        arrange(desc(weight)) %>%
        ungroup() %>%
        arrange(date) %>%
        select(date, Tickers, weight, RebalanceTime)

    # df_Cons <- rebalance_col %>% filter(date == first(date))
    # W_Cap = 0.8
    Proportional_Cap_Foo <- function(df_Cons, W_Cap = 0.05){

        # Let's require a specific form from the user... Alerting when it does not adhere this form
        if( !"weight" %in% names(df_Cons)) stop("... for Calc capping to work, provide weight column called 'weight'")

        if( !"date" %in% names(df_Cons)) stop("... for Calc capping to work, provide date column called 'date'")

        if( !"Tickers" %in% names(df_Cons)) stop("... for Calc capping to work, provide id column called 'Tickers'")

        # First identify the cap breachers...
        Breachers <-
            df_Cons %>% filter(weight > W_Cap) %>% pull(Tickers)

        # Now keep track of breachers, and add to it to ensure they remain at 10%:
        if(length(Breachers) > 0) {

            while( df_Cons %>% filter(weight > W_Cap) %>% nrow() > 0 ) {


                df_Cons <-

                    bind_rows(

                        df_Cons %>% filter(Tickers %in% Breachers) %>% mutate(weight = W_Cap),

                        df_Cons %>% filter(!Tickers %in% Breachers) %>%
                            mutate(weight = (weight / sum(weight, na.rm=T)) * (1-length(Breachers)*W_Cap) )

                    )

                Breachers <- c(Breachers, df_Cons %>% filter(weight > W_Cap) %>% pull(Tickers))

            }

            if( sum(df_Cons$weight, na.rm=T) > 1.001 | sum(df_Cons$weight, na.rm=T) < 0.999 | max(df_Cons$weight, na.rm = T) > W_Cap) {

                stop( glue::glue("For the Generic weight trimming function used: the weight trimming causes non unit
      summation of weights for date: {unique(df_Cons$date)}...\n
      The restriction could be too low or some dates have extreme concentrations...") )

            }

        } else {

        }

        df_Cons

    }
    # Now, to map this across all the dates, we can use purrr::map_df as follows:
    Capped_df <- rebalance_col %>%
        # Split our df into groups (where the groups here are the rebalance dates:
        group_split(RebalanceTime) %>%
        # Apply the function Proportional_Cap_Foo to each rebalancing date:
        map_df(~Proportional_Cap_Foo(., W_Cap = w_cap)) %>% select(-RebalanceTime)

    wts <- Capped_df %>%
        tbl_xts(cols_to_xts = weight, spread_by = Tickers)

    rts <- data %>%
        filter(Tickers %in% unique(Capped_df$Tickers)) %>%
        tbl_xts(cols_to_xts = Return, spread_by = Tickers)

    wts[is.na(wts)] <- 0
    rts[is.na(rts)] <- 0

    Idx <- rmsfuns::Safe_Return.portfolio(R = rts, weights = wts, lag_weights = TRUE) %>%
        # Let's make this a tibble:
        xts_tbl() %>%
        rename({{ fund_name }} := "portfolio.returns")

    return(Idx)
}