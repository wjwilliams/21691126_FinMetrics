Roll_optimizer <- function(return_mat, EOQ_datevec, LookBackSel = 36){

    return_df_used <- return_mat %>% filter(date >= EOQ_datevec %m-% months(LookBackSel))

    if(return_df_used %>% nrow() < LookBackSel) return(NULL) # PRO TIP - return NULL effectively skips the iteration when binding....

    return_mat_Nodate <- data.matrix(return_df_used[, -1])
    # Simple Sample covariance and mean for the lookback period:
    Sigma <- RiskPortfolios::covEstimation(return_mat_Nodate)
    Mu <- return_mat %>% summarise(across(-date, ~prod(1+.)^(1/n())-1)) %>% purrr::as_vector()


    My_Weights <-

            optim_foo(Mu, Sigma, LB, UB, Amat, bvec, meq) %>%

        mutate(date = EOQ_datevec , Look_Back_Period = LookBackSel)

}