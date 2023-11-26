optim_foo <- function(Mu, Sigma, LB, UB, Amat, bvec, meq){

    w.opt <-
        quadprog::solve.QP(Dmat = Sigma,
                           dvec = Mu,
                           Amat = Amat,
                           bvec = bvec,
                           meq = meq)$solution

    result.QP <- tibble(stocks = colnames(Sigma), weight = w.opt)
    result.QP
}
