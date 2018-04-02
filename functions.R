
dgramcharlier <- function(x, mu3=0, mu4=3) {
  psi <- function(x) ( 1 + mu3*(x^3 - 3*x)/6 + (mu4 - 3)*(x^4 - 6*x^2 + 3)/24 )
  dnorm(x)*psi(x)
}

dgramcharlier_adj <- function(x, mu3=0, mu4=3) {
  psi <- function(x) ( 1 + mu3*(x^3 - 3*x)/6 + (mu4 - 3)*(x^4 - 6*x^2 + 3)/24 )
  .gamma <- 1 + (mu3^2)/6 + ((mu4 - 3)^2)/24
  dnorm(x)*(psi(x)^2)/.gamma
}

gc_check <- function(x) {
  function(parms) {
    gc <- dgramcharlier(x, mu3=parms[1], mu4=parms[2])
    if (any( gc < 0 ))
      FALSE
    else
      TRUE
  }
}

gclogLik <- function(x) {
  function(parms) {
    gc <- dgramcharlier(x, mu3=parms[1], mu4=parms[2])
    if (any( gc < 0 ))
      NA
    else
      -sum(log(gc))
  }
}

gcUnconstrainedlogLik <- function(x) {
  function(parms) {
    parms <- uncons_regionD(parms[1], parms[2])
    gc <- dgramcharlier(x, mu3=parms[1], mu4=parms[2])
    gc <- abs(gc)
    -sum(log(gc))
  }
}
d_poly <- function(x) {
  2*(x^3 - 3*x)^2 - 1*(x^2 - 1)*(x^4 - 6*x^2 + 3)
}

mu3 <- function(x) {
  -12 * (x^3 - 3*x)/d_poly(x)
}

mu4 <-function(x) {
  24 * (x^2 - 1)/d_poly(x) + 3
}

regionD <- function(x) {
  dm <- cbind(x=x, mu3=mu3(x), mu4=mu4(x))
  idx <- order(dm[,'mu4'])
  dm[idx,]
}

adj_seq <- function(start, end, a=-3, length.out=100) {
  x <- seq(start^a, end^a, length.out=length.out)
  x^(1/a)
}

create_uncons_regionD <- function() {
  x <- adj_seq(sqrt(3), 100)
  rd <- regionD(x)
  rd_curve <- approxfun(c(3, rd[,'mu4'], 7), c(0, rd[,'mu3'], 0))
  ff <- function(x, a, b) a + (b - a)/(1 + exp(-x))
  function(mu3p, mu4p) {
    mu4 <- ff(mu4p, 3, 7)
    mu3_l <- rd_curve(mu4)
    mu3_u <- -mu3_l
    mu3 <- ff(mu3p, mu3_l, mu3_u)
    c(mu3, mu4)
  }
}

uncons_regionD <- create_uncons_regionD()

library(zoo)

.garch_likelihood <- function(.rets) {
  function(x) {
    # x <- c(...)
    omega <- x[1]
    alpha <- x[2]
    beta <- x[3]
    garch <- var.garch(.rets, omega, alpha, beta)
    # sum( 0.5*log(2*pi) + 0.5*log(garch) + 0.5*(.rets[-1]^2)/garch )
    # dens_ <- dnorm(.rets, sd=sqrt(garch))
    dens_ <- dgramcharlier(.rets/sqrt(garch), mu3=0, mu4=3)/sqrt(garch)
    llk <- - sum( log( dens_ ) )
    llk
  }
}

.garch_gc_likelihood <- function(.rets) {
  function(x) {
    omega <- x[1]
    alpha <- x[2]
    beta <- x[3]
    
    parms <- uncons_regionD(x[4], x[5])
    skew <- parms[1]
    kurt <- parms[2]
    
    garch <- var.garch(.rets, omega, alpha, beta)
    dens_ <- dgramcharlier(.rets/sqrt(garch), mu3=skew, mu4=kurt)/sqrt(garch)
    # dens_ <- dens_[-1]
    dens_[dens_ < 0] <- 0
    
    - sum( log( dens_ ) )
  }
}

.garch_gc_likelihood2 <- function(.rets) {
  function(x) {
    omega <- x[1]
    alpha <- x[2]
    beta <- x[3]
    
    garch <- var.garch(.rets, omega, alpha, beta)
    dens_ <- dgramcharlier(.rets[-1]/sqrt(garch), mu3=x[4], mu4=x[5])/sqrt(garch)
    dens_ <- dens_[-1]
    dens_ <- abs(dens_)
    
    - sum( log( dens_ ) )
  }
}

garch_fit <- function(rets) {
  # initialization of parameters
  alpha <- 0.1
  beta <- 0.8
  omega <- (1 - alpha - beta)*var(rets)
  mu <- mean(rets)
  # scaling and optimization bounds
  small <- 1e-6
  lo <- c(small, small, small, -1, 3)
  hi <- c(100*abs(mu), 1-small, 1-small, 1, 7)
  # setting up likelihood function
  garch_likelihood <- .garch_likelihood(rets)
  # optimization
  # res <- optim(c(omega, alpha, beta), garch_likelihood, lower=lo, upper=hi, method='L-BFGS-B')
  res <- nlminb(c(omega, alpha, beta), garch_likelihood, lower=lo, upper=hi, control=list(x.tol=1e-8, trace=0))
  # setting up returning values
  par <- res$par
  parms <- list(omega=par[1], alpha=par[2], beta=par[3], skewness=par[4], kurtosis=par[5])
  parms$gamma <- 1 - parms$alpha - parms$beta
  parms$long_term_variance <- parms$omega/parms$gamma
  parms$long_term_volatility <- sqrt(parms$long_term_variance)
  parms$long_term_volatility_annu <- parms$long_term_volatility*sqrt(252)
  parms
}

var.garch <- function(rets, omega, alpha, beta) {
  e2 <- rets^2
  e2t <- omega + alpha*c(mean(e2), e2[-length(e2)])
  filter(e2t, beta, "recursive", init=mean(e2))
}

tr_par <- function(par) {
  res <- c(par[1:3], uncons_regionD(par[4], par[5]))
  names(res) <- c('omega', 'alpha', 'beta', 'skewness', 'kurtosis')
  res
}

garch_fit_nlopt <- function(start, objective, lower, upper) {
  
  gl_d_omega <- function(...) {
    x <- c(...)
    shock <- 1e-7
    ( objective(c(x[1]+shock, x[2], x[3], x[4], x[5])) - objective(c(x[1]-shock, x[2], x[3], x[4], x[5])) )/(2*shock)
  }
  
  gl_d_alpha <- function(...) {
    x <- c(...)
    shock <- 1e-3
    ( objective(c(x[1], x[2]+shock, x[3], x[4], x[5])) - objective(c(x[1], x[2]-shock, x[3], x[4], x[5])) )/(2*shock)
  }
  
  gl_d_beta <- function(...) {
    x <- c(...)
    shock <- 1e-2
    ( objective(c(x[1], x[2], x[3]+shock, x[4], x[5])) - objective(c(x[1], x[2], x[3]-shock, x[4], x[5])) )/(2*shock)
  }
  
  gl_d_skewness <- function(...) {
    x <- c(...)
    shock <- 1e-2
    ( objective(c(x[1], x[2], x[3], x[4]+shock, x[5])) - objective(c(x[1], x[2], x[3], x[4]-shock, x[5])) )/(2*shock)
  }
  
  gl_d_kurtosis <- function(...) {
    x <- c(...)
    shock <- 1e-2
    ( objective(c(x[1], x[2], x[3], x[4], x[5]+shock)) - objective(c(x[1], x[2], x[3], x[4], x[5]-shock)) )/(2*shock)
  }
  
  # optimization
  
  eval_grad_f0 <- function(x) {
    c(gl_d_omega(x[1], x[2], x[3], x[4], x[5]),
      gl_d_alpha(x[1], x[2], x[3], x[4], x[5]),
      gl_d_beta(x[1], x[2], x[3], x[4], x[5]),
      gl_d_skewness(x[1], x[2], x[3], x[4], x[5]),
      gl_d_kurtosis(x[1], x[2], x[3], x[4], x[5])
    )
  }
  
  eval_g0 <- function(x) {
    sum(x[2:3]) - 1
  }
  
  eval_jac_g0 <- function(x) {
    c(0, 1, 1, 0, 0)
  }
  
  # optimization
  res <- nloptr::nloptr(x0 = start,
                        eval_f = objective,
                        eval_grad_f = eval_grad_f0,
                        lb = lower,
                        ub = upper,
                        eval_g_ineq = eval_g0,
                        eval_jac_g_ineq = eval_jac_g0,
                        opts = list(algorithm="NLOPT_LD_MMA",
                                    xtol_rel=1.0e-8, maxeval=100, print_level=3))
  
  message(res$message)
  
  res
}

# simutate r_t = \sqrt{ h_t } e_t
# where e_t follows any random variate
# h_t = w + a r_{t-1} + b h_{t-1}

garch_sim <- function(omega, alpha, beta, et, skip=100) {
  .garch_var <- function(omega, alpha, beta) {
    function(r, h) {
      omega + alpha*r^2 + beta*h
    }
  }
  
  g11 <- .garch_var(omega, alpha, beta)
  
  h_ <- numeric(length(et))
  r_ <- numeric(length(et))
  h_[1] = omega/(1-alpha-beta)
  r_[1] = sqrt(h_[1]) * et[1]
  for (ix in 2:length(et)) {
    h_[ix] = g11(r_[ix-1], h_[ix-1])
    r_[ix] = sqrt(h_[ix]) * et[ix]
  }
  
  tail(r_, length(et)-skip)
}

format_par <- function(x) {
  par <- tr_par(x[1:5])
  c(par, x[6])
}
