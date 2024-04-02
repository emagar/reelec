philips et al

delta r_jt      = beta_0j - alpha_j r_jt-1 + beta_Lj x_t-1 + beta_Sj delta x_t + error

(r_jt - r_jt-1) = beta_0j - alpha_j r_jt-1 + beta_Lj x_t-1 + beta_Sj (x_t - x_t-1) + error


delta r_t      = beta_0 - alpha r_t-1 + betaL x_t-1 + betaS delta x_t + error

(r_t - r_t-1)  = beta_0 - alpha r_t-1 + betaL x_t-1 + betaS (x_t - x_t-1) + error

r_t            = beta_0 + r_t-1 - alpha r_t-1 + betaL x_t-1 + betaS (x_t - x_t-1) + error

r_t            = beta_0 + (1 - alpha) r_t-1 + betaL x_t-1 + betaS (x_t - x_t-1) + error

if alpha = 1: r_t            = beta_0 + betaL x_t-1 + betaS (x_t - x_t-1) + error

if alpha = 1: r_t            = beta_0 + betaL x_t-1 + betaS x_t - betaS x_t-1 + error

if alpha = 1 & betaL = betaS: r_t            = beta_0 + betaS x_t + error




