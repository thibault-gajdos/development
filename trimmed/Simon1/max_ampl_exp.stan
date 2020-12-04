// generated with brms 2.14.0
functions {
}
data {
  int<lower=1> N;  // total number of observations
  vector[N] Y;  // response variable
  int<lower=1> K_b1;  // number of population-level effects
  matrix[N, K_b1] X_b1;  // population-level design matrix
  int<lower=1> K_b2;  // number of population-level effects
  matrix[N, K_b2] X_b2;  // population-level design matrix
  int<lower=1> K_b3;  // number of population-level effects
  matrix[N, K_b3] X_b3;  // population-level design matrix
  // covariate vectors for non-linear functions
  vector[N] C_1;
  int prior_only;  // should the likelihood be ignored?
}
transformed data {
}
parameters {
  vector<lower=0>[K_b1] b_b1;  // population-level effects
  vector<lower=0>[K_b2] b_b2;  // population-level effects
  vector<lower=0>[K_b3] b_b3;  // population-level effects
  real<lower=0> sigma;  // residual SD
}
transformed parameters {
}
model {
  // likelihood including all constants
  if (!prior_only) {
    // initialize linear predictor term
    vector[N] nlp_b1 = X_b1 * b_b1;
    // initialize linear predictor term
    vector[N] nlp_b2 = X_b2 * b_b2;
    // initialize linear predictor term
    vector[N] nlp_b3 = X_b3 * b_b3;
    // initialize non-linear predictor term
    vector[N] mu;
    for (n in 1:N) {
      // compute non-linear predictor values
      mu[n] = nlp_b1[n] * exp( - nlp_b2[n] * C_1[n]) + nlp_b3[n];
    }
    target += normal_lpdf(Y | mu, sigma);
  }
  // priors including all constants
  target += normal_lpdf(b_b1 | 0, 1)
    - 1 * normal_lccdf(0 | 0, 1);
  target += normal_lpdf(b_b2 | 0, 1)
    - 1 * normal_lccdf(0 | 0, 1);
  target += normal_lpdf(b_b3 | 0, 1)
    - 1 * normal_lccdf(0 | 0, 1);
  target += student_t_lpdf(sigma | 3, 0, 2.5)
    - 1 * student_t_lccdf(0 | 3, 0, 2.5);
}
generated quantities {
}
