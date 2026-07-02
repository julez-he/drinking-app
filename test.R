library(tidyverse)
library(mrgsolve)
library(patchwork)


mod <- mrgsolve::mread_cache("model", "model")

build_dataset <- function(
              input_rate,
              input_amt,
              input_age,
              input_wt,
              rep = 1,
              cp_0 = NA) {
  rate_mmolh <- input_amt / (input_rate)            # mmol/h
  tibble(
    ID   = 1:rep,
    TIME = c(0),
    AMT  = c(input_amt),      # dose row, obs row
    RATE = c(rate_mmolh),     # infusion rate for the dose
    CMT  = c(1),               # depot, central
    EVID = c(1),               # dose, observation
    AGE  = input_age,
    WT   = input_wt,
    CPO = ifelse(is.na(cp_0), 0, cp_0)
  )
}



build_dataset(  input_rate = 400,
  input_amt = 720*0.195*0.789, # ml * v/v
  input_age = 21,
  input_wt = 45,
  rep = 1,
  cp_0 = 0.5
) -> dataset_1




fit <- mrgsolve::mrgsim_df(mod %>% zero_re(), dataset_1, end = 24 * 60)

p1 <- fit %>%
  as_tibble() %>%
  ggplot(aes(x = TIME, y = IPRED)) +
  geom_line() +
  labs(title = "Ethanol Concentration Over Time",
       x = "Time (minutes)",
       y = "Ethanol Concentration (g/L)") +
  theme_minimal()

p2 <- fit %>% 
  as_tibble() %>%
  ggplot(aes(x = TIME, y = CUM_GZT)) +
  geom_line() +
  labs(title = NULL,
       x = "Time (minutes)",
       y = "Cumulative time in Golden Zone (0.2 - 0.6 Promille)") +
  theme_minimal()


p1 / p2






build_dataset <- function(
    input_rate,  # vector of infusion durations (h)
    input_amt,   # vector of total doses (mmol)
    input_age,   # vector of ages (years)
    input_wt     # vector of weights (kg)
) {
  
  combs <- expand_grid(
    RATE = input_rate,
    AMT   = input_amt,
    AGE   = input_age,
    WT    = input_wt
  )
  
  dataset <- combs %>%
    mutate(
      ID   = row_number(),
      TIME = 0,                         # dose at time zero
      CMT  = 1,                         # depot compartment
      EVID = 1                          # dosing event
    ) %>%
    select(ID, TIME, AMT, RATE, CMT, EVID, AGE, WT)
  
  return(dataset)
}





sim_convert <- function(AMT, RATE, AGE, WT) {
  df <- build_dataset(
    input_rate = RATE,
    input_amt  = AMT,
    input_age  = AGE,
    input_wt   = WT
  )
  
  raw_out <- mrgsim_df(mod %>% zero_re(), df, end = 24*60, carry_out = c("AMT", "RATE"))
  
  out <- raw_out %>%
    group_by(ID) %>%
    summarise(
      AMT = max(AMT, na.rm = TRUE),
      RATE = max(RATE, na.rm = TRUE),
      C_MAX = max(CMAX, na.rm = TRUE),
      T_MAX = max(TMAX, na.rm = TRUE),
      CUM_GZT = max(CUM_GZT, na.rm = TRUE),
      MIN_ABOVE_003 = sum(GT003),
      MIN_ABOVE_005 = sum(GT005),
      MIN_ABOVE_010 = sum(GT010),
      MIN_ABOVE_020 = sum(GT020),
      MIN_ABOVE_050 = sum(GT050),
      MIN_ABOVE_100 = sum(GT100),
      MIN_ABOVE_123 = sum(GT123),
      MIN_ABOVE_137 = sum(GT137),
      BELOW_005 = NA
    )
  
  
}





optimize_sim <- function(
    param,           # e.g. "C_MAX", "CUM_GZT", "MIN_ABOVE_050", ...
    age, wt,         # fixed covariates
    init_amt, rate,
    lower_amt, upper_amt,
    target_value
) {
  # objective: NEGATIVE because optim() MINimizes
  obj_fun <- function(x) {
    res <- sim_convert(AMT  = x[1],
                       RATE = rate,
                       AGE  = age,
                       WT   = wt)
    # assume one row returned
    abs(target_value - res[[param]])
  }
  
  opt <- optim(
    par    = c(init_amt, rate),
    fn     = obj_fun,
    method = "L-BFGS-B",
    lower  = c(lower_amt),
    upper  = c(upper_amt)
  )
  
  # package up results
  best_val <- -opt$value
  tibble(
    AMT           = opt$par[1],
    RATE          = rate,
    !!param       := best_val
  )
}



best <- optimize_sim(
  param       = "C_MAX",
  age         = 21,
  wt          = 45,
  init_amt    = 50,
  rate        = 50,
  lower_amt   = 10,
  upper_amt   = 100,
  
)


print(best)


























