## MARKOV MODEL SECONDARY CARE

library(dplyr)
library(future)
library(future.apply)
library(foreach)


## constants 

samples <- 10000
cycles <- 12
years <- 5
people <- 170 
states <- c("home", "callout", "ae_convey_noadmit", "ae_no_convey_noadmit", "admit_via_ae_convey", "admit_via_ae_no_convey", "admit_no_ae_convey", "admit_no_ae_no_convey", "tempres", "death")
discountRate <- 0.035

# --- Calibration ratios (multipliers) ---
CAL_CALL_OUT   <- 1.131291261
CAL_CONVEY     <- 1.032876847
CAL_AE         <- 1.201096261
CAL_ADMISSION  <- 1.119387811

## probabilities to sample 

# call 


# ambulance callout  

amb_callout <- function(type = "intervention", cycles = 12) {
  
  # base monthly probability (as you had it)
  monthlyRate <- 276 * 1.2 / (170 * cycles)
  
  # apply calibration
  monthlyRate <- monthlyRate * CAL_CALL_OUT
  
  # clamp
  p <- max(min(monthlyRate, 1), 0)
  return(p)
}


# ambulance conveyance 

amb_conveyance <- function(type= "intervention") {
  
  if (type=="intervention") {
   p <- rbeta(1, 158, 276 - 158)
  } else {
   p <-  rbeta(1, 70, 106 - 70)
  }
  
  return((p))
}


# A&E use
# A&E use with shared intercept + baseline trend, plus group-specific post terms
# A&E use with shared intercept + baseline trend.
# Intervention uses its own post-intercept and post-slope.
# Counterfactual ("control") uses control post terms REBASED to the intervention baseline
# (so the implied absolute change at the control baseline is re-expressed on the intervention baseline).

ae_use <- function(type = "intervention", cycles = 12) {
  
  T <- cycles
  
  # --- shared baseline (from ITS) on log-count scale ---
  alpha  <- 0.45788266   # Intercept
  b_time <- 0.04071905   # Baseline trend (per month)
  
  # --- observed PRE totals over 12 months (for the population size in `people`) ---
  pre_total_12m_int  <- 476
  pre_total_12m_ctrl <- 345
  
  pre_mean_month_int  <- pre_total_12m_int  / 12
  pre_mean_month_ctrl <- pre_total_12m_ctrl / 12
  
  # months relative to intervention point:
  t_pre  <- -11:0
  t_post <- 1:T
  
  # baseline predicted counts (shape) from intercept + baseline trend
  mu_pre_shape  <- exp(alpha + b_time * t_pre)
  mu_post_shape <- exp(alpha + b_time * t_post)
  
  # scale baseline so its PRE mean matches observed PRE mean for the INTERVENTION group
  scale_k  <- pre_mean_month_int / mean(mu_pre_shape)
  mu0_post <- scale_k * mu_post_shape  # shared baseline monthly counts post (on intervention baseline)
  
  # --- draw post terms (log-count scale) ---
  if (type == "intervention") {
    
    b_post  <- rnorm(1, mean = 0.11493318, sd = 0.174)
    b_slope <- rnorm(1, mean = -0.1003871, sd = 0.0305)
    
  } else {
    
    # draw control-group coefficients (as estimated on the control baseline)
    b_post_ctrl  <- rnorm(1, mean = 0.02219856, sd = 0.176)
    b_slope_ctrl <- rnorm(1, mean = -0.0589255, sd = 0.0229)
    
    # --- REBASE control post-intercept to intervention baseline ---
    RR0_ctrl   <- exp(b_post_ctrl)
    delta0     <- (RR0_ctrl - 1) * pre_mean_month_ctrl          # implied absolute change at control baseline
    RR0_rebase <- 1 + delta0 / pre_mean_month_int               # re-expressed relative change at intervention baseline
    RR0_rebase <- max(RR0_rebase, 1e-12)                        # guard against numerical issues
    b_post     <- log(RR0_rebase)
    
    # --- REBASE control post-slope to intervention baseline (mean effect over 12 months) ---
    # Interpret slope as an average effect over 12 months by using the mean multiplier across months 1..12,
    # then re-express that absolute change on the intervention baseline, and convert back to a slope.
    t12          <- 1:12
    RR_slope_bar <- mean(exp(b_slope_ctrl * t12))               # average relative multiplier over 12 months (control baseline)
    delta_slope  <- (RR_slope_bar - 1) * pre_mean_month_ctrl    # implied absolute change at control baseline
    RR_s_rebase  <- 1 + delta_slope / pre_mean_month_int        # re-expressed relative change at intervention baseline
    RR_s_rebase  <- max(RR_s_rebase, 1e-12)
    
    # Convert rebased average multiplier back to a per-month slope.
    # Using 6.5 matches the mean month index over 1..12 (consistent with "average over 12 months").
    b_slope <- log(RR_s_rebase) / 6.5
  }
  
  # apply level + slope change over post months
  mu1_post <- mu0_post * exp(b_post + b_slope * t_post)
  
  # mean monthly count over months 1..T
  eventsPerMonth1 <- mean(mu1_post)
  
  # convert to per-person monthly probability (Bernoulli approximation) + calibration
  p_month <- (eventsPerMonth1 * CAL_AE) / people
  p_month <- max(min(p_month, 1), 0)
  
  return(p_month)
}


# admit to hospital 

# Emergency (non-elective) admissions — shared baseline anchored to the intervention pre total.
# Intervention uses its own post-intercept and post-slope.
# Counterfactual ("control") uses control post terms REBASED to the intervention baseline
# (so the implied absolute change at the control baseline is re-expressed on the intervention baseline).

hospital_admit <- function(type = "intervention", cycles = 12) {
  
  T <- cycles
  
  # --- shared baseline (from ITS) on log-count scale ---
  alpha  <- 0.233   # Intercept
  b_time <- 0.054   # Baseline trend (per month)
  
  # --- observed PRE totals over 12 months (for the population size in `people`) ---
  pre_total_12m_int  <- 296
  pre_total_12m_ctrl <- 217
  
  pre_mean_month_int  <- pre_total_12m_int  / 12
  pre_mean_month_ctrl <- pre_total_12m_ctrl / 12
  
  # time indices (relative to intervention point)
  t_pre  <- -11:0
  t_post <- 1:T
  
  # baseline predicted counts (shape) from intercept + baseline trend
  mu_pre_shape  <- exp(alpha + b_time * t_pre)
  mu_post_shape <- exp(alpha + b_time * t_post)
  
  # scale baseline so its PRE mean matches observed PRE mean for the INTERVENTION group
  scale_k  <- pre_mean_month_int / mean(mu_pre_shape)
  mu0_post <- scale_k * mu_post_shape  # shared baseline monthly counts post (on intervention baseline)
  
  # --- draw post terms (log-count scale) ---
  if (type == "intervention") {
    
    b_post  <- rnorm(1, mean = -0.009, sd = 0.18)
    b_slope <- rnorm(1, mean = -0.111, sd = 0.0337)
    
  } else {
    
    # draw control-group coefficients (as estimated on the control baseline)
    b_post_ctrl  <- rnorm(1, mean =  0.026, sd = 0.148)
    b_slope_ctrl <- rnorm(1, mean = -0.049, sd = 0.019)
    
    # --- REBASE control post-intercept to intervention baseline ---
    RR0_ctrl   <- exp(b_post_ctrl)
    delta0     <- (RR0_ctrl - 1) * pre_mean_month_ctrl          # implied absolute change at control baseline
    RR0_rebase <- 1 + delta0 / pre_mean_month_int               # re-expressed relative change at intervention baseline
    RR0_rebase <- max(RR0_rebase, 1e-12)                        # guard against numerical issues
    b_post     <- log(RR0_rebase)
    
    # --- REBASE control post-slope to intervention baseline (mean effect over 12 months) ---
    t12          <- 1:12
    RR_slope_bar <- mean(exp(b_slope_ctrl * t12))               # average relative multiplier over 12 months (control baseline)
    delta_slope  <- (RR_slope_bar - 1) * pre_mean_month_ctrl    # implied absolute change at control baseline
    RR_s_rebase  <- 1 + delta_slope / pre_mean_month_int        # re-expressed relative change at intervention baseline
    RR_s_rebase  <- max(RR_s_rebase, 1e-12)
    
    # Convert rebased average multiplier back to a per-month slope.
    # Using 6.5 matches the mean month index over 1..12.
    b_slope <- log(RR_s_rebase) / 6.5
  }
  
  # apply level + slope change over post months
  mu1_post <- mu0_post * exp(b_post + b_slope * t_post)
  
  # mean monthly count over months 1..T
  eventsPerMonth1 <- mean(mu1_post)
  
  # convert to per-person monthly probability (Bernoulli approximation) + calibration
  p_month <- (eventsPerMonth1 * CAL_ADMISSION) / people
  p_month <- max(min(p_month, 1), 0)
  
  return(p_month)
}


# hospital length of stay 

hospital_los <- function(type="intervention") {
  if (type == "intervention") {
    rpois(1, 9.91 - 1) + 1 # add real 
  } else {
    rpois(1, 10.3 - 1) + 1 # add real 
  }
}


death_rate <- function(year) {
  annual_to_monthly_prob((0.025484 + 0.036249)/2)
  ## 79 age simple av from https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/lifeexpectancies/datasets/mortalityratesqxbysingleyearofage#:~:text=Dataset%20Mortality%20rates%20(qx)%2C%20by%20single%20year,Contact:%20Demography%20team.%20*%2010%20December%202025.
}

# temp residence probability 
tempres_prob <- function(type="intervention") {
  
  if (type=="intervention") {
    p <- rbeta(1, 5, 165) # add real 
  } else {
    p <- rbeta(1, 5, 165) # add real 
  }
  
  return(annual_to_monthly_prob(p))
}

# temp residence length length of stay 

tempres_los <- function(type="intervention") {
  if (type == "intervention") {
    rpois(1, 78.5 - 1) + 1 # add real 
  } else {
    rpois(1, 228	 - 1) + 1 # add real 
  }
}

## costs

fixedCost <- function() {
  194544.00/people# b&D costs final.xlsx
}

setupCost <- function() {
  list(hardware=704.77, provider_incentive=110.37) # b&D costs final.xlsx
}


monthlyCost <- function() {
  
  list(licences=23.75, tech_partner=16.04) # b&D costs final.xlsx

}

calloutCost <- function() {
  327  # b&D costs final.xlsx
}

conveyanceCost <- function() {
  132 # b&D costs final.xlsx
}
aeCost <- function() {
  280.17  # b&D costs final.xlsx
}

admissionCost <- function() {
  2711 # b&D costs final.xlsx
}

admissionDayCost <- function() {
  345  # b&D costs final.xlsx
} 
tempresDayCost <- function() {
  161 ## add real 
}

## functions 

# Convert annual probability to monthly probability
annual_to_monthly_prob <- function(p_annual) {
  if (any(p_annual < 0 | p_annual > 1, na.rm = TRUE)) {
    stop("Probabilities must be between 0 and 1")
  }
  1 - (1 - p_annual)^(1 / 12)
}

calc_buckets <- function(type = "intervention",
                                    tol = 1e-12) {
  
  ## find prob callout and conveyance from callout
  p_callout <- amb_callout(type)
  p_convey <- amb_conveyance(type)
  
  callout_no_more <- p_callout * (1 - p_convey)
  
  ##estimate % that do not go to A&E from conveyance - small at 5%
  rate_convey_no_ae <- rbeta(1, 190*0.05, 190*0.95)
  adjust = 190 * (1 - rate_convey_no_ae) ## based on 158 over 10 months conveyances 
  rate_convey_of_ae <- rbeta(1, adjust , 595 - (adjust)) ## based on 595 A&E attendances over 12 months 
  
  #draw prob ae 
  p_ae <- ae_use(type)
  
  #find conveyance to admit probablity with no a&E
  p_convey_to_admit <- p_callout * p_convey * rate_convey_no_ae
  
  ## adjust for convey to ae rate and no-convey 
  p_convey_ae <- min(p_ae * rate_convey_of_ae, (p_callout * p_convey) - p_convey_to_admit)
  p_ae_no_convey <- p_ae - p_convey_ae 
  
  ## find admit probabilioty 
  p_admit <- hospital_admit(type)
  ## find probailkity ae led to admit 
  p_admit_via_ae <- p_admit * rbeta(1, 355, 240)
  
  ## create admit from ae probs, conveyed and not conveyed. 
  p_admit_via_ae_convey <- p_admit_via_ae * (p_convey_ae/(p_convey_ae + p_ae_no_convey))
  p_admit_via_ae_no_convey <- p_admit_via_ae - p_admit_via_ae_convey
  
  #find prob for admitted but no a&E
  p_admit_no_ae <- max(0, p_admit -  p_admit_via_ae - p_convey_to_admit)
  
  ## AMEND FOR NO ADMIT FROM AE 
  p_convey_ae_notadmit = max(0, p_convey_ae - p_admit_via_ae_convey)
  p_no_convey_ae_noadmit = max(0, p_ae_no_convey - p_admit_via_ae_no_convey)
  
  return(list(
    p_callout = callout_no_more,
    p_ae_convey_noadmit = p_convey_ae_notadmit,
    p_ae_no_convey_noadmit = p_no_convey_ae_noadmit,
    p_admit_via_ae_convey = p_admit_via_ae_convey,
    p_admit_via_ae_no_convey = p_admit_via_ae_no_convey,
    p_admit_no_ae_convey = p_convey_to_admit,
    p_admit_no_ae_no_convey = p_admit_no_ae
    
    # 
  ))
  
}


transition_matrix <-function(type="intervention") {
  
  max_tries <- 1000
  i <- 1
  
  repeat {
    probs <- calc_buckets(type)
    
    if (all(probs >= 0)) break
    
    i <- i + 1
    if (i > max_tries) {
      stop("calc_buckets kept producing negative probabilities")
    }
  }
  
  
  tempres <- (tempres_prob(type))
  death  <- (death_rate())
  
 # p_callout_only <- callout * (1 - conveyance)
 # p_convey_total <- callout * conveyance
  
  home <- 1 - probs$p_callout - probs$p_ae_convey_noadmit - probs$p_ae_no_convey_noadmit - 
    probs$p_admit_via_ae_convey - probs$p_admit_via_ae_no_convey - probs$p_admit_no_ae_convey - probs$p_admit_no_ae_no_convey -
    tempres - death 

  
  tm <- matrix(nrow = length(states), ncol = length(states))
  colnames(tm) <- states
  rownames(tm) <- states 
  
  probs <- c(home, probs$p_callout , probs$p_ae_convey_noadmit , probs$p_ae_no_convey_noadmit , 
               probs$p_admit_via_ae_convey , probs$p_admit_via_ae_no_convey , probs$p_admit_no_ae_convey , probs$p_admit_no_ae_no_convey ,
               tempres , death )
  for (s in states[1:(length(states) - 2)]) {
    tm[s, ] <- probs
  } 
  
  tm["tempres", ] <- c(1 - death, 0, 0, 0, 0 , 0, 0, 0, 0 , death)
  tm['death', ] <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 1)
  
  return(tm)
}

one_sample <- function(type) {
  tm <- transition_matrix(type)
  sample <- data.frame()
  for (i in 1:(people)) {
    chain <- matrix(nrow = cycles * years + 1, ncol = 5)
    colnames(chain) <- c("state", "admission_los", "tempres_los", "patient", "step")
    chain[1, "state"] <- 'home'
    chain[1, "step"] <- 0
    for (j in 2:(cycles * years + 1)) {
      probs <- tm[chain[j-1, "state"], ]
      probs <- probs/sum(probs)
      chain[j, "state"] <- sample(colnames(tm),1, prob = probs)
      
      if (grepl("admit_", chain[j, "state"])) {
        chain[j, "admission_los"] = hospital_los(type)
      } else if (chain[j, "state"] == "tempres") {
        chain[j, "tempres_los"] = tempres_los(type)
      }
      
      chain[j, "patient"] <- i
      chain[j, "step"] <- j -1
    }
    
    sample <- rbind(sample, chain)
  }
  
  return(sample)
}

sampleChain <- function(max_retries = 5) {
  
  sample <- data.frame()
  
  start <- Sys.time()
  
  for (i in 1:samples) {
    
    attempt <- 1
    success <- FALSE
    
    while (!success && attempt <= max_retries) {
      
      res_i <- tryCatch(
        {
          intervention <- one_sample("intervention")
          intervention$type <- "intervention"
          intervention$sample <- i
          intervention <- addCosts(intervention)
          
          control <- one_sample("control")
          control$type <- "control"
          control$sample <- i
          control <- addCosts(control)
          
          list(intervention = intervention, control = control)
        },
        error = function(e) {
          message(
            "Iteration ", i,
            " failed on attempt ", attempt,
            ": ", conditionMessage(e)
          )
          NULL
        }
      )
      
      if (!is.null(res_i)) {
        sample <- rbind(sample, res_i$intervention)
        sample <- rbind(sample, res_i$control)
        success <- TRUE
        message("Done ", i)
      } else {
        attempt <- attempt + 1
      }
    }
    
  
    if (!success) {
      warning("Iteration ", i, " failed after ", max_retries, " attempts — skipping.")
    }
    
    if (i %% 50 == 0) {
      time <- Sys.time() - start
      message("Seconds elapsed", time)
      message("Average per sample", round(time/i, 1))
      message("Time left seconds: ", (time/i) * (samples - i) )
    }
  }
  
  return(sample)
}
sampleChain_parallel <- function(
    samples,
    workers = NULL,
    seed = TRUE,
    packages = NULL,
    globals = TRUE  # TRUE = auto-detect globals (usually enough)
) {
  stopifnot(is.numeric(samples), length(samples) == 1, samples >= 1)
  
  if (!is.null(workers)) {
    future::plan(future::multisession, workers = workers)  # cross-platform
  }
  
  out_list <- future.apply::future_lapply(
    X = seq_len(samples),
    FUN = function(i) {
    
      # your existing code unchanged
      intervention <- one_sample("intervention")
      intervention$type <- "intervention"
      intervention$sample <- i
      intervention <- addCosts(intervention)
      
      control <- one_sample("control")
      control$type <- "control"
      control$sample <- i
      control <- addCosts(control)
      
      writeLines(
        as.character(i),
        file.path("progress", paste0("done-", i, ".txt"))
      )
      
      rbind(intervention, control)
    },
    future.seed = seed,
    future.packages = packages,
    future.globals = globals
  )
  
  out <- do.call(rbind, out_list)
  rownames(out) <- NULL
  out
}


sampleChain_foreach <- function(samples, workers = parallel::detectCores() - 1) {
  stopifnot(samples >= 1)
  
  cl <- parallel::makeCluster(workers)
  on.exit(parallel::stopCluster(cl), add = TRUE)
  doParallel::registerDoParallel(cl)
  value_names <- names(Filter(
    Negate(is.function),
    as.list(.GlobalEnv)
  ))
  fn_names <- names(Filter(is.function, as.list(.GlobalEnv)))
  
  # export only what you need (be explicit)
  parallel::clusterExport(cl, varlist = c(fn_names, value_names), envir = environment())
  
  res <- foreach::foreach(
    i = seq_len(samples),
    .combine = "rbind",
    .packages = c("dplyr")  # add any you truly need inside one_sample/addCosts
  ) %dopar% {
    # If you need reproducibility across workers:
    # doRNG::%dorng% and set.seed() once outside (see note below)
    
    intervention <- one_sample("intervention")
    intervention$type <- "intervention"
    intervention$sample <- i
    intervention <- addCosts(intervention)
    
    control <- one_sample("control")
    control$type <- "control"
    control$sample <- i
    control <- addCosts(control)
    
    rbind(intervention, control)
    
    writeLines(
      as.character(i),
      file.path("progress", paste0("done-", i, ".txt"))
    )
  }
  
  rownames(res) <- NULL
  res
}

addCosts <- function(df) {
  fixed <- fixedCost()
  setup <- setupCost()
  monthly <- monthlyCost()
  callout <- calloutCost()
  conveyance <- conveyanceCost()
  ae <- aeCost()
  admission <- admissionCost()
  admissionDay <- admissionDayCost()
  tempres <- tempresDayCost()
  
  df$fixed_cost <- ifelse(
    df$step == 1, fixed, 0
  )
  
  df$setup_hardware <- ifelse(
    df$step == 1, setup$hardware, 0
  )
  
  df$setup_provider_incentive <- ifelse(
    df$step %in% c(1, 13, 25, 37, 49), setup$provider_incentive, 0
  )
  
  df$monthly_licence_cost <- monthly$licences
  df$monthly_tech_partner <- monthly$tech_partner
  
  df$health_cost <- ifelse(
    df$state == "callout", callout, ifelse(
      df$state == "ae_convey_noadmit", callout + conveyance + ae, ifelse(
        df$state == "ae_no_convey_noadmit",ae, ifelse(
          df$state == "admit_via_ae_convey", callout + conveyance + ae + admission + (as.numeric(df$admission_los) * admissionDay), ifelse(
            df$state == "admit_via_ae_no_convey", ae + admission + (as.numeric(df$admission_los) * admissionDay), ifelse(
              df$state == "admit_no_ae_convey", callout + conveyance + admission + (as.numeric(df$admission_los) * admissionDay), ifelse(
                df$state == "admit_no_ae_no_convey", admission + (as.numeric(df$admission_los) * admissionDay), ifelse(
                  df$state == "tempres", as.numeric(df$tempres_los) * tempres, 0
                )
              )
            )
          )
        )
      )
    )
  )
  return(df)
}

##states <- c("home", "callout", "ae_convey_noadmit", "ae_no_convey_noadmit", "admit_via_ae_convey", "admit_via_ae_no_convey", "admit_no_ae_convey", "admit_no_ae_no_convey", "tempres", "death")

discount <- function(df, colname) {
  df[, colname] * ((1 - discountRate) ^ (df$year - 1))
}

## analysis

#future::plan(future::multisession, workers = 10)
#future::plan()
#future::nbrOfWorkers()

#options(future.debug = TRUE)

start <- Sys.time()

s <- one_sample("intervention")

Sys.time() - start 

dir.create("progress", showWarnings = FALSE)


#df <- sampleChain_parallel(
  #samples = samples,    
  #seed = TRUE,
  #packages = NULL        
#)

df <- sampleChain_foreach(samples)

#df <- sampleChain()

save.image("results.Rdata")

df <- df[df$step != 0, ]
df$year <- ceiling(as.numeric(df$step)/cycles)

df$fixed_cost_discount <- discount(df, "fixed_cost")
df$health_cost_discount <- discount(df, "health_cost")

df$setup_hardware_cost_discount <- discount(df, "setup_hardware")
df$setup_provider_incentive_cost_discount <- discount(df, "setup_provider_incentive")

df$monthly_licence_cost_discount <- discount(df, "monthly_licence_cost")
df$monthly_tech_partner_cost_discount <- discount(df, "monthly_tech_partner")

eventsByYearSample <- df %>% 
  group_by(sample, year, type, state) %>%
  summarise(freq = n())
             
write.csv(eventsByYearSample, "events.csv", row.names = FALSE)


costsByYearSample <- df %>%
  group_by(year, sample, type) %>%
  summarise(
    # undiscounted
    "fixed_cost" = sum(fixed_cost, na.rm = TRUE),
    "health_cost" = sum(health_cost, na.rm = TRUE),
    "setup_hardware" = sum(setup_hardware, na.rm = TRUE),
    "setup_provider_incentive" = sum(setup_provider_incentive, na.rm = TRUE),
    "monthly_licence" = sum(monthly_licence_cost, na.rm = TRUE),
    "monthly_tech_partner" = sum(monthly_tech_partner, na.rm = TRUE),

    # discounted
    "fixed_cost_discount" = sum(fixed_cost_discount, na.rm = TRUE),
    "health_cost_discount" = sum(health_cost_discount, na.rm = TRUE),
    "setup_hardware_cost_discount" = sum(setup_hardware_cost_discount, na.rm = TRUE),
    "setup_provider_incentive_cost_discount" = sum(setup_provider_incentive_cost_discount, na.rm = TRUE),
    "monthly_licence_cost_discount" = sum(monthly_licence_cost_discount, na.rm = TRUE),
    "monthly_tech_partner_cost_discount" = sum(monthly_tech_partner_cost_discount, na.rm = TRUE),

    "total_difference_discount" = sum(fixed_cost_discount + setup_hardware_cost_discount + setup_provider_incentive_cost_discount +
                                               monthly_licence_cost_discount + monthly_tech_partner_cost_discount, na.rm = TRUE),
    .groups = "drop"
  )

write.csv(costsByYearSample, "costs.csv", row.names = FALSE )

costsBySample <- df %>%
  group_by(sample, type) %>%
  summarise(
    # undiscounted
    "fixed_cost" = sum(fixed_cost, na.rm = TRUE),
    "health_cost" = sum(health_cost, na.rm = TRUE),
    "setup_hardware" = sum(setup_hardware, na.rm = TRUE),
    "setup_provider_incentive" = sum(setup_provider_incentive, na.rm = TRUE),
    "monthly_licence" = sum(monthly_licence_cost, na.rm = TRUE),
    "monthly_tech_partner" = sum(monthly_tech_partner, na.rm = TRUE),
    
    # discounted
    "fixed_cost_discount" = sum(fixed_cost_discount, na.rm = TRUE),
    "health_cost_discount" = sum(health_cost_discount, na.rm = TRUE),
    "setup_hardware_cost_discount" = sum(setup_hardware_cost_discount, na.rm = TRUE),
    "setup_provider_incentive_cost_discount" = sum(setup_provider_incentive_cost_discount, na.rm = TRUE),
    "monthly_licence_cost_discount" = sum(monthly_licence_cost_discount, na.rm = TRUE),
    "monthly_tech_partner_cost_discount" = sum(monthly_tech_partner_cost_discount, na.rm = TRUE)
    .groups = "drop"
  )


hist(costsBySample[costsBySample$type=="intervention", ]$health_cost_discount, breaks = 10)

costs <- merge(costsByYearSample[costsByYearSample$type == "intervention", c("sample", "year", "total_intervention_cost_discount", "health_cost_discount")], 
               costsByYearSample[costsByYearSample$type == "control", c("sample", "year", "health_cost_discount")], 
                by = c("sample", "year")) 

names(costs) <- c("sample", "year", "total_intervention_cost_discount_int", "health_cost_discount_int", "health_cost_discount_con")

costs <- costs %>%
  mutate(difference = health_cost_discount_con - health_cost_discount_int)


costsCI <- costs %>%
  group_by(year) %>%
  summarise(difference_LCI = quantile(difference, 0.025, na.rm = TRUE ) ,
            difference_UCI = quantile(difference, 0.975, na.rm = TRUE ))


hist(
  costs$difference,
  breaks = seq(
    min(costs$difference, na.rm = TRUE),
    max(costs$difference, na.rm = TRUE),
    length.out = 31
  ),
  xaxt = "n",          # suppress default x-axis
  xlab = "Health benefit (£)"
)

axis(
  1,
  labels = scales::label_dollar(prefix = "£")(axTicks(1)),
  at = axTicks(1)
)

costs_sample <- costs %>%
  group_by(sample, year = 1) %>%
  summarise(total_intervention_cost_discount_int = sum(total_intervention_cost_discount_int, na.rm = TRUE), 
            health_cost_discount_int = sum(health_cost_discount_int, na.rm = TRUE),
            health_cost_discount_con = sum(health_cost_discount_con, na.rm = TRUE))

write.csv(costs_sample, "costs_sample.csv", row.names = FALSE)

write.csv(costsCI, "costsCI.csv", row.names = FALSE)

costsBySample <- costsBySample %>%
  mutate(intervention_cost_discount = fixed_cost_discount + setup_hardware_cost_discount + setup_provider_incentive_cost_discount + 
           monthly_licence_cost_discount + monthly_tech_partner_cost_discount) %>%
  mutate(intervention_cost_discount = ifelse(type == "control", 0, intervention_cost_discount)) %>%
  mutate(difference_discounted = health_cost_discount - intervention_cost_discount) %>%
  arrange( sample, type) %>%
  mutate(benefit = ifelse(type == "control", NA, lag(difference_discounted, 1) - difference_discounted)) %>%
  mutate(bcr = benefit / intervention_cost_discount)

t.test(costsBySample[costsBySample$type == "intervention", ]$bcr)

averageCosts <- costsBySample %>%
  group_by(type) %>%
  summarise(fixed_cost_discount = )