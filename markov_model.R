## MARKOV MODEL SECONDARY CARE

library(dplyr)
library(future)
library(future.apply)


## constants 

samples <- 100
cycles <- 12
years <- 5
people <- 170 
states <- c("home", "callout", "ae_convey_noadmit", "ae_no_convey_noadmit", "admit_via_ae_convey", "admit_via_ae_no_convey", "admit_no_ae_convey", "admit_no_ae_no_convey", "tempres", "death")
discountRate <- 0.035

## probabilities to sample 

# call 


# ambulance callout  

amb_callout <- function(type = "intervention", cycles = 12) {
  monthlyRate <- 276 * 1.2 / (170 * cycles)
  p <- 1 - exp(-monthlyRate)
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

ae_use <- function(type= "intervention") {
  
  eventsPerYear = 2.729
  eventsPerCycle = eventsPerYear/cycles
  
  if (type=="intervention") {
    slopeMonthly =  rnorm(1, -0.6525, 0.1983) ## 
  } else (
    slopeMonthly =  rnorm(1, 0, 1) 
  )
  
  meanRateRatio = exp(slopeMonthly)
  cycleRate = eventsPerYear * meanRateRatio/cycles 
  
  return(1 - exp(-cycleRate)) ## Linden, A. (2015). Conducting Interrupted Time-Series Analysis for Single- and Multiple-Group Comparisons. Stata Journal, 15(2), 480–500.Briggs, A., Sculpher, M., Claxton, K., et al. (2012). Probabilistic Sensitivity Analysis: Statistical Assumptions and Methods.
  
} 

# admit to hospital 

hospital_admit <- function(type= "intervention") {
  
  eventsPerYear = 2.088 + (384/170)

  if (type=="intervention") {
    slopeMonthly =  rnorm(1, -0.7215, 0.2193) ## 
  } else (
    slopeMonthly =  rnorm(1, 0, 1) 
  )
  
  meanRateRatio = exp(slopeMonthly)
  cycleRate = eventsPerYear * meanRateRatio/cycles 
  
  return(1 - exp(-cycleRate)) ## Linden, A. (2015). Conducting Interrupted Time-Series Analysis for Single- and Multiple-Group Comparisons. Stata Journal, 15(2), 480–500.Briggs, A., Sculpher, M., Claxton, K., et al. (2012). Probabilistic Sensitivity Analysis: Statistical Assumptions and Methods.
  
}

# hospital length of stay 

hospital_los <- function(type="intervention") {
  if (type == "intervention") {
    rpois(1, 9.91 - 1) + 1 # add real 
  } else {
    rpois(1, 10.3 - 1) + 1 # add real 
  }
}


death_rate <- function() {
  annual_to_monthly_prob((0.025484 + 0.036249)/2)
  ## 70 age simple av from https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/lifeexpectancies/datasets/mortalityratesqxbysingleyearofage#:~:text=Dataset%20Mortality%20rates%20(qx)%2C%20by%20single%20year,Contact:%20Demography%20team.%20*%2010%20December%202025.
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
  327 # b&D costs final.xlsx
}

conveyanceCost <- function() {
  132 # b&D costs final.xlsx
}
aeCost <- function() {
  280.17 # b&D costs final.xlsx
}

admissionCost <- function() {
  2711 # b&D costs final.xlsx
}

admissionDayCost <- function() {
  345 # b&D costs final.xlsx
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
  
  p_callout <- amb_callout(type)
  p_convey <- amb_conveyance(type)
  
  callout_no_more <- p_callout * (1 - p_convey)
  
  rate_convey_no_ae <- rbeta(1, 170*0.05, 170*0.95)
  adjust = 158 * 1.2 * (1 - rate_convey_no_ae) ## based on 158 over 10 months conveyances 
  rate_convey_of_ae <- rbeta(1, adjust , 595 - (adjust)) ## based on 595 A&E attendances over 12 months 
  
  p_ae <- ae_use(type)
  
  p_convey_to_admit <- p_callout * p_convey * rate_convey_no_ae
  
  p_convey_ae <- min(p_ae * rate_convey_of_ae, (p_callout * p_convey) - p_convey_to_admit)
  p_ae_no_convey <- p_ae - p_convey_ae 
  
  p_admit <- hospital_admit(type)
  p_admit_via_ae <- p_admit * rbeta(1, 355, 384)
  
  p_admit_via_ae_convey <- p_admit_via_ae * (p_convey_ae/(p_convey_ae + p_ae_no_convey))
  p_admit_via_ae_no_convey <- p_admit_via_ae - p_admit_via_ae_convey
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
  
  probs <- calc_buckets()
  
  
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

sampleChain <- function() {
  
  sample <- data.frame()
  
  for (i in 1:samples) {
    
    intervention <- one_sample("intervention")
    intervention$type <- "intervention"
    intervention$sample <- i
    intervention <- addCosts(intervention)
    
    control <- one_sample("control")
    control$type <- "control"
    control$sample <- i
    control <- addCosts(control)
    
    sample <- rbind(sample, intervention)
    sample <- rbind(sample, control)
    
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


addCosts <- function(df) {
  setup <- setupCost()
  monthly <- monthlyCost()
  callout <- calloutCost()
  conveyance <- conveyanceCost()
  ae <- aeCost()
  admissionDay <- admissionDayCost()
  tempres <- tempresDayCost()
  
  df$intervention_cost <- ifelse(
    df$step == 1, setup + monthly, monthly
  )
  
  df$health_cost <- ifelse(
    df$state == "callout", callout, ifelse(
      df$state == "conveyance", callout + conveyance, ifelse(
        df$state == "aeuse", callout + conveyance + ae, ifelse(
          df$state == "admission", callout + conveyance + ae + (as.numeric(df$admission_los) * admissionDay), ifelse(
            df$state == "tempres", as.numeric(df$tempres_los) * tempres, 0
          )
        )
      )
    )
  )
  return(df)
}

## analysis


df <- sampleChain_parallel(
  samples = samples,     # or a number like 1000
  workers = parallel::detectCores() - 1 ,
  seed = TRUE,
  packages = NULL        # add c("dplyr") if your functions rely on it
)
df <- df[df$step != 0, ]
df$year <- ceiling(as.numeric(df$step)/cycles)
df[df$type == "control", ]$intervention_cost <- 0
df$intervention_cost_discount <- df$intervention_cost * ((1 - discountRate) ^ (df$year - 1))
df$health_cost_discount <- df$health_cost * ((1 - discountRate) ^ (df$year - 1))

costsByYearSample <- df %>%
              group_by(year, sample, type) %>%
              summarise(health = sum(health_cost, na.rm = TRUE), intervention = sum(intervention_cost, na.rm = TRUE))

costsBySample <- df %>%
  group_by(sample, type) %>%
  summarise(health = sum(health_cost, na.rm = TRUE), intervention = sum(intervention_cost, na.rm = TRUE))

hist(costsBySample[costsBySample$type=="intervention", ]$health, breaks = 10)

costs <- merge(df[df$type == "intervention", c("sample", "year", "patient", "step", "intervention_cost", "intervention_cost_discount", "health_cost", "health_cost_discount")], 
               df[df$type == "control", c("sample", "year", "patient", "step", "intervention_cost", "intervention_cost_discount", "health_cost", "health_cost_discount")], 
               by = c("sample", "year", "patient", "step"))
costs$intervention_cost.y <- NULL
costs$intervention_cost_discount.y <- NULL

names(costs) <- c("sample", "year", "patient", "step", "intervention_cost", "intervention_cost_discounted", "health_cost_int", "heatlh_cost_int_discount", "health_cost_con", "health_cost_con_discount" )

costsAggregate <- costs %>%
  group_by(sample) %>%
  summarise(intervention_cost = sum(intervention_cost, na.rm = TRUE), intervention_cost_discounted = sum(intervention_cost_discounted, na.rm = TRUE),
            health_cost_int = sum(health_cost_int, na.rm = TRUE), heatlh_cost_int_discount = sum(heatlh_cost_int_discount, na.rm = TRUE), health_cost_con = sum(health_cost_con, na.rm = TRUE), health_cost_con_discount = sum(health_cost_con_discount, na.rm = TRUE)) %>%
  mutate(total_int_cost_discount = heatlh_cost_int_discount +  intervention_cost_discounted)

t.test(costsAggregate$total_int_cost_discount, costsAggregate$health_cost_con_discount)
