## MARKOV MODEL SECONDARY CARE

library(dplyr)
library(future)
library(future.apply)


## constants 

samples <- 100
cycles <- 12
years <- 5
people <- 170 
states <- c("home", "callout", "conveyance", "aeuse", "admission", "tempres", "death")
discountRate <- 0.035

## probabilities to sample 

# call 


# ambulance callout  

amb_callout <- function(type= "intervention") {

  if (type=="intervention") {
    rbeta(1, 10, 160) ## add real 
  } else (
    rbeta(1, 17, 153) ## add real 
  )
  
}

# ambulance conveyance 

amb_conveyance <- function(type= "intervention") {
  
  if (type=="intervention") {
    rbeta(1, 10, 160) ## add real 
  } else (
    rbeta(1, 17, 153) ## add real 
  )
  
}

# A&E use

ae_use <- function(type= "intervention") {
  
  if (type=="intervention") {
    rbeta(1, 10, 160) ## add real 
  } else (
    rbeta(1, 17, 153) ## add real 
  )
  
  
}

# admit to hospital 

hospital_admit <- function(type= "intervention") {
  
  if (type=="intervention") {
    rbeta(1, 10, 160) ## add real 
  } else (
    rbeta(1, 17, 153) ## add real 
  )
  
}

# hospital length of stay 

hospital_los <- function(type="intervention") {
  if (type == "intervention") {
    rpois(1, 8 - 1) + 1 # add real 
  } else {
    rpois(1, 9 - 1) + 1 # add real 
  }
}


death_rate <- function() {
  0.017455
  ## 70 age simple av from https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/lifeexpectancies/datasets/mortalityratesqxbysingleyearofage#:~:text=Dataset%20Mortality%20rates%20(qx)%2C%20by%20single%20year,Contact:%20Demography%20team.%20*%2010%20December%202025.
}

# temp residence probability 
tempres_prob <- function(type="intervention") {
  
  if (type=="intervention") {
    rbeta(1, 5, 165) # add real 
  } else {
    rbeta(1, 6, 164) # add real 
  }
  
}

# temp residence length length of stay 

tempres_los <- function(type="intervention") {
  if (type == "intervention") {
    rpois(1, 8 - 1) + 1 # add real 
  } else {
    rpois(1, 9 - 1) + 1 # add real 
  }
}


## costs

setupCost <- function() {
  50 #add correct
}
monthlyCost <- function() {
  
  15 # add correct 
}
calloutCost <- function() {
  100 ## add real 
}
conveyanceCost <- function() {
  100 ## add real
}
aeCost <- function() {
  100 #add real 
}
admissionDayCost <- function() {
  100 ## add real
} 
tempresDayCost <- function() {
  100 ## add real 
}

## functions 

# Convert annual probability to monthly probability
annual_to_monthly_prob <- function(p_annual) {
  if (any(p_annual < 0 | p_annual > 1, na.rm = TRUE)) {
    stop("Probabilities must be between 0 and 1")
  }
  1 - (1 - p_annual)^(1 / 12)
}

transition_matrix <-function(type="intervention") {
  
  callout <- annual_to_monthly_prob(amb_callout(type))
  conveyance <- annual_to_monthly_prob(amb_conveyance(type))
  ae <- annual_to_monthly_prob(ae_use(type))
  admission <- annual_to_monthly_prob(hospital_admit(type))
  tempres <- annual_to_monthly_prob(tempres_prob(type))
  death  <- annual_to_monthly_prob(death_rate())
  home <- 1 - callout - conveyance - ae - admission - tempres
  
  tm <- matrix(nrow = length(states), ncol = length(states))
  colnames(tm) <- states
  rownames(tm) <- states 
  
  probs <- c(home, callout, conveyance, ae, admission, tempres, death)
  for (s in states[1:5]) {
    tm[s, ] <- probs
  } 
  
  tm["tempres", ] <- c(1 - death, 0, 0, 0, 0 , 0, death)
  tm['death', ] <- c(0, 0, 0, 0, 0, 0, 1)
  
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
      
      if (chain[j, "state"] == "admission") {
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
