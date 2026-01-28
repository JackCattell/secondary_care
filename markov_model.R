## MARKOV MODEL SECONDARY CARE

library(dplyr)

## constants 

samples <- 10
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
    rbeta(1, 17, 153) ## add real 
  } else (
    rbeta(1, 10, 160) ## add real 
  )
  
}

# ambulance conveyance 

amb_conveyance <- function(type= "intervention") {
  
  if (type=="intervention") {
    rbeta(1, 17, 153) ## add real 
  } else (
    rbeta(1, 10, 160) ## add real 
  )
  
}

# A&E use

ae_use <- function(type= "intervention") {
  
  if (type=="intervention") {
    rbeta(1, 17, 153) ## add real 
  } else (
    rbeta(1, 10, 160) ## add real 
  )
  
}

# admit to hospital 

hospital_admit <- function(type= "intervention") {
  
  if (type=="intervention") {
    rbeta(1, 17, 153) ## add real 
  } else (
    rbeta(1, 10, 160) ## add real 
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
  1000 #add correct
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

df <- sampleChain()
df <- df[df$step != 0, ]
df$year <- ceiling(as.numeric(df$step)/cycles)
df[df$type == "control", ]$intervention_cost <- 0
df$intervention_cost_discount <- df$intervention_cost * ((1 - discountRate) ^ (df$year - 1))
df$health_cost_discount <- df$health_cost * ((1 - discountRate) ^ (df$year - 1))

costsByYear <- df %>%
              group_by(year, sample, type) %>%
              summarise(health = sum(health_cost, na.rm = TRUE), intervention = sum(intervention_cost, na.rm = TRUE)) %>%
              group_by(year, type) %>%
              summarise(mean_intervention = mean(intervention, na.rm = TRUE), health_cost = mean(health, na.rm = TRUE))
  
