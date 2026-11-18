library(ggplot2)
library(gridExtra)
library(grid)


###############  Setup, parameters to be fitted from data #####################################################

# when observing success or failure, add a small fraction of evidence to the opposite outcome
pseudocount = 0

difficulty <- seq(0, 1, length.out = 100) # task difficulty

motivation_threshold = 10 # how much force a baby can apply - fit to individuals

# desired succeess probability used to estimate the number of required attempts
threshold_probability_of_success = 0.9 # fit to population

# converting estimated number of attempts to cumulative force
k = 1 # fit to population

# scales the difference between own and adult skill when deciding to ask for help 
k_helpseeking_willingness = 1 # fit to individuals, reasonable interval is [0.5, 2]

# skill/difficulty - probability of success in one attempt given by Beta distribution -- uninformative prior
baby_success <- list(a=1, b=1) # density <- dbeta(difficulty, baby_success$a, baby_success$b)
adult_success <- list(a=1, b=1)

weigh_adult_evidence = 1 # when learning about task rewards, how much do ww weigh adult attempts in relation to our own
# in [0..1] - currently unused

source('~/Downloads/UfT_DEVELOPMENT/R_code_baby_effort/force_affect_motivation_utils.R')


###############  Experimental steps #####################################################

#observe adult outcome, and update own and adult skill
# adult_outcome if TRUE if successfull, false if failed
observe_adult = function(adult_outcome, adult_success, baby_success) {
  adult_success = update_skill(adult_outcome, adult_success)
  baby_success = update_skill(adult_outcome, baby_success, weigh_adult_evidence)
  return ( list (a=adult_success, b=baby_success) )
}

# estimate force
# predicting how much force the task needs, and how much we would invest
#estimate_cumulative_force = function(baby_success) {
#  required_attempts = force_required(expectation(baby_success)) 
#  motivated_attempts <- motivation_sigmoid(required_attempts, motivation_threshold)*required_attempts
#  return (motivated_attempts)
#}










