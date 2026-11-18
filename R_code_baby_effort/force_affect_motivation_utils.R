# Models for Force, Motivation, Affect


# expectation of beta distribution
expectation = function(dist) {
  # expectation of success based on this probability
  return( dist$a / (dist$a + dist$b) )
}

expected_value <- function(values, density) {
  return(sum(values * density) / sum(density))
}


# compute the probability that the task can be completed successfully in n_attempts
# if probability of success is given by a Beta distribution parametrised by a, b
prob_of_success <- function(n_attempts, skill) {
  a = skill$a 
  b = skill$b
  p_success <- a / (a + b)
  p_all_failures <- (1 - p_success)^n_attempts
  return(1 - p_all_failures)
}


force_required <-function(prob_of_success_per_attempt, required_probability = threshold_probability_of_success) {
  # assuming a fixed amount of force per attempt
  # estimate the number of attempts to achieve a threshold probability of success
  n_attempts = log(1 - required_probability) / log(1 - prob_of_success_per_attempt)
  return(n_attempts) # this will be a fractional number, unless we return ceiling(n_attempts)
}

# required force will growing to infinity as the task becomes harder
# motivation controls how much force one is willing and able to apply
motivation_sigmoid <- function(x, threshold = 12) {
  return ( 1 / (1 + exp(x - threshold ))  )
}

#  information-theoretic surprise
quantify_surprise <- function(expected_outcome, outcome) {
  if (outcome == FALSE) {
    p_failure = 1 - expected_outcome
    surprise = -log(p_failure)
  } else {
    p_success = expected_outcome
    surprise = -log(p_success)
  }
  return(surprise)
}

# affect is positive or negative based on success or failure, 
# and proportional to information-theoretic surprise 
affect <- function(expected_outcome, outcome) {  
  sign = ifelse(outcome, 1, -1)
  return (round ( ( sign*quantify_surprise(expected_outcome, outcome)), 2) )
}

# Deciding if to request help 
# based on pr_adult and baby succeeding
# Additionally, scale the probability by a coefficient k_helpseeking_willingness
request_help = function(adult_skill, baby_skill, helpseeking_willingness) {
  pr_adult = round(expectation(adult_skill),2)
  pr_self = round(expectation(baby_skill),2)
  
  diff = max(pr_adult - pr_self, 0)
  return (diff*helpseeking_willingness)
}



# outcome - TRUE or FALSE - outcome of an attempt;
# skill - adult or baby skill 
# result: update Beta distribution over skill after observing outcome
# pseudocount ensures that we never think the task is impossible, or certain
update_skill <- function(outcome, skill, evidence_weight = 1) {
  a <- skill[[1]] + ifelse(outcome, 1*evidence_weight, pseudocount)
  b <- skill[[2]] + ifelse(outcome, pseudocount, 1*evidence_weight)
  return( list(a = a, b = b) )
}



# showing difficulty distribution as density and as a histogram
plot_difficulty <-function(difficulty, beta_distribution, name = "Adult") {
  difficulty_density <- dbeta(difficulty, beta_distribution$a, beta_distribution$b)
  t = paste("Inferred difficulty, ", name)
  g = ggplot(data.frame(difficulty, difficulty_density), aes(x = difficulty, y = difficulty_density)) +
    geom_line() +
    ggtitle(t) +
    xlab("Probability of success in one attempt") +
    ylab("Probability density") +
    theme_minimal()
  return(g)
}


