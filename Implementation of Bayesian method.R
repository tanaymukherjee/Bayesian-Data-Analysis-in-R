# Author: Tanay Mukehrjee
# Date: Jan 6th, 2019


#----
# Question 1

# Number of random draws from the prior
n_draws <- 10000

prior <- 5 # Here you sample n_draws draws from the prior  
hist(prior) # It's always good to eyeball the prior to make sure it looks ok.

# Here you define the generative model
generative_model <- function(parameters) {
  ...
}

# Here you simulate data using the parameters from the prior and the 
# generative model
sim_data <- rep(NA, n_draws)
for(i in 1:n_draws) {
  sim_data[i] <- generative_model(prior[i])
}

# Here you filter off all draws that do not match the data.
posterior <- prior[sim_data == observed_data] 

hist(posterior) # Eyeball the posterior
length(posterior) # See that we got enought draws left after the filtering.
# There are no rules here, but you probably want to aim
# for >1000 draws.

# Now you can summarize the posterior, where a common summary is to take the mean
# or the median posterior, and perhaps a 95% quantile interval.
median(posterior)
quantile(posterior, c(0.025, 0.975))

#----
# Alternate solution to que 1
n_draw <- 10000

# Defining and drawing from the prior distribution
prior_rate <- runif(n_draw, 0, 1)

# Defining the generative model
gen_model <- function(rate) {
  subscribers <- rbinom(1, size = 16, prob = rate)
  subscribers
}

# Simulating the data
subscribers <- rep(NA, n_draw)
for(i in 1:n_draw) {
  subscribers[i] <- gen_model(prior_rate[i])
}

# Filtering out those parameter values that didn't result in the
# data that we actually observed
post_rate <- prior_rate[subscribers == 6]

# Checking that there enough samples left
length(post_rate)

hist(post_rate, xlim = c(0, 1))

mean(post_rate)

quantile(post_rate, c(0.025, 0.975))


#----
# Question 2

sum(post_rate > 0.2) / length(post_rate)


#----
# Question 3

# This can be done with a for loop
singnups <- rep(NA, length(post_rate))
for(i in 1:length(post_rate)) {
  singnups[i] <- rbinom(n = 1, size = 100, prob = post_rate[i])
}

# But since rbinom is vectorized we can simply write it like this:
signups <- rbinom(n = length(post_rate), size = 100, prob = post_rate)

hist(signups, xlim = c(0, 100))

quantile(signups, c(0.025, 0.975))

