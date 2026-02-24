# Load the required library
library(brms)

### Open the data file
dl = read.csv("./for github repository/data_files/dataset Model 4 stop gesturing.csv", header = T)

#get default priors from brms
mprior4 = get_prior(ceased_wing.fluttering ~ 1 +
                      (1|ID),
                    data = d4, family = "bernoulli")


make_stancode(ceased_wing.fluttering ~ 1 +
                (1|ID),
              data = d4, family = "bernoulli", prior = mprior4)


#run the model
m4.stop = brm(ceased_wing.fluttering ~ 1 +
                (1|ID),
              data = d4, family = "bernoulli", prior = mprior4,
              chains = 4, cores = 4, iter = 4000, warmup = 2000, 
              control = list(adapt_delta = 0.99))


#posterior predictive check
pp_check(m4.stop, ndraws= 1000, type= "bars")


#Estimates of the model with 95% CI
summary(m4.stop, prob = 0.95) #

#Estimates of the model with 89% CI
summary(m4.stop, prob = 0.89) #


#compiling the posterior distribution
post.m4.stop = posterior_samples(m4.stop)

#create inverse logit function to recompose the original proportions in linear space
inv_logit <- function(x) {
  1 / (1 + exp(-x))
}

#transform the posterior predictions in linear space
posterior_prob_intercept = inv_logit(post.m4.stop[,"b_Intercept"])

mean(posterior_prob_intercept) #0.8404254

quantile(posterior_prob_intercept, probs = c(0.025, 0.975))
# 2.5%     97.5% 
#   0.6386691 0.9760914  


# calculate % support for the estimates
length(which( post.m4.stop[,"b_Intercept"]>0))/length(post.m4.stop[,"b_Intercept"]) # 0.994
length(which(posterior_prob_intercept>0.5))/length(posterior_prob_intercept) # 0.994


# 99.4 % posterior support for a positive intercept 
#(for wing fluttering stopping more when partner entered the nest)
