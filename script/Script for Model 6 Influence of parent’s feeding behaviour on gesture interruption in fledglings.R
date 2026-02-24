# Load the required library
library(brms)

### Open the data file
dw3 = read.csv("./for github repository/data_files/dataset Model 6 stop gesture when fed.csv", header = T)


#get the brms default prior
mprior2 = get_prior(ceased_wing.fluttering ~ 1 +
                      (1|ID),
                    data = dw3, family = "bernoulli")


make_stancode(ceased_wing.fluttering ~ 1 +
                (1|ID),
              data = dw3, family = "bernoulli", prior = mprior2)



m3b.stop = brm(ceased_wing.fluttering ~ 1 +
                 (1|ID),
               data = dw3, family = "bernoulli", prior = mprior2,
               chains = 4, cores = 4, iter = 4000, warmup = 2000, 
               control = list(adapt_delta = 0.99))


#posterior predictive check. 
pp_check(m3b.stop, ndraws= 1000, type= "bars")


#Estimates of the model an 95% CI
summary(m3b.stop, prob = 0.95) #

#Estimates of the model an 89% CI
summary(m3b.stop, prob = 0.89) #

#compile the posterior distribution
post.m3b.stop = posterior_samples(m3b.stop)

#create inverse logit function to recompose the original proportions in linear space
inv_logit <- function(x) {
  1 / (1 + exp(-x))
}


#transform the posterior predictions in linear space
posterior_prob_intercept = inv_logit(post.m3b.stop[,"b_Intercept"])

mean(posterior_prob_intercept) #0.8727934

quantile(posterior_prob_intercept, probs = c(0.025, 0.975))
# 2.5%     97.5% 
# 0.6994771 0.9783749 

length(which(post.m3b.stop[,"b_Intercept"]>0))/length(post.m3b.stop[,"b_Intercept"]) #0.99925
length(which(posterior_prob_intercept>0.5))/length(posterior_prob_intercept) #0.99925


# 99.923 % posterior support for a positive intercept 
#(for wing fluttering stopping more when parents feed them)
