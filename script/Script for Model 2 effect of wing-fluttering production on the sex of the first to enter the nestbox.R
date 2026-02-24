# Load the required library
library(brms)

### Open the data file
tab.obs2 = read.csv("./for github repository/data_files/dataset Model 2 sex indiv enter nest first.csv", header = T)


#releveling the factor
tab.obs2$sex.gesture = relevel(as.factor(tab.obs2$sex.gesture), ref = "none")

#getting default priors from brms
mprior = get_prior(first.feeder.num ~ sex.gesture + first.arriver +
                     (1+sex.gesture.num+first.arriver.num|id.male), 
                   data = tab.obs2, family = "bernoulli")

#adjusting priors
mprior$prior[2:3] <- "normal(0,1)"


make_stancode(first.feeder.num ~ sex.gesture + first.arriver +
                (1+sex.gesture.num+first.arriver.num|id.male), 
              data = tab.obs2, family = "bernoulli", prior = mprior)



m1.feed = brm(first.feeder.num ~ sex.gesture + first.arriver +
                (1+sex.gesture.num+first.arriver.num|id.male), 
              data = tab.obs2, family = "bernoulli", prior = mprior,
              chains = 4, cores = 4, iter = 4000, warmup = 2000, 
              control = list(adapt_delta = 0.97))


#posterior predictive check
pp_check(m1.feed, ndraws= 1000, type= "bars")


#Estimates of the model with 89% CI
summary(m1.feed, prob = 0.89)

# Estimates of the model with 95% CI
summary(m1.feed, prob = 0.95) 


#get conditional effects for the plot
ce = conditional_effects(m1.feed, effects = "sex.gesture")

ce = data.frame(ce["sex.gesture"])


### compile the posterior distribution

post.m1.feed = posterior_samples(m1.feed)

post.m1.feed = post.m1.feed[,c("b_sex.gesturefemale","b_first.arrivermale")]

mean(post.m1.feed[,1])


# support for positive estimate
round(sum(post.m1.feed[,"b_sex.gesturefemale"] > 0.00)/
        length(post.m1.feed[,"b_sex.gesturefemale"]),3)  #0.685  


round(sum(post.m1.feed[,"b_first.arrivermale"] > 0.00)/
        length(post.m1.feed[,"b_first.arrivermale"]),3)  #0.504  


