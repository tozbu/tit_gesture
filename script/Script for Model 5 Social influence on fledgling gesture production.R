# Load the required library
library(brms)

### Open the data file
dw2 = read.csv("./for github repository/data_files/dataset Model 5 gesture and mparent presence.csv", header = T)


#get default brms priors
mprior = get_prior(gesture ~ mate.parent_presence +
                     (1+mate.parent_presence.num|ID),
                   data = dw2, family = "bernoulli")


make_stancode(gesture ~ mate.parent_presence +
                (1+mate.parent_presence.num|ID),
              data = dw2, family = "bernoulli", prior = mprior)


#run the model
mw2.gesture = brm(gesture ~ mate.parent_presence +
                    (1+mate.parent_presence.num|ID),
                  data = dw2, family = "bernoulli",
                  prior = mprior,
                  chains = 8, cores = 4, iter = 4000, warmup = 2000, 
                  control = list(adapt_delta = 0.99))


#posterior predictive check
pp_check(mw2.gesture, ndraws= 1000, type= "bars")


#Estimates of the model an 89% CI
summary(mw2.gesture, prob = 0.89) #


#Estimates of the model an 95% CI
summary(mw2.gesture, prob = 0.95)


#extract conditional effects etsimates
ce2 = conditional_effects(mw2.gesture)

ce2 = data.frame(ce2["mate.parent_presence"])


### compile the posterior distribution

post.mw2.gesture = posterior_samples(mw2.gesture)

post.mw2.gesture = post.mw2.gesture[,c("b_mate.parent_presence")]

# support for positive estimate
round(sum(length(post.mw2.gesture < 0.00)/
            length(post.mw2.gesture)),3)  # 100% posterior support for fledgling gesture more in parents presence than when they are absent.  

#### Plot model effect of parent presence ####

### create aggregate table to compile the datapoints to plot

tw.gesture = aggregate(dw2$gesture, by = list(dw2$ID, dw2$mate.parent_presence), mean)

colnames(tw.gesture) = c("ID", "mate.parent_presence", "p.gesture")

#plug in the sample size

xx= aggregate(dw2$gesture, by = list(dw2$ID, dw2$mate.parent_presence), length)

tw.gesture$N = xx$x  
aggregate(tw.gesture$p.gesture, by =list(tw.gesture$mate.parent_presence), mean)
# Group.1  x
# 1       0 0.000000
# 2       1 0.423913

### Sample size for the model

nrow(dw2) #99

length(unique(dw2$ID)) #24 fledglings (and independent obs)


#### The plot

xx = c(1,2)
jitter.fac = 10
alpha = 0.4
cex = 1.5
ylim = c(0,1)
xlim = c(0.5,2.5)
col= c('orange','turquoise')

windows(10,7)

par (oma = c(2,2,0.5,0.5))

### general boxplot

boxplot(tw.gesture$p.gesture ~ tw.gesture$mate.parent_presence, xlab="", ylab= "",
        col="white", outline=F, whisklty = 0, staplelty = 0, 
        ylim = ylim, xlim = xlim, axes=F, at =xx, lwd = 2, border = "white")

### parent absent
par(new = T)

plot(x = jitter(rep(xx[1],length(tw.gesture$p.gesture[tw.gesture$mate.parent_presence==0])),
                factor = jitter.fac), 
     y= tw.gesture$p.gesture[tw.gesture$mate.parent_presence==0], 
     xlim = xlim, axes = F, pch = 16, col = adjustcolor(col[1], alpha = alpha),
     ylab ='', xlab ='', cex = sqrt(tw.gesture$N[tw.gesture$mate.parent_presence==0])*2, ylim = ylim)


### parent present
par(new = T)

plot(x = jitter(rep(xx[2],length(tw.gesture$p.gesture[tw.gesture$mate.parent_presence==1])),
                factor = jitter.fac/2), 
     y= tw.gesture$p.gesture[tw.gesture$mate.parent_presence==1], 
     xlim = xlim, axes = F, pch = 16, col = adjustcolor(col[2], alpha = alpha),
     ylab ='', xlab ='', cex = sqrt(tw.gesture$N[tw.gesture$mate.parent_presence==1])*2, ylim = ylim)

### add the model lines and 95% CI
sqt=0.15



lines(x=c(1-sqt,1+sqt), y= c(ce2$mate.parent_presence.estimate__[which(ce2$mate.parent_presence.effect1__==0)], 
                             ce2$mate.parent_presence.estimate__[which(ce2$mate.parent_presence.effect1__==0)]), 
      col="black", lwd=5)


lines(x=c(2-sqt,2+sqt), y= c(ce2$mate.parent_presence.estimate__[which(ce2$mate.parent_presence.effect1__==1)], 
                             ce2$mate.parent_presence.estimate__[which(ce2$mate.parent_presence.effect1__==1)]), 
      col="black", lwd=5)


## add the CI

arrows(x0 = 1, x1 = 1, y0 = ce2$mate.parent_presence.lower__[which(ce2$mate.parent_presence.effect1__==0)], 
       y1 = ce2$mate.parent_presence.upper__[which(ce2$mate.parent_presence.effect1__==0)],
       angle=90, length=0.1, lwd=2, col ='black')

arrows(x0 = 1, x1 = 1, y0 = ce2$mate.parent_presence.upper__[which(ce2$mate.parent_presence.effect1__==0)], 
       y1 = ce2$mate.parent_presence.lower__[which(ce2$mate.parent_presence.effect1__==0)],
       angle=90, length=0.1, lwd=2, col ='black')



arrows(x0 = 2, x1 = 2, y0 = ce2$mate.parent_presence.lower__[which(ce2$mate.parent_presence.effect1__==1)], 
       y1 = ce2$mate.parent_presence.upper__[which(ce2$mate.parent_presence.effect1__==1)],
       angle=90, length=0.1, lwd=2, col ='black')

arrows(x0 = 2, x1 = 2, y0 = ce2$mate.parent_presence.upper__[which(ce2$mate.parent_presence.effect1__==1)], 
       y1 = ce2$mate.parent_presence.lower__[which(ce2$mate.parent_presence.effect1__==1)],
       angle=90, length=0.1, lwd=2, col ='black')


axis(2, las = 1)

axis(1, labels = c('',''), at = c(1,2))


mtext(c("No", 'Yes'), at =c(1,2), side =1, cex = 1.2, line =1)
mtext("Parent Presence", line = 3, cex =1.4, side =1)
mtext('Proportion of gesture', side =2, line =4, cex =1.3)

legend(x=0.5, y=1.02, legend = c("Parent absent", "Parent present"), 
       xjust = -0.5,
       pch = c(16,16), col = adjustcolor(col, alpha = alpha), 
       pt.cex = 3, 
       y.intersp = 1.2,
       x.intersp = 1.2,
       cex =1.15, bty="n")


legend(x = 0.75, y= .85, legend = c(" 1", " 3", " 9"), 
       pch = c(16,16,16,16), col = "black", 
       pt.cex = c(sqrt(c(1,3,9))*2), 
       y.intersp = c(1,rep(1.55,3)),
       x.intersp = 1.2, title = "N observations:",
       cex =1.1, bty="n")

box()
