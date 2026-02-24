
# Load the required library
library(brms)

### Open the data file
dg = read.csv("./for github repository/data_files/dataset Model 1 gesture and mate presence.csv", header = T)

#get default prior from BRMS
mprior = get_prior(gesture ~ sex*mate_presence +
                     (1+mate_presence.num|ID)+
                     (1+mate_presence.num+sex.num|nest),
                   data = dg, family = "bernoulli")

#adjust the prior
mprior$prior[2:4] <- "normal(0,1)"


make_stancode(gesture ~ sex*mate_presence +
                (1+mate_presence.num|ID)+
                (1+mate_presence.num+sex.num|nest),
              data = dg, family = "bernoulli", prior = mprior)



m2.gesture = brm(gesture ~ sex*mate_presence +
                   (1+mate_presence.num|ID)+
                   (1+mate_presence.num+sex.num|nest),
                 data = dg, family = "bernoulli", prior = mprior,
                 chains = 4, cores = 4, iter = 4000, warmup = 2000, 
                 control = list(adapt_delta = 0.97))


#Posterior predictive check
pp_check(m2.gesture, ndraws= 1000, type= "bars")


#Summary of the model with 89% CI
summary(m2.gesture, prob = 0.89) 

#summary of the model with 95% CI
summary(m2.gesture, prob = 0.95)


### getting the conditional effects for plotting
ce2 = conditional_effects(m2.gesture)

ce2 = data.frame(ce2["sex:mate_presence"])


### compile the posterior distribution

post.m2.gesture = posterior_samples(m2.gesture)

post.m2.gesture = post.m2.gesture[,c("b_sexmale", "b_mate_presenceyes",
                                     "b_sexmale:mate_presenceyes")]

# support for positive estimate
round(sum(post.m2.gesture[,"b_sexmale"] < 0.00)/
        length(post.m2.gesture[,"b_sexmale"]),3)  #0.997 99.7% posterior support for males gesturing less than females 


round(sum(post.m2.gesture[,"b_mate_presenceyes"] > 0.00)/
        length(post.m2.gesture[,"b_mate_presenceyes"]),3) #0.998  99.8% posterior support for gesture to be more frequent in mate presence


round(sum(post.m2.gesture[,"b_sexmale:mate_presenceyes"] < 0.00)/
        length(post.m2.gesture[,"b_sexmale:mate_presenceyes"]),3) #0.844 low support for the interaction sex * mate_presence. 




#### Plotting model effect of sex and mate presence ####


### create an aggregate table of the datapoints to plot 

t.gesture = aggregate(dg$gesture, by = list(dg$ID, dg$mate_presence, dg$sex), mean)
colnames(t.gesture) = c("ID", "mate_presence", "sex", "p.gesture")

#plug in the sample size

xx= aggregate(dg$gesture, by = list(dg$ID, dg$mate_presence, dg$sex), length)

t.gesture$N = xx$x  

t.gesture$sex.mate.combi = paste(t.gesture$sex, t.gesture$mate_presence, sep = "_")


### Sample size for the model

nrow(dg) #783
length(unique(dg$nest)) #15 nests
length(unique(dg$ID)) #29 birds 
length(unique(dg$ID[dg$sex=="male"])) #15 males

length(unique(dg$obs)) #567 independent observations




#### The plot

xx = c(1,2,4,5)
jitter.fac = 10
alpha = 0.4
cex = 1.5
ylim = c(0,1)
xlim = c(0.5,5.5)
col= c('orange','turquoise')

windows(10,7)

par (oma = c(2,2,0.5,0.5))

### general boxplot

boxplot(t.gesture$p.gesture ~ t.gesture$sex.mate.combi, xlab="", ylab= "",
        col="white", outline=F, whisklty = 0, staplelty = 0, 
        ylim = ylim, xlim = xlim, axes=F, at =xx, lwd = 2, border = col)

### female mate absent
par(new = T)

plot(x = jitter(rep(xx[1],length(t.gesture$p.gesture[t.gesture$sex=="female"&t.gesture$mate_presence=="no"])),
                factor = jitter.fac), 
     y= t.gesture$p.gesture[t.gesture$sex=="female"&t.gesture$mate_presence=="no"], 
     xlim = xlim, axes = F, pch = 16, col = adjustcolor(col[1], alpha = alpha),
     ylab ='', xlab ='', cex = sqrt(t.gesture$N[t.gesture$sex=="female"&t.gesture$mate_presence=="no"])/1.5, ylim = ylim)


### female mate present
par(new = T)

plot(x = jitter(rep(xx[2],length(t.gesture$p.gesture[t.gesture$sex=="female"&t.gesture$mate_presence=="yes"])),
                factor = jitter.fac/2), 
     y= t.gesture$p.gesture[t.gesture$sex=="female"&t.gesture$mate_presence=="yes"], 
     xlim = xlim, axes = F, pch = 16, col = adjustcolor(col[2], alpha = alpha),
     ylab ='', xlab ='', cex = sqrt(t.gesture$N[t.gesture$sex=="female"&t.gesture$mate_presence=="yes"])/1.5, ylim = ylim)


### male mate absent
par(new = T)

plot(x = jitter(rep(xx[3],length(t.gesture$p.gesture[t.gesture$sex=="male"&t.gesture$mate_presence=="no"])),
                factor = jitter.fac/4), 
     y= t.gesture$p.gesture[t.gesture$sex=="male"&t.gesture$mate_presence=="no"], 
     xlim = xlim, axes = F, pch = 16, col = adjustcolor(col[1], alpha = alpha),
     ylab ='', xlab ='', cex = sqrt(t.gesture$N[t.gesture$sex=="male"&t.gesture$mate_presence=="no"])/1.5, ylim = ylim)


### male mate present
par(new = T)

plot(x = jitter(rep(xx[4],length(t.gesture$p.gesture[t.gesture$sex=="male"&t.gesture$mate_presence=="yes"])),
                factor = jitter.fac/5), 
     y= t.gesture$p.gesture[t.gesture$sex=="male"&t.gesture$mate_presence=="yes"], 
     xlim = xlim, axes = F, pch = 16, col = adjustcolor(col[2], alpha = alpha),
     ylab ='', xlab ='', cex = sqrt(t.gesture$N[t.gesture$sex=="male"&t.gesture$mate_presence=="yes"])/1.5, ylim = ylim)



### add the model lines and 95% CI
sqt=0.25



lines(x=c(1-sqt,1+sqt), y= c(ce2$sex.mate_presence.estimate__[which(ce2$sex.mate_presence.sex=="female"&
                                                                      ce2$sex.mate_presence.mate_presence=="no")], 
                             ce2$sex.mate_presence.estimate__[which(ce2$sex.mate_presence.sex=="female"&
                                                                      ce2$sex.mate_presence.mate_presence=="no")]), 
      col="black", lwd=5)


lines(x=c(2-sqt,2+sqt), y= c(ce2$sex.mate_presence.estimate__[which(ce2$sex.mate_presence.sex=="female"&
                                                                      ce2$sex.mate_presence.mate_presence=="yes")], 
                             ce2$sex.mate_presence.estimate__[which(ce2$sex.mate_presence.sex=="female"&
                                                                      ce2$sex.mate_presence.mate_presence=="yes")]), 
      col="black", lwd=5)



lines(x=c(4-sqt,4+sqt), y= c(ce2$sex.mate_presence.estimate__[which(ce2$sex.mate_presence.sex=="male"&
                                                                      ce2$sex.mate_presence.mate_presence=="no")], 
                             ce2$sex.mate_presence.estimate__[which(ce2$sex.mate_presence.sex=="male"&
                                                                      ce2$sex.mate_presence.mate_presence=="no")]), 
      col="black", lwd=5)


lines(x=c(5-sqt,5+sqt), y= c(ce2$sex.mate_presence.estimate__[which(ce2$sex.mate_presence.sex=="male"&
                                                                      ce2$sex.mate_presence.mate_presence=="yes")], 
                             ce2$sex.mate_presence.estimate__[which(ce2$sex.mate_presence.sex=="male"&
                                                                      ce2$sex.mate_presence.mate_presence=="yes")]), 
      col="black", lwd=5)

## add the CI

arrows(x0 = 1, x1 = 1, y0 =ce2$sex.mate_presence.lower__[which(ce2$sex.mate_presence.sex=="female"&
                                                                 ce2$sex.mate_presence.mate_presence=="no")], 
       y1 = ce2$sex.mate_presence.upper__[which(ce2$sex.mate_presence.sex=="female"&
                                                  ce2$sex.mate_presence.mate_presence=="no")],
       angle=90, length=0.1, lwd=2, col ='black')

arrows(x0 = 1, x1 = 1, y0 =ce2$sex.mate_presence.upper__[which(ce2$sex.mate_presence.sex=="female"&
                                                                 ce2$sex.mate_presence.mate_presence=="no")], 
       y1 = ce2$sex.mate_presence.lower__[which(ce2$sex.mate_presence.sex=="female"&
                                                  ce2$sex.mate_presence.mate_presence=="no")],
       angle=90, length=0.1, lwd=2, col ='black')


arrows(x0 = 2, x1 = 2, y0 =ce2$sex.mate_presence.lower__[which(ce2$sex.mate_presence.sex=="female"&
                                                                 ce2$sex.mate_presence.mate_presence=="yes")], 
       y1 = ce2$sex.mate_presence.upper__[which(ce2$sex.mate_presence.sex=="female"&
                                                  ce2$sex.mate_presence.mate_presence=="yes")],
       angle=90, length=0.1, lwd=2, col ='black')

arrows(x0 = 2, x1 = 2, y0 =ce2$sex.mate_presence.upper__[which(ce2$sex.mate_presence.sex=="female"&
                                                                 ce2$sex.mate_presence.mate_presence=="yes")], 
       y1 = ce2$sex.mate_presence.lower__[which(ce2$sex.mate_presence.sex=="female"&
                                                  ce2$sex.mate_presence.mate_presence=="yes")],
       angle=90, length=0.1, lwd=2, col ='black')


arrows(x0 = 4, x1 = 4, y0 =ce2$sex.mate_presence.lower__[which(ce2$sex.mate_presence.sex=="male"&
                                                                 ce2$sex.mate_presence.mate_presence=="no")], 
       y1 = ce2$sex.mate_presence.upper__[which(ce2$sex.mate_presence.sex=="male"&
                                                  ce2$sex.mate_presence.mate_presence=="no")],
       angle=90, length=0.1, lwd=2, col ='black')

arrows(x0 = 4, x1 = 4, y0 =ce2$sex.mate_presence.upper__[which(ce2$sex.mate_presence.sex=="male"&
                                                                 ce2$sex.mate_presence.mate_presence=="no")], 
       y1 = ce2$sex.mate_presence.lower__[which(ce2$sex.mate_presence.sex=="male"&
                                                  ce2$sex.mate_presence.mate_presence=="no")],
       angle=90, length=0.1, lwd=2, col ='black')


arrows(x0 = 5, x1 = 5, y0 =ce2$sex.mate_presence.lower__[which(ce2$sex.mate_presence.sex=="male"&
                                                                 ce2$sex.mate_presence.mate_presence=="no")], 
       y1 = ce2$sex.mate_presence.upper__[which(ce2$sex.mate_presence.sex=="male"&
                                                  ce2$sex.mate_presence.mate_presence=="no")],
       angle=90, length=0.1, lwd=2, col ='black')

arrows(x0 = 5, x1 = 5, y0 =ce2$sex.mate_presence.upper__[which(ce2$sex.mate_presence.sex=="male"&
                                                                 ce2$sex.mate_presence.mate_presence=="no")], 
       y1 = ce2$sex.mate_presence.lower__[which(ce2$sex.mate_presence.sex=="male"&
                                                  ce2$sex.mate_presence.mate_presence=="no")],
       angle=90, length=0.1, lwd=2, col ='black')


axis(2, las = 1)

axis(1, labels = c('',''), at = c(1.5,4.5))


mtext(c("Female", 'Male'), at =c(1.5,4.5), side =1, cex = 1.2, line =1)
mtext("Sex", line = 3, cex =1.4, side =1)
mtext('Proportion of gesture', side =2, line =4, cex =1.3)

legend(x=2.5, y=1.02, legend = c("Mate absent", "Mate present"), 
       xjust = -0.5,
       pch = c(16,16), col = adjustcolor(col, alpha = alpha), 
       pt.cex = 3, 
       y.intersp = 1.2,
       x.intersp = 1.2,
       cex =1.15, bty="n")


legend(x = 4.5, y=1.02, legend = c(" 1", " 5", " 15", " 40"), 
       pch = c(16,16,16,16), col = "black", 
       pt.cex = c(sqrt(c(1,5,15,40))/1.5), 
       y.intersp = c(1,rep(1.35,3)),
       x.intersp = 1.2, title = "N observations:",
       cex =1.1, bty="n")

box()

