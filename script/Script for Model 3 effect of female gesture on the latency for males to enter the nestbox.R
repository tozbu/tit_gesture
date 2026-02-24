# Load the required library
library(brms)

### Open the data file
dl = read.csv("./for github repository/data_files/dataset Model 3 latency to enter nest.csv", header = T)


#getting default priors from brms
mprior = get_prior(time ~ gesture_mate + arrival_order +
                     (1+gesture_mate.num+arrival_order.num|nest), 
                   data = dl, family = "weibull")

#customise the priors
mprior$prior[2:3] <- "normal(0,1)"


make_stancode(time ~ gesture_mate + arrival_order +
                (1+gesture_mate.num+arrival_order.num|nest), 
              data = dl, family = "weibull", prior = mprior)


#run the model
m3.lat = brm(time ~ gesture_mate + arrival_order +
               (1+gesture_mate.num+arrival_order.num|nest), 
             data = dl, family = "weibull", prior = mprior,
             chains = 4, cores = 4, iter = 4000, warmup = 2000, 
             control = list(adapt_delta = 0.97))


#Estimates of the model and 89% CI
summary(m3.lat, prob = 0.89) #

#Estimates of the model and 95% CI
summary(m3.lat, prob = 0.95) #


#extracting predicted values from conditional effect function

ce3 = conditional_effects(m3.lat, effects = "gesture_mate")

ce3 = data.frame(ce3["gesture_mate"])


### compile the posterior distribution

post.m3.lat = posterior_samples(m3.lat)

post.m3.lat[,c("b_gesture_mate")]




# support for positive estimate
round(sum(post.m3.lat[,c("b_gesture_mate")] < 0.00)/
        length(post.m3.lat[,c("b_gesture_mate")]),3)  #0.961  


# support for positive estimate
round(sum(post.m3.lat[,c("b_arrival_order")] > 0.00)/
        length(post.m3.lat[,c("b_arrival_order")]),3)  #0.564 



### Create aggregate table to compile the data points to plot
t.lat = aggregate(dl$time, by=list(dl$gesture_mate, dl$ID), mean)

colnames(t.lat) = c("gesture_mate", "ID", "mean.lat")

xx = aggregate(dl$time, by=list(dl$gesture_mate, dl$ID), length)
t.lat$N=xx$x

### create t.lat only for males that appear in both conditions

t.lat2 = t.lat

xx = data.frame(table(t.lat$ID))

xx$Var1[which(xx$Freq==2)]

t.lat2=t.lat2[t.lat2$ID%in%c(xx$Var1[which(xx$Freq==2)]),]


### Plot


xx = c(1,2)
jitter.fac = 10
alpha = 0.4
cex = 1.5
ylim = c(0,60)
xlim = c(0.5,2.5)
col= c('orange','turquoise')

windows(7,7)

par (oma = c(2,2,0.5,0.5))

### general boxplot

boxplot(t.lat$mean.lat ~ t.lat$gesture_mate, xlab="", ylab= "",
        col="white", outline=F, whisklty = 0, staplelty = 0, 
        ylim = ylim, xlim = xlim, axes=F, at =xx, lwd = 2, border = col)

### no female gesture
par(new = T)

plot(x = rep(xx[1],length(t.lat$mean.lat[t.lat$gesture_mate==0])),
     y= t.lat$mean.lat[t.lat$gesture_mate==0], 
     xlim = xlim, axes = F, pch = 16, col = adjustcolor(col[1], alpha = alpha),
     ylab ='', xlab ='', cex = sqrt(t.lat$N[t.lat$gesture_mate==0]), ylim = ylim)


### no female gesture
par(new = T)

plot(x = rep(xx[2],length(t.lat$mean.lat[t.lat$gesture_mate==1])),
     y= t.lat$mean.lat[t.lat$gesture_mate==1], 
     xlim = xlim, axes = F, pch = 16, col = adjustcolor(col[2], alpha = alpha),
     ylab ='', xlab ='', cex = sqrt(t.lat$N[t.lat$gesture_mate==1]), ylim = ylim)


### add the model lines and 95% CI
sqt=0.25



lines(x=c(1-sqt,1+sqt), 
      y= c(ce3$gesture_mate.estimate__[which(ce3$gesture_mate.gesture_mate==0)], 
           ce3$gesture_mate.estimate__[which(ce3$gesture_mate.gesture_mate==0)]), 
      col="black", lwd=5)


lines(x=c(2-sqt,2+sqt), 
      y= c(ce3$gesture_mate.estimate__[which(ce3$gesture_mate.gesture_mate==1)], 
           ce3$gesture_mate.estimate__[which(ce3$gesture_mate.gesture_mate==1)]), 
      col="black", lwd=5)


## add the CI

arrows(x0 = 1, x1 = 1, y0 =ce3$gesture_mate.lower__[which(ce3$gesture_mate.gesture_mate==0)], 
       y1 = ce3$gesture_mate.upper__[which(ce3$gesture_mate.gesture_mate==0)],
       angle=90, length=0.1, lwd=2, col ='black')

arrows(x0 = 1, x1 = 1, y0 =ce3$gesture_mate.upper__[which(ce3$gesture_mate.gesture_mate==0)], 
       y1 = ce3$gesture_mate.lower__[which(ce3$gesture_mate.gesture_mate==0)],
       angle=90, length=0.1, lwd=2, col ='black')


arrows(x0 = 2, x1 = 2, y0 =ce3$gesture_mate.lower__[which(ce3$gesture_mate.gesture_mate==1)], 
       y1 = ce3$gesture_mate.upper__[which(ce3$gesture_mate.gesture_mate==1)],
       angle=90, length=0.1, lwd=2, col ='black')

arrows(x0 = 2, x1 = 2, y0 =ce3$gesture_mate.upper__[which(ce3$gesture_mate.gesture_mate==1)], 
       y1 = ce3$gesture_mate.lower__[which(ce3$gesture_mate.gesture_mate==1)],
       angle=90, length=0.1, lwd=2, col ='black')


axis(2, las = 1)

axis(1, labels = c('',''), at = c(1,2))


mtext(c("No", 'Yes'), at =c(1,2), side =1, cex = 1.2, line =1)
mtext("Female gesture", line = 3, cex =1.4, side =1)
mtext('Latency for males to enter the nestbox (sec.)', side =2, line =4, cex =1.3)

legend(x=1.2, y=63, legend = c("No Female gesture", "Female gesture"), 
       xjust = -0.5,
       pch = c(16,16), col = adjustcolor(col, alpha = alpha), 
       pt.cex = 2, 
       x.intersp = 1.2,
       cex =1.1, bty="n")


legend(x = 1.7, y=55, legend = c(" 1", " 5", " 13"), 
       pch = c(16,16,16), col = "black", 
       pt.cex = c(sqrt(c(1,5,13))), 
       y.intersp = c(1,rep(1.15,3)),
       x.intersp = 1.2, title = "N observations:",
       cex =1, bty="n")

box()

#add lines between individuals in both conditions

yy1 = t.lat2$mean.lat[t.lat2$gesture_mate==0] #create vector of latency when there was no female gestures
yy2 = t.lat2$mean.lat[t.lat2$gesture_mate==1] #create vector of latency when there was female gestures

segments(x0 = rep(1, length(yy1)), x1=rep(2, length(yy2)), y0 =yy1, y1=yy2, col="grey")





