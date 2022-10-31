
#It should be mentioned that I have been using R notebook to create the project.
#This means that all the R code is already in the pdf file, so this file is just
#to make sure you don't feel cheated in not getting an R file. It would probably
#make more sense to give you the Rmd file instead. Please ask for it if you
#want it. 

setwd("/Users/dwiinberg/Desktop/Project 1")

D = read.table("finans1_data.csv", header=TRUE, sep=";", as.is=TRUE)
D = D[ ,c("t","AGG","VAW","IWN","SPY")]

head(D)

##############################################################################
hist(D$AGG, xlab="Return (AGG)", prob=TRUE, breaks = 50, col="blue")


##############################################################################
#Setting the t-variable as a date variable.
D$t = as.Date(x=D$t, format="%Y-%m-%d")

ylim = c(-0.2,0.2)
plot(D$t, D$AGG, type="l", ylim=ylim, xlab="Date", ylab="Return AGG", col="blue")
plot(D$t, D$VAW, type="l", ylim=ylim, xlab="Date", ylab="Return VAW", col="red")
plot(D$t, D$IWN, type="l", ylim=ylim, xlab="Date", ylab="Return IWN", col="green")
plot(D$t, D$SPY, type="l", ylim=ylim, xlab="Date", ylab="Return SPY", col="orange")

boxplot(D$AGG, D$VAW, D$IWN, D$SPY, names=c("AGG", "VAW", "IWN", "SPY"),
        xlab="ETF", ylab="Return", col="red")

##############################################################################
#Calculation number of observations, sample mean and variance for the four ETF's. 
sum(!is.na(D$AGG))
var(D$AGG, na.rm=TRUE)
sd(D$AGG, na.rm=TRUE)
summary(D$AGG, na.rm=TRUE)

sum(!is.na(D$VAW))
var(D$VAW, na.rm=TRUE)
sd(D$VAW, na.rm=TRUE)
summary(D$VAW, na.rm=TRUE)

sum(!is.na(D$IWN))
var(D$IWN, na.rm=TRUE)
sd(D$IWN, na.rm=TRUE)
summary(D$IWN, na.rm=TRUE)

sum(!is.na(D$SPY))
var(D$SPY, na.rm=TRUE)
sd(D$SPY, na.rm=TRUE)
summary(D$SPY, na.rm=TRUE)


##############################################################################
qqnorm(D$AGG, main="AGG", col="blue")
qqline(D$AGG, col="blue")

qqnorm(D$VAW, main="VAW", col="red")
qqline(D$VAW, col="red")

qqnorm(D$IWN, main="IWN", col="green")
qqline(D$IWN, col="green")

qqnorm(D$SPY, main="SPY", col="orange")
qqline(D$SPY, col="orange")


##############################################################################
#VAW: expected weekly return = 0.1794%, standard deviation = 3.608%
mean_vaw = 0.001794
sd_vaw = 0.03608

hist(D$VAW, xlab="Return (VAW)", prob=TRUE, breaks = 100)

#Theoretical normal distribution
x = seq(from = min(D$VAW), to = max(D$VAW), by=0.001)
y = dnorm(x, mean_vaw, sd_vaw)
lines(x, y, col="red")

#Vertical lines highlighting 1,2,3 standard deviations to each side and the mean.
for (i in seq(-3, 3, 1)) {
  abline(v = mean_vaw + i*sd_vaw, col="blue")
}

##############################################################################
loss_vaw = min(D$VAW)
loss_vaw

#Number of standard distributions out:
distributions_out = loss_vaw/sd_vaw
distributions_out

#So the biggest loss of 20.4% in a week is almost 6 standard deviations out!

#The probability if this happening in a standard deviation is:
pnorm(loss_vaw, mean = mean_vaw, sd = sd_vaw)

##############################################################################
#Calculated "by hand":
mean_agg = mean(D$AGG)
sd_agg = sd(D$AGG)
mean_agg + c(-1, 1) * qt(0.975, length(D$AGG) - 1) * (sd_agg / sqrt(length(D$AGG)))

#Using built-in R function
t.test(D$AGG, conf.level = 0.95)
t.test(D$VAW, conf.level=0.95)
t.test(D$IWN, conf.level=0.95)
t.test(D$SPY, conf.level=0.95)


##############################################################################
#Using formula 3.23 to calculate the P-value
t_obs = (mean_agg - 0) / (sd_agg / sqrt(length(D$AGG)))
2 * (1 -pt(t_obs, df=length(D$AGG)-1))

#Same results as before, but this time we're specifying mu. 
#Doesn't make a difference though. 
t.test(D$AGG, mu=0)


##############################################################################
#method 3.51 p 172, using theorem 3.50
n_agg = length(D$AGG); n_vaw = length(D$VAW)
#mean_agg; sd_agg; n_agg
#mean_vaw; sd_vaw; n_vaw

t_obs = ( (mean_vaw - mean_agg) - 0) / sqrt( (sd_agg^2)/n_agg + (sd_vaw^2)/n_vaw )
v = ( (sd_agg^2/n_agg) + (sd_vaw^2/n_vaw) )^2 /
  ( (sd_agg^2/n_agg)^2 / (n_agg-1) + (sd_vaw^2/n_vaw)^2 / (n_vaw-1) )

t_obs; v; 2*(1 - pt(t_obs, v))

t.test(D$VAW, D$AGG)

##############################################################################
#Values for the covariance formula:
cov(D$VAW, D$IWN); sd(D$VAW); sd(D$IWN)
r = (0.0009838237) / (0.03608286 * 0.03201547)
r

# Computing the correlation between selected ETF's
cor(D[ ,c("AGG","VAW","IWN","SPY")], use="pairwise.complete.obs")

ylim = c(-0.2,0.2)
plot(c(D$t, D$t), c(D$VAW, D$IWN), type="p", ylim=ylim, xlab="Date", 
     ylab="Return", col=c("red", "green"), cex=0.4)






