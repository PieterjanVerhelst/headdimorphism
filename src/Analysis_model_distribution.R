# Mixture analysis
# Pieterjan Verhelst

# First manually run scripts:
# Preprocessing.R
# Determine_headwidth.R


# Additional info
# https://stats.stackexchange.com/questions/132652/how-to-determine-which-distribution-fits-my-data-best
# https://cran.r-project.org/web/packages/mclust/vignettes/mclust.html
# https://www.jmp.com/support/help/Likelihood_AICc_and_BIC.shtml


#library(betareg)
library(mixtools)
library(mclust)


###############
# UNIMODAL
###############

# Plot raw density data (non-parametric plot)
plot(eels$diff)
d <- density(eels$diff) # returns the density data
plot(d) # plots the results 

# Plot histogram and ecdf
hist(eels$diff, breaks = 20, xlim=c(-0.10, 0.15))
plot(ecdf(eels$diff),main="Empirical cumulative distribution function")

# Plot QQ plot
z.norm<-(eels$diff-mean(eels$diff))/sd(eels$diff) ## standardized data
qqnorm(z.norm) ## drawing the QQplot
abline(0,1) ## drawing a 45-degree reference line


# Fit normal curve
# Normality if p-value > 0.05 (H0: normality)
shapiro.test(eels$diff)    # Data follows a normal distribution
mean_diff<-mean(eels$diff)
mean_sd<-sd(eels$diff)
MASS::fitdistr(eels$diff,"normal")
curve(dnorm(x,m=mean_diff,sd=mean_sd),from=-0.1,to=0.15,main="Normal distribution")
AIC(MASS::fitdistr(eels$diff,"normal"))     # https://www.r-bloggers.com/fitting-a-model-by-maximum-likelihood/
BIC(MASS::fitdistr(eels$diff,"normal"))
logLik(MASS::fitdistr(eels$diff,"normal"))



# Combine plots
h<-hist(eels$diff, breaks = 20, xlim=c(-0.10, 0.15), ylim=c(0, 60), main="Normal pdf and
     histogram")
xhist<-c(min(h$breaks),h$breaks)
yhist<-c(0,h$density,0)
xfit<-seq(min(eels$diff),max(eels$diff),length=100)
yfit<-dnorm(xfit,mean=mean(eels$diff),sd=sd(eels$diff))
#plot(xhist,yhist,type="s",ylim=c(0,max(yhist,yfit)), main="Normal pdf and histogram")
lines(d, col = "green", lwd = 2)      # non-parametric pdf
lines(xfit,yfit, col="red", lwd = 2)  # normal pdf



plot(d, lty = 2, lwd = 2)
lines(xfit,yfit, col="green", lwd = 2)  # normal pdf



###############
# BIMODAL
###############

# Plot bimodal distribution
# https://www.r-bloggers.com/fitting-mixture-distributions-with-the-r-package-mixtools/
residuals <- eels$diff
mixmdl <- normalmixEM(residuals, maxit = 2000, k=2)
plot(mixmdl,which=2, xlim=c(-0.10, 0.15), breaks = 10, whichplots = 2)
lines(density(residuals), lty=2, lwd=2)    # overlays the nonparametric density estimate
# Export figure as svg file




###############
# SELECTION CRITERIA
###############

# Calculate selection criteria

uni<-Mclust(eels$diff,1)
bi<-Mclust(eels$diff,2)
AIC(uni)
AIC(bi)
BIC(uni)
BIC(bi)

summary(uni)    # small values of BIC, AIC and log likelihood indicate better models
summary(bi)



