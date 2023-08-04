library(faraway)
data(bliss)
bliss

GLM1 <-glm(cbind(dead,alive)~conc,family = binomial,bliss)
summary(GLM1)
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  -2.3238     0.4179  -5.561 2.69e-08 ***
#   conc          1.1619     0.1814   6.405 1.51e-10 ***

attach(bliss)

y<-dead/30
mu<-y
mu
###log of mu##
eta<-logit(mu)
##log of mu + error divided by variance
z<-eta + (y-mu)/(mu*(1-mu))
##weights: 30* variance
w<-30*mu*(1-mu)

LM1<-lm(z~conc,weights = w)
summary(LM1)
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  -2.3025     0.1499  -15.36 0.000599 ***
#   conc          1.1536     0.0641   18.00 0.000374 ***
LM1$fitted.values

for (i in 1:3){
eta <-LM1$fitted.values;
mu<-ilogit(eta)
z<-eta+ (y-mu)/(mu*(1-mu))
w<-30*mu*(1-mu)
LM1 <-lm(z~conc,weights = w)
print(coef(LM1))

}
# (Intercept)        conc 
# -2.323672    1.161847 
# (Intercept)        conc 
# -2.323790    1.161895 
# (Intercept)        conc 
# -2.323790    1.161895 

link<-expression(log(m/(1-m)))

##derivative of link function with respect to mu##
dfdm<-deriv(link,"m")
dfdm