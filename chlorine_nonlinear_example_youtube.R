chlorine_data <-read.csv("C://Users//Andrea//Downloads//Chlorine Data - Sheet1.csv", header = FALSE)
colnames(chlorine_data)[1]<-"x"
colnames(chlorine_data)[2]<-"y"
x<-chlorine_data$x
y<-chlorine_data$y

##Plot of data##
plot(x=chlorine_data$x, y = chlorine_data$y)


##Nonlinear regression function##
nls(
formula = y~a+(.49-a)*exp(-B*(x - 8)),
data = chlorine_data,
start =  list(a=.3,B=.02)
)
# Nonlinear regression model
# model: y ~ a + (0.49 - a) * exp(-B * (x - 8))
# data: chlorine_data
# a      B 
# 0.3901 0.1016 
# residual sum-of-squares: 0.005002
# 
# Number of iterations to convergence: 9 
# Achieved convergence tolerance: 4.767e-06

##Model##
model<-expression(a+(.49-a)*exp(-B*(x - 8)))
model


dfda<-deriv(model,"a")
dfdb<-deriv(model,"B")
dfda
dfdb
##create z matrix
z<-data.frame(matrix(ncol = 2, nrow = 44))
x<-c("q","r")
colnames(z)<-x
z$q<-"1 - exp(-B * (x - 8))"
z$r<-"-(0.49 - a * (exp(-B * x - 8) * x - 8))"

##include starting values for a and B parameter values
z$q<-"1 - exp(-.02 * (x - 8))"
z$r<-"-(0.49 - .3 * (exp(-.02 * x - 8) * x - 8))"





##starting values##
a<-.3
B<-.02
aB_matrix<-as.matrix(c(a,B))


z$q<-attributes(eval(dfda))$gradient
z$r<-attributes(eval(dfdb))$gradient




fj<-eval(model)
y2<-y-fj

k3<-solve(t(z)%*%as.matrix(z))%*%t(z)%*%y2
aB_matrix1<-as.data.frame(k3+aB_matrix)


SaB<-(y - eval(model))^2
SaB_sum<-sum(SaB)
i=0
print("iteration")
print(i)
print(aB_matrix)
print(SaB_sum)