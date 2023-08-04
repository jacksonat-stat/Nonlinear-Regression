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

##Derivatives of model for Z matrix
dfda<-deriv(model,"a")
dfdb<-deriv(model,"B")
dfda
dfdb
##create z matrix
z<-data.frame(matrix(ncol = 2, nrow = 44))
colnames(z)<-c("q","r")






##Step 1:starting values##
a<-.3
B<-.02
aB_matrix<-as.matrix(c(a,B))

##Step 2: Calculate Jacobian
z$q<-"1 - exp(-B * (x - 8))"
z$r<-"-(0.49 - a * (exp(-B * x - 8) * x - 8))"
# 
# ##include starting values for a and B parameter values
z$q<-"1 - exp(-.02 * (x - 8))"
z$r<-"-(0.49 - .3 * (exp(-.02 * x - 8) * x - 8))"

##Z matrix w. partial derivatives evaluated wrt A and B
z$q<-attributes(eval(dfda))$gradient
z$r<-attributes(eval(dfdb))$gradient



###Step 3##
##creation of matrix consisting of difference btwn y and model evaluated at parameters and x values
fj<-eval(model)
y2<-y-fj

#Perform Least Squares
k3<-solve(t(z)%*%as.matrix(z))%*%t(z)%*%y2
##add new paramters from LS to old parameters
aB_matrix1<-as.data.frame(k3+aB_matrix)


SaB<-(y - eval(model))^2
SaB_sum<-sum(SaB)
i=0
print("iteration")
print(i)
print(aB_matrix)
print(SaB_sum)


##Second iteration
a<-aB_matrix1$V1[1]
B<-aB_matrix1$V1[2]
aB_matrix<-as.matrix(c(a,B))


q<-attributes(eval(dfda))$gradient
r<-attributes(eval(dfdb))$gradient


z<-bind_cols(q,r)

fj<-eval(model)
y2<-y-fj

k3<-solve(t(z)%*%as.matrix(z))%*%t(z)%*%y2
aB_matrix1<-as.data.frame(k3+aB_matrix)


SaB<-(y - eval(model))^2
SaB_sum1<-sum(SaB)

print("iteration")
print(1)
print(aB_matrix1)
print(SaB_sum1)
g<-SaB_sum - SaB_sum1
g
g<.005