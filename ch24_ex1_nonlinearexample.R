

data <-matrix(c(1,4,16,.8,.45,.04),nrow=3,ncol = 2)
data<-as.data.frame(data)
t<-data$V1
y<-data$V2

##model expersion##
model <-expression(exp(-g*t))

##derivative of model expression with respect to g##
dfdg<-deriv(model,"g")
dfdg

##starting values##
g<-1
  
aB_matrix <-as.matrix(c(g))

##derivative of model expression wrt g evaluated with t values
q<-attributes(eval(dfdg))$gradient
##q relabled as dataframe##
z<-as.data.frame(q)

##y terms of model evaluated at inital values##
fj<-eval(model)

##y minus y terms of model evaluated at inital values##
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
repeat{
  i<-i+1
  if(exists("SaB_sum1")){SaB_sum<-SaB_sum1}
  if(exists("SaB_sum1")==FALSE)
  {SaB_sum<-SaB_sum}
  
  g<-aB_matrix1$V1[1]
  
  aB_matrix <-as.matrix(c(g))
  
  
  q<-attributes(eval(dfdg))$gradient
  
  
  z<-as.data.frame(q)
  
  fj<-eval(model)
  y2<-y-fj
  
  k3<-solve(t(z)%*%as.matrix(z))%*%t(z)%*%y2
  aB_matrix1<-as.data.frame(k3+aB_matrix)
  
  
  SaB<-(y - eval(model))^2
  SaB_sum1<-sum(SaB)
  
  print("iteration")
  print(i)
  print(aB_matrix1)
  print(SaB_sum1)
  h<-SaB_sum - SaB_sum1
  
  ##check statment##
  if(h<.005 & g >0)
  {break}
}