chlorine_data <-read.csv("C://Users//Andrea//Downloads//Chlorine Data - Sheet1.csv", header = FALSE)
colnames(chlorine_data)[1]<-"x"
colnames(chlorine_data)[2]<-"y"
x<-chlorine_data$x
y<-chlorine_data$y
model<-expression(a+(.49-a)*exp(-B*(x - 8)))
model
dfda<-deriv(model,"a")
dfdb<-deriv(model,"B")
dfda
dfdb

##starting values##
a<-.3
B<-.02
aB_matrix<-as.matrix(c(a,B))


q<-attributes(eval(dfda))$gradient
r<-attributes(eval(dfdb))$gradient


z<-bind_cols(q,r)

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

repeat{
i<-i+1
if(exists("SaB_sum1")){SaB_sum<-SaB_sum1}
if(exists("SaB_sum1")==FALSE)
  {SaB_sum<-SaB_sum}
  
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
print(i)
print(aB_matrix1)
print(SaB_sum1)
g<-SaB_sum - SaB_sum1

##check statment##
if(g<.005 & g >0)
{break}
}






###############Plot of Data#########
##Plot of data##
plot(x=chlorine_data$x, y = chlorine_data$y)



model<-expression(.39+(.49-.39)*exp(-.10*(x - 8)))

predicted_values <-eval(model)

plot(x=chlorine_data$x, y = chlorine_data$y)

lines(x=chlorine_data$x, y = predicted_values,col = "red")