
w<-c(610,450,450,430,410,500,500,500,450,450,480,475,485,474,485,600)
d<-c(550,500,520,740,800,230,235,240,600,650,230,1400,615,515,700,750)
y<-c(33.6,22.3,22,18.7,20.2,31,30,32,26.6,15.1,30,13.5,26.8,25,20.4,15)
w<-as.data.frame(w)
y<-as.data.frame(y)
d<-as.data.frame(d)
mining_data<-bind_cols(w,d,y)

model<-expression(a*(1-exp(-B*(w/d))))

dfda <-deriv(model,"a")

dfdB<-deriv(model,"B")


##starting values##
##starting values##
a<-35
B<-1
aB_matrix<-as.matrix(c(a,B))


q<-attributes(eval(dfda))$gradient
r<-attributes(eval(dfdB))$gradient


z<-bind_cols(q,r)

fj<-eval(model)
y2<-y-fj


k3<-solve(t(z)%*%as.matrix(z))%*%t(z)%*%as.matrix(y2)



aB_matrix1<-as.data.frame(k3+aB_matrix)


SaB<-(y - eval(model))^2
SaB_sum<-sum(SaB)
i=0
print("iteration")
print(i)
print(aB_matrix)
print(SaB_sum)

#aB_matrix<-as.data.frame(aB_matrix)
repeat{
  i<-i+1
  if(exists("SaB_sum1")){SaB_sum<-SaB_sum1}
  if(exists("SaB_sum1")==FALSE)
  {SaB_sum<-SaB_sum}
  
    a<-aB_matrix1$y[1]
    B<-aB_matrix1$y[2]

  aB_matrix<-as.matrix(c(a,B))
  
  
  q<-attributes(eval(dfda))$gradient
  r<-attributes(eval(dfdB))$gradient
  
  
  z<-bind_cols(q,r)
  
  fj<-eval(model)
  y2<-y-fj
  
  k3<-solve(t(z)%*%as.matrix(z))%*%t(z)%*%as.matrix(y2)
  aB_matrix1<-as.data.frame(k3+aB_matrix)
  
  
  SaB<-(y - eval(model))^2
  SaB_sum1<-sum(SaB)
  
  print("iteration")
  print(i)
  print(aB_matrix1)
  print(SaB_sum1)
  g<-SaB_sum - SaB_sum1
  
  ##check statment##
  if(g<.01 & g >0)
  #if(i==215)
  {break}
}

