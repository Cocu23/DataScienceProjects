#Data
LinRegData=mydata[,1:2]
LinRegData

#First Glance
summary(LinRegData)


plot(LinRegData)


model= lm (V11 ~V12, data = LinRegData)
model

#     Coefficients:
#     (Intercept)      V12  
#     0.8896      -0.1245  

plot(model)


attach(LinRegData)
plot(V11, V12,xlab = " v11", ylab = "v12", main = "LinReg",  pch=19)

abline(lm(V11~V12), col="red") # regression line (y~x) 
lines(lowess(V11,V12), col="blue") # lowess line (x,y)
