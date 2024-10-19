
#### DATA SET  ####
Factors= matrix(c(31.08,39.84,37.68,59.68,31.32,37.32,35.4,56.72
                  ,34.2,43.08,38.52,61.36,32.76,39,37.08,60.04),byrow=T,ncol=1)
dimnames(Factors) = list(c("(1)","a","b","ab","c","ac","bc","abc",
                           "d","ad", "bd","abd","cd","acd","bcd","abcd"),c("Rate"))

####Estimation of the main factors forming the dataframe also####
A = rep(c(-1,1),8)
B =rep(c(-1,-1,1,1),4)
C= c(rep(-1,4),rep(1,4),rep(-1,4),rep(1,4))
D=c(rep(-1,8),rep(1,8))
data.rate=data.frame(A,B,C,D,Factors)##The given data
data.rate


#### Design Matrix ####
I=c(rep(1,16))
AB = A*B
AC = A*C
BC = B*C
ABC = A*B*C
AD=A*D
BD=B*D
ABD=A*B*D
CD=C*D
ACD=A*C*D
BCD=B*C*D
ABCD=A*B*C*D
Design.matrix=cbind(I, A,B,AB,C, AC,BC,ABC,D,AD,BD,ABD,
                    CD,ACD,BCD,ABCD,Factors)
Design.matrix

### Another way to justify that the effects are accurate #####

n = 1 ##Replication
Feff = t(Factors) %*% cbind(A,B,AB,C, AC,BC,ABC,D,AD,BD,ABD,CD,ACD,BCD,ABCD)/(8*n)
Feff
Ieff=t(Factors) %*% cbind(I)/(16*n)
Ieff
eff=cbind(Ieff,Feff)
eff
Summary = rbind( cbind(I,A,B,AB,C, AC,BC,ABC,D,AD,BD,ABD,CD,ACD,BCD,ABCD),eff)
Summary
dimnames(Summary)[[1]] = c(dimnames(Factors)[[1]],"Effect")
Summary

#### Half normal plot and estimate of effect of all the factors and their combination #####
library(unrepx)
G=Design.matrix[,17]
pilotEff = yates(G, labels = c("A","B","C", "D")) 
pilotEff
hnplot(pilotEff,ID=0)

#### Main and Interaction plot #####
lm.rate=lm(Factors ~ A*B, data=data.rate)
summary(lm.rate)

##### Model Adequacy Checking #####
mod=lm(Factors ~ A+B+A:B, data=data.rate)
summary(mod)
residuals=mod$res
residuals
qqnorm(mod$res,ylab="Ordinary Residuals")
qqline(mod$res)

##
library(DoE.base)
library(FrF2)
MEPlot(lm.rate)
IAPlot(lm.rate)


####### Design Projection ########
values <- matrix(c(31.08, 31.32, 34.2, 32.76, 39.84, 37.32, 43.08, 39, 37.68, 35.4, 38.52, 37.08, 59.68, 56.72, 61.36, 60.04), byrow = TRUE, ncol = 4)
dimnames(values) <- list(c("(1)", "a", "b", "ab"), c("Rep1", "Rep2", "Rep3", "Rep4"))
Total <- apply(values, 1, sum)
Total
A <- rep(c(-1, +1), 2)
B <- rep(c(-1, -1, +1, +1), 1)
data.mat <- data.frame(A, B, as.vector(values))
model <- aov(as.vector(values) ~ A*B, data = data.mat)
summary(model)

### Introducing 3 levels in A factor and 3 levels in the C factor and check for the basic factors###
data.rate <- data.frame(
  refresh_rate = c("144Hz", "90Hz", "60Hz", "144Hz", "90Hz", "60Hz", "144Hz", "90Hz", "60Hz"),
  brightness_level = c("L1", "L1", "L1", "L2", "L2", "L2", "L3", "L3", "L3"),
  rate = c(42.44,36.3,34.1,50.11 ,42.1 ,35.7, 63.2 ,49.8 ,37.89)
)

# Check the structure of the data
data.rate
str(data.rate)
data.rate
# Perform ANOVA analysis
rate.aov <- aov(rate ~ refresh_rate + brightness_level, data = data.rate)
summary(rate.aov)

# Perform LSD test
library(agricolae)
lsd.test <- LSD.test(rate.aov, "rate", alpha = 0.05, group = TRUE)
lsd.test

# Perform Tukey's HSD test
tukey <- TukeyHSD(rate.aov, ordered = TRUE, conf.level = 0.95)
tukey

# Check normality of residuals
qqnorm(resid(rate.aov))
qqline(resid(rate.aov), col = 'red')

library(DescTools)
g=factor(data.rate$refresh_rate)
dunnett= DunnettTest(data.rate$rate,g, control="144Hz", conf.level=0.95)
dunnett



