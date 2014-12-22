## LengthMatters.r
########################################################################
##
## i.e. Sail Boat Length
##
## Coursera DS Intro Course
## Basic excercise in using RStudio&R
##
########################################################################
##
## Data are extracted from the "www.YachtWorld.com"
##
## Imported data are stored in A (list)
## A=[Length, Boat, Year, PriceUSD, Type, Use, NofE, Fuel, Hull]
##
## Length - Boat Length [ft]
## Boat - Boat Brand Name
## Year - Year New
## PriceUSD - Asking Price [USD]
## Type = [S-Sail Boat]
## Use= [N-New ; U-Used]
## NoOfEngines=[S=Single;T=Twin;O=Other]
## Fuel = [D-Diesel;G-Gas/Petrol;O-Other]
## Hull = [O-Other;Al-Aluminium;FC-Ferro-Cement;ST-Steel;FG-Fiberglass;CF-Carbon-Fiber;CP-Composite;W-Wood]
##
########################################################################


# help(read.cvs), getwd()
# get the data

A <- read.csv(file="YachtWorld-coursera.csv",head=TRUE,sep=",")

# names(A), summary(A)

## select some data range

ind = A$Length < 50 & 
      A$Use == "U" & 
      A$Hull == "FG" & 
      A$Year > 1980 &
      A$PriceUSD < 500000;

## so, the 'measurement' is

BoatPriceUSD = A$PriceUSD[ind]
BoatLength = A$Length[ind]
BoatAge = 2015-A$Year[ind]

# some plots, help(plot)
plot(BoatLength,BoatPriceUSD)
plot(BoatAge,BoatPriceUSD)

# histograms

hist(BoatAge)
hist(BoatLength)
hist(BoatPriceUSD,20)

# Linear Model, BoatPriceUSD = (par[1])* BoatAge + (par[2]) * BoatLength
# least square parameter estimation, help(lsfit)
# BoatPriceUSD = X * par = [BoatAge | BoatLength] * [par1 par2]'

X=cbind(BoatAge, BoatLength)

par=lsfit(X, BoatPriceUSD)

summary(par)
ls.print(par)


# Length Matters?
plot(BoatLength, BoatPriceUSD, xlim = c(20,50), ylim = c(-100000,300000), )
par(new=T)
plot(BoatLength+0.5, BoatPriceUSD-par$residual, xlim = c(20,50), ylim = c(-100000,300000), xlab="", ylab="", col = 'red')
grid(col = "lightgray", lty = "dotted")


# linear model fit

par2=lm(BoatPriceUSD~BoatAge+BoatLength)
plot(par2)


# BoatPriceUSD vs. BoatAge
plot(BoatAge,BoatPriceUSD,xlim=c(0,max(BoatAge)),ylim=c(-100000,max(BoatPriceUSD)),xlab="",ylab="")
par(new=T)
plot(BoatAge+0.5,BoatPriceUSD-par$residual,xlim=c(0,max(BoatAge)),ylim=c(-100000,max(BoatPriceUSD)),xlab="",ylab="",col='red')
grid(col = "lightgray", lty = "dotted")
title(main="Boat Price [USD]", sub="black - data, red - linear least square estimate", ylab ="BoatPriceUSD", xlab = "Boat Age")

## BoatPriceUSD vs. BoatLength

plot(BoatLength,BoatPriceUSD,xlim=c(0,max(BoatLength)),ylim=c(-100000,max(BoatPriceUSD)),xlab="",ylab="")
par(new=T)
plot(BoatLength+0.5,BoatPriceUSD-par2$residual,xlim=c(0,max(BoatLength)),ylim=c(-100000,max(BoatPriceUSD)),xlab="",ylab="",col='red')
grid(col = "lightgray", lty = "dotted")
title(main="Boat Price [USD]", sub="black - data, red - least square estimate", ylab ="BoatPriceUSD", xlab = "Boat Length")


# plot some data for various boat lengths
for (len in 40:41) {
   lenInd = BoatLength == len
   plot(BoatAge[lenInd],BoatPriceUSD[lenInd],xlim=c(0,max(BoatAge)),ylim=c(-100000,max(BoatPriceUSD)), xlab="",ylab="")
   par(new=T)
   plot(BoatAge[lenInd],BoatPriceUSD[lenInd]-par2$residual[lenInd],xlim=c(0,max(BoatAge)),ylim=c(-100000,max(BoatPriceUSD)),xlab="",ylab="",type="l",col='red')
   grid(col = "lightgray", lty = "dotted")
   titleTXT = c("Boat Price [USD]","BoatLength = ",as.character(len))
   title(main=titleTXT, sub="black - data, red - least square estimate", xlab = "Boat Age", ylab ="BoatPriceUSD")
}


#multiple plots on one page

par(mfrow=c(1,2))
for (len in 41:42) {
   lenInd = BoatLength == len
   plot(BoatAge[lenInd],BoatPriceUSD[lenInd],xlim=c(0,max(BoatAge)),ylim=c(-100000,max(BoatPriceUSD)), xlab="",ylab="")
   par(new=T)
   plot(BoatAge[lenInd],BoatPriceUSD[lenInd]-par2$residual[lenInd],xlim=c(0,max(BoatAge)),ylim=c(-100000,max(BoatPriceUSD)),xlab="",ylab="",type="l",col='red')
   grid(col = "lightgray", lty = "dotted")
   titleTXT = c("Boat Price [USD]","BoatLength = ",as.character(len))
   title(main=titleTXT, sub="black - data, red - least square estimate", xlab = "Boat Age", ylab ="BoatPriceUSD")
}


# What is a best buy for 50K USD?, (best price,len,age)
ind50 = BoatPriceUSD<=50000
BoatAge50 = BoatAge[ind50]
BoatLength50=BoatLength[ind50]
BoatPriceUSD50=BoatPriceUSD[ind50]

mat=array(0,c(max(BoatAge50),3))

for (age in 1:34) {
  ind1 = BoatAge50==age
  bage = BoatAge50[ind1]
  blen = BoatLength50[ind1]
  bprice = BoatPriceUSD50[ind1]
  ind2 = match(max(blen),blen)
  mat[age,1:3] = c(age,blen[ind2],bprice[ind2])
}

# plot(age, length)
par(mfrow=c(1,1))
   plot(mat[1:34,1],mat[1:34,2],xlab="",ylab="",type="l",col='red')
   grid(col = "lightgray", lty = "dotted")
   titleTXT = c("Buy a Boat for 50K USD","Length vs. Age")
   title(main=titleTXT,xlab = "Boat Age", ylab ="Boat Lenght")


# What is a best buy (best price,len,age)

zmat = array(0,c(max(BoatAge),max(BoatLength)))

for (age in 1: max(BoatAge)) {
   ind3 = BoatAge == age
   BoatPriceUSDage=BoatPriceUSD[ind3]
 
   for (len in min(BoatLength):max(BoatLength)) {
      ind4 = BoatLength[ind3]== len
      temp = BoatPriceUSDage[ind4]
      if (length(temp)) {
        zmat[age,len] = min(BoatPriceUSDage[ind4])
      }
   }
}

# 3D, kind of
xa = seq(max(BoatAge))
ya = seq(max(BoatLength))
persp(xa,ya[20:49],zmat[1:34,20:49],theta=-30,phi=30,xlab="age",ylab="length", zlab="price", main="Best Buy")



