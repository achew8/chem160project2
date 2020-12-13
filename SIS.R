alpha<-0.00218

alpha.f<-0.0000009
alpha.m<-0.000006
gamma.f<-0.007
gamma.m<-0.05

Sf.hist<- c()
Sm.hist<- c()
Im.hist<- c()
If.hist<- c()

Sf<-9000
Sm<-14000
Im<-1000
If<-1000

for (day in 1:2000) {
	Sf.hist[day]<-Sf
	Sm.hist[day]<-Sm
	Im.hist[day]<-Im
	If.hist[day]<-If

	delta.Sf <-(gamma.f*If-alpha.f*Sf*Im)
	delta.Sm <-(gamma.m*Im-alpha.m*Sm*If)
	delta.Im <-(alpha.m*Sm*If-gamma.m*Im)
	delta.If <-(alpha.f*Sf*Im-gamma.f*If)
	delta.Im <-(alpha.m*Sm*If-gamma.m*Im)

	Sf<-Sf+delta.Sf
	Sm<-Sm+delta.Sm
	If<-If+delta.If
	Im<-Im+delta.Im

	Sf<-max(Sf,0) 
	Sm<-max(Sm,0) 
	If<-max(If,0)
	Im<-max(Im,0)

}

plot(Im.hist,col=7,type="l",main="SIS Model",ylim=c(0,6000),ylab="Infected People",xlab="Days")
