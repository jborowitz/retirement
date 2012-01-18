#rm(list=ls())
y0 <- .15
pi0 <- .0625
pi1 <- .001985
sig.w <- .00269
sig.delta <- .0000383
sig.mu0 <- .0901
sig.mu.delta <- -.00019033
rho <- .8468
theta <- -.574
age <- 27
T2 <- 70
a0 <- 20


delta <- rnorm(1,mean=y0 * sig.mu.delta / sig.mu0,sd=sqrt(sig.delta - sig.mu.delta/sig.mu0))
#delta <- 0
print(delta)
w0 <- y0 - delta
b<-pi0+pi1*(seq(age,T2 + age -1)-a0)/10
xi <- rnorm(T2,sd=sqrt(pi0+pi1*(seq(age,T2 + age -1)-a0)/10))
w <- append(w0,rnorm(T2-1,sd=sqrt(sig.w)))
L <- rbind(rep(0,T2),cbind(diag(T2-1),rep(0,T2-1)))
#L <- cbind(rep(0,T2),rbind(diag(T2-1),rep(0,T2-1)))
mu <- solve(diag(T2)-L) %*% (.1* delta + w)
nu <- solve(diag(T2) - rho* L) %*% (diag(T2) - theta * L) %*% xi
y = append(y0,mu + nu)
plot(y,ylim=range(y,mu,nu),type="n")
lines(y,lty=1)
lines(mu,lty=2)
lines(nu,lty=3)
points(xi,pch=1)
points(w,pch=2)
legend(1,range(y,mu,nu)[2],c("y","mu","nu"),lty=1:3)
print(cov(cbind(xi,w)))
print(y[1])
print(var(y))

#Note; it's a little ambiguous from their paper whether Gottschalk and
#Moffit actually calibrate and report delta and w as age group changes or
#age changes, so whether you get a new one when you go from your 30s to your
#40s, or whether you get a new one every year.  Playing with the parameters,
#if you go with the decade interpretation, so you would have to scale delta
#and w down by .1, then you get really almost entirely transitory earnings
#differences.
