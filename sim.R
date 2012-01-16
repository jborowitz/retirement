rm(list=ls())
y0 <- .75
pi0 <- .0625
pi1 <- .001985
sig.w <- .00269
sig.delta <- .0000383
sig.mu0 <- .0901
sig.mu.delta <- -.00019033
rho <- .8468
theta <- -.0574
age <- 27
T2 <- 70
a0 <- 20


delta <- rnorm(1,mean=y0 * sig.mu.delta / sig.mu0,sd=sqrt(sig.delta - sig.mu.delta/sig.mu0))
#delta <- 0
w0 <- y0 - delta
xi <- rnorm(T2,sd=sqrt(pi0+pi1*(seq(age,T2 + age -1)-a0)))
w <- append(w0,rnorm(T2-1,sd=sqrt(sig.w)))
L <- rbind(rep(0,T2),cbind(diag(T2-1),rep(0,T2-1)))
#L <- cbind(rep(0,T2),rbind(diag(T2-1),rep(0,T2-1)))
mu <- solve(diag(T2)-L) %*% (delta + w)
nu <- solve(diag(T2) - rho* L) %*% (diag(T2) - theta * L) %*% xi
y = append(y0,mu + nu)
plot(y,ylim=range(y,mu,nu),type="n")
lines(y,lty=1)
lines(mu,lty=2)
lines(nu,lty=3)
#points(xi,pch=1)
#points(w,pch=2)
legend(1,range(y,mu,nu)[2],c("y","mu","nu"),lty=1:3)
print(cov(cbind(xi,w)))
print(y[1])

#print(c(y0,delta,mu))
