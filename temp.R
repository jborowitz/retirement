func <- function(x){
    return(-x^3+10*x^2)
}
test <- function(x){
    a <- seq(1,30,by=3)
    b <- func(a)
    #c <- approx(a,b,xout=x)
    c <- spline(a,b,xout=x)
    return(c)
}
    a <- seq(1,30,by=3)
    b <- func(a)
    #c <- approx(a,b,xout=x)
d<-test(seq(1.5,40.5,by=1))
plot(a,b)
lines(d$x,d$y)
#New plan: make lists of each input (so incomes are every $1000 from 0 to 2
# million).  Then, use interp with a rounding type to find the nearest
# value in each dimension.  Then I can look up an exact value.  I think
# this woudl NOT be convex, but I would just hope that doesn't really
# matter.
d <- array(0,dim=c(length(a),length(b)))
vapply(X=c(6,1,12,-1),FUN=inv,FUN.VALUE=0,seq(0,10,by=2))

inv <- function(value,vec){
    #Right now I am trying to vectorize 'inv' so I can vectorize other parts
    #of the code, including indexvalue
    #print('value is ')
    #print(value)
    #print('vec is ')
    #print(vec)
    if (value > max(vec)){
        #return(list(index=length(vec),value=vec[length(vec)]))
        return(length(vec))
    }
    if (value <= min(vec)){
        #return(list(index=1,value=vec[1]))
        return(1)
    }
    low <- sum(vec<value)
    high <- low+1
    lowv <- vec[low]
    highv <- vec[high]
    lowdiff <- abs(lowv-value)
    highdiff <- abs(highv-value)
    if (lowdiff < highdiff){
        #return(list(index=low,value=vec[low]))
        return(low)
        #return(list(key=low,value=lowv))
    }
    else{
        return(high)
        #return(list(index=high,value=vec[high]))
        #return(highv)
        #return(list(key=high,value=highv))
    }
}

agrid<-seq(1000,10000,by=1000)
bgrid<-seq(3,60,by=3)
dgrid<-seq(12,120,by=12)
e <- expand.grid(agrid,bgrid,dgrid)
func<- e$Var1 * e$Var2 * e$Var3
N <- 1
v <- c(6500,31,55)
#q <- t(matrix(v,dim=(N,length(v))))
mystuff<-indexvalue(array(seq(500,29*500+500,by=500)),array(rep(31,30)),array(rep(55,30)),agrid=agrid, bgrid=bgrid,dgrid=dgrid,func=func)
mystuff<-indexvalue(array(seq(500,500,by=500)),array(rep(31,N)),array(rep(55,N)),agrid=agrid, bgrid=bgrid,dgrid=dgrid,func=func)
mystuff<-indexvalue(seq(500,500,by=500),rep(31,N),rep(55,N),agrid=agrid, bgrid=bgrid,dgrid=dgrid,func=func)
mystuff<-indexvalue(seq(500,29*500+500,by=500),rep(31,30),rep(55,30),agrid=agrid, bgrid=bgrid,dgrid=dgrid,func=func)
print(mystuff)
print(c(a$val,6500*31*55))
# Thoughts on what's going on: I ultimately have some different ranges of x
# values, and I will computer the tax rate for each of these x values.  One
# thing that's likely for me to do is to expand and contract the number of
# different x values (e.g. what if we were in Florida instead?).  At the
# beginning of running a particular version, I will make a single call of
# taxsim.  This is reasonable because while there is fairly high overhead
# for the call, the overhead isnt' absolutely so large that you wouldn't
# want to pay it once per run.  Therefore, given an individual's X's, we
# could determine initially what variables will be changing and their
# values.  Thus the way to start might be to take as given ranges of
# particular values (either going to the indexvalue function or not) and
# hard code these in.  On the other hand, if I did this, I would still
# probably want to expand or contract the NUMBER of varying assumptions, so
# it would be good for this to work on arbitrary numbers of columns.

#The 'out' variable here is already pre-determined from my external run for
#taxsim, as are the a, b, and c variables which went into the expand
#operation.
