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
    #print(length(value))
    #print(is.list(value))
    #print(is.atomic(value))
    #print(is.list(vec))
    #print(is.array(vec))
    #print(is.vector(vec))
    print(value > max(vec))
    if (value > max(vec)){
        return(length(vec))
    }
    if (value <= min(vec)){
        return(0)
    }
    low <- sum(vec<value)
    high <- low+1
    lowv <- vec[low]
    highv <- vec[high]
    lowdiff <- abs(lowv-value)
    highdiff <- abs(highv-value)
    if (lowdiff < highdiff){
        return(lowv)
        #return(list(key=low,value=lowv))
    }
    else{
        return(highv)
        #return(list(key=high,value=highv))
    }
}
indexvalue <- function(aval,bval,dval){
    #this function takes 3 function values and returns the indexes that
    #would need to be used to look up the three dimensional function value.
    #So there's a function f(aval,bval,cval) that is represented by some
    #discreete points a1...an, b1...bn, and c1...cn, and we want to know
    #f(aval, bval, cval), but we really need to know the index ai, bi, ci,
    #which would best approximate f(aval, bval, cval).  
    a<-seq(1000,10000,by=1000)
    b<-seq(3,60,by=3)
    d<-seq(12,120,by=12)
    e <- expand.grid(a,b,d)
    names(e)
    out<- e$Var1 * e$Var2 * e$Var3
    acall <- inv(a,aval)
    bcall <- inv(b,bval)
    dcall <- inv(d,dval)
    output <- array(out,dim=c(length(a),length(b),length(d)))
    #The key insight here is expand.grid(a1,a2,a3) to
    #array(.,dim=c(a1,a2,a3)) is what orders things into a 3d array
    return(list(output=output,val=output[acall$key,bcall$key,dcall$key]))
}
a<-indexvalue(6500,31,55)
print(a)
print(names(a))
