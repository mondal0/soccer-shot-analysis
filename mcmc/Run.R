library(Rcpp)
library(msm)
library(Matrix)
library(MASS)
library(spam)
set.seed(1)


## source files

sourceCpp("mcmc/rkslambda.cpp") # Note: Requires C++ shared libraries in R
source("mcmc/design.txt")

##

P=as(P, "dgCMatrix")
P1=as(P1, "dgCMatrix")
P2=as(P2, "dgCMatrix")
P3=as(P3, "dgCMatrix")
P4=as(P4, "dgCMatrix")
P5=as(P5, "dgCMatrix")

A1=as(A1, "dgCMatrix")
A2=as(A2, "dgCMatrix")
K=as(K, "dgCMatrix")


Ti=as(Ti, "dgCMatrix")
V=as(V, "dgCMatrix")
S=as(S, "dgCMatrix")

SH=as(SH, "dgCMatrix")
AH1=as(AH1, "dgCMatrix")
AH2=as(AH2, "dgCMatrix")

Fs=as(Fs, "dgCMatrix")

lam_p=lam_p1=lam_p2=lam_p3=lam_p4=lam_p5=200
lam_a1=lam_a2=lam_k=200
lam_b=lam_g=lam_t=lam_v=lam_s=200
lam_sh=lam_ah1=lam_ah2=200
lam_h=lam_psi=200
  
m_lam=19

mblks=c(m_p,m_p1,m_p2,m_p3,m_p4,m_p5,
        m_a,m_a,m_k,
        m_b,m_g,m_t,m_v,m_s,
        m_s,m_a,m_a,
        m_h,m_psi)

istart= cumsum(c(1,mblks))[1:m_lam]
iend=cumsum(mblks)


Q_p = diag(1,m_p)+1
Q_p1 = diag(1,m_p1)+1
Q_p2 = diag(1,m_p2)+1
Q_p3 = diag(1,m_p3)+1
Q_p4 = diag(1,m_p4)+1
Q_p5 = diag(1,m_p5)+1

Q_a = diag(1,m_a)+1
Q_k = diag(1,m_k)+1

Q_t = diag(1,m_t)+1
Q_v = diag(1,m_v)+1
Q_s = diag(1,m_s)+1

Q_psi=as(Q_psi, "dgCMatrix")


Q=bdiag(lam_p*Q_p, lam_p1*Q_p1, lam_p2*Q_p2, lam_p3*Q_p3, lam_p4*Q_p4, lam_p5*Q_p5,
        lam_a1*Q_a, lam_a2*Q_a, lam_k*Q_k,
        lam_b*diag(1,m_b), lam_g*diag(1,m_g), lam_t*Q_t, lam_v *Q_v, lam_s*Q_s,
        lam_sh*Q_s, lam_ah1*Q_a, lam_ah2*Q_a,
        lam_h *diag(1,m_h), lam_psi*Q_psi)


X=as(cbind(P, P1, P2, P3, P4, P5,
            A1, A2, K,
            B, G, Ti, V, S,
            SH, AH1, AH2,
            H, Fs), "dgCMatrix")

m=ncol(X)

y=1-Soccer_Data$result


index1=which(y==1)
index0=which(y==0)

m_in0=length(index0)
m_in1=length(index1)


wei=rep(1,n)
XWX=t(X)%*%Diagonal(n,wei)%*%X
z=rep(0,n)
beta.x=rep(0,m)

a=1
b=.005


## MCMC initialize 

nsim=12000
burnin=2000
n.fix=nsim-burnin


beta.m=matrix(0,n.fix,m)
lambda.m=matrix(0,n.fix,m_lam)

WEI = numeric(n)
RES = numeric(n)

for(r in 1:nsim){
  
  bj=beta.x[istart[1]:iend[1]]
  r1=a + m_p/2
  r2= b+ as.numeric(t(bj)%*%Q_p%*%bj)/2
  lam_p=rgamma(1,r1,r2)
  
  bj=beta.x[istart[2]:iend[2]]
  r1=a + m_p1/2
  r2= b+ as.numeric(t(bj)%*%Q_p1%*%bj)/2
  lam_p1=rgamma(1,r1,r2)
  
  bj=beta.x[istart[3]:iend[3]]
  r1=a + m_p2/2
  r2= b+ as.numeric(t(bj)%*%Q_p2%*%bj)/2
  lam_p2=rgamma(1,r1,r2)
  
  bj=beta.x[istart[4]:iend[4]]
  r1=a + m_p3/2
  r2= b+ as.numeric(t(bj)%*%Q_p3%*%bj)/2
  lam_p3=rgamma(1,r1,r2)
  
  bj=beta.x[istart[5]:iend[5]]
  r1=a + m_p4/2
  r2= b+ as.numeric(t(bj)%*%Q_p4%*%bj)/2
  lam_p4=rgamma(1,r1,r2)
  
  bj=beta.x[istart[6]:iend[6]]
  r1=a + m_p5/2
  r2= b+ as.numeric(t(bj)%*%Q_p5%*%bj)/2
  lam_p5=rgamma(1,r1,r2)
  
  
  # 2
  bj=beta.x[istart[7]:iend[7]]
  r1=a + m_a/2
  r2= b+ as.numeric(t(bj)%*%Q_a%*%bj)/2
  lam_a1=rgamma(1,r1,r2)
  
  bj=beta.x[istart[8]:iend[8]]
  r1=a + m_a/2
  r2= b+ as.numeric(t(bj)%*%Q_a%*%bj)/2
  lam_a2=rgamma(1,r1,r2)
  
  bj=beta.x[istart[9]:iend[9]]
  r1=a + m_k/2
  r2= b+ as.numeric(t(bj)%*%Q_k%*%bj)/2
  lam_k=rgamma(1,r1,r2)
  
  
  # 3
  bj=beta.x[istart[10]:iend[10]]
  r1=a + m_b/2
  r2= b+ sum(bj^2)/2
  lam_b=rgamma(1,r1,r2)
  
  bj=beta.x[istart[11]:iend[11]]
  r1=a + m_g/2
  r2= b+ sum(bj^2)/2
  lam_g=rgamma(1,r1,r2)
  
  bj=beta.x[istart[12]:iend[12]]
  r1=a + m_t/2
  r2= b+ as.numeric(t(bj)%*%Q_t%*%bj)/2
  lam_t=rgamma(1,r1,r2)
  
  bj=beta.x[istart[13]:iend[13]]
  r1=a + m_v/2
  r2= b+ as.numeric(t(bj)%*%Q_v%*%bj)/2
  lam_v=rgamma(1,r1,r2)
  
  bj=beta.x[istart[14]:iend[14]]
  r1=a + m_s/2
  r2= b+ as.numeric(t(bj)%*%Q_s%*%bj)/2
  lam_s=rgamma(1,r1,r2)
  
  # 4
  bj=beta.x[istart[15]:iend[15]]
  r1=a + m_s/2
  r2= b+ as.numeric(t(bj)%*%Q_s%*%bj)/2
  lam_sh=rgamma(1,r1,r2)
  
  bj=beta.x[istart[16]:iend[16]]
  r1=a + m_a/2
  r2= b+ as.numeric(t(bj)%*%Q_a%*%bj)/2
  lam_ah1=rgamma(1,r1,r2)
  
  bj=beta.x[istart[17]:iend[17]]
  r1=a + m_a/2
  r2= b+ as.numeric(t(bj)%*%Q_a%*%bj)/2
  lam_ah2=rgamma(1,r1,r2)
  
  # 5
  bj=beta.x[istart[18]:iend[18]]
  r1=a + m_h/2
  r2= b+ sum(bj^2)/2
  lam_h=rgamma(1,r1,r2)
  
  bj=beta.x[istart[19]:iend[19]]
  r1=a + m_psi/2
  r2= b+ as.numeric(t(bj)%*%Q_psi%*%bj)/2
  lam_psi=rgamma(1,r1,r2)
  
  
  Q=bdiag(lam_p*Q_p, lam_p1*Q_p1, lam_p2*Q_p2, lam_p3*Q_p3, lam_p4*Q_p4, lam_p5*Q_p5,
          lam_a1*Q_a, lam_a2*Q_a, lam_k*Q_k,
          lam_b *diag(1,m_b), lam_g*diag(1,m_g), lam_t*Q_t, lam_v *Q_v, lam_s*Q_s,
          lam_sh*Q_s, lam_ah1*Q_a, lam_ah2*Q_a,
          lam_h *diag(1,m_h), lam_psi*Q_psi)
  
  lambda=c(lam_p,lam_p1,lam_p2,lam_p3,lam_p4,lam_p5,
           lam_a1,lam_a2,lam_k,
           lam_b,lam_g,lam_t,lam_v,lam_s,
           lam_sh,lam_ah1,lam_ah2,
           lam_h,lam_psi)
  
  # update z
  
  mu= X%*%beta.x
  sigma=1/sqrt(wei)
  
  z[index0]= rtnorm(m_in0, mean = mu[index0], sd = sigma[index0], lower = -Inf, upper = 0)
  z[index1]= rtnorm(m_in1, mean = mu[index1], sd = sigma[index1], lower = 0, upper = Inf)
  
  
  #  update beta
  
  XWX=t(X)%*%Diagonal(n,wei)%*%X
  Q0= as.matrix(Q + XWX)
  R0=chol(Q0, symmetric=T, sparse=T)
  
  beta.x=  solve(Q0,(t(X)%*%Diagonal(n,wei)%*% z), sparse=T) + backsolve(R0, rnorm(m))
  
  
  # update weights
  
  mu= X%*%beta.x
  res.act = z-mu
  res.abs=abs(res.act)
  
  
  wei=1/kslambda(as.numeric(res.abs))
  
  
  #print(r)
  if(r > burnin){
    
    lambda.m[(r-burnin),]=lambda
    beta.m[(r-burnin),]=as.numeric(beta.x)
    WEI = ((r-burnin-1)*WEI + wei)/(r-burnin)
    RES = ((r-burnin-1)*RES + res.act)/(r-burnin)
  }
  
  if(r%%300==0) {
    
    print(r)
    print(lambda)
    print(mean(beta.x))
    print(mean(wei))
    print(Sys.time() - st)
  }
  
}

print("end")

saveRDS(beta.m[,1:1777], file = "Beta1.rds")
saveRDS(beta.m[,1778:5995], file = "Beta2.rds")

#save.image("SpatialIRT.RData")

