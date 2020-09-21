library(MCMCpack)
b=2
alpha=c(2,1,1)
D=4
K=3
V=6
N=10
eta=rep(b,V)
beta=c()
for (k in 1:K) {
  beta=rbind(beta,rdirichlet(1, eta ))
}
apply(beta, 1, sum)
a=array(dim = c(N,K,D))
for(d in 1:D){
  teta=rdirichlet(1, alpha )
  for (n in 1:N) {
    z=sample(1:K, 1, prob=teta)
    w=sample(1:V, 1, prob=beta[z,])
    a[n,z,d]=w
  }
}

t_w=matrix(NA,D,V)
word=c()
j=1

for(d in 1:D)
{   
  for (v in 1:V) {
    a=sample(1:K, 1, prob=document[D,])
    t_w[d,v]=a
    b=sample(1:V, 1, prob=topic[a,])
    word[j]=b
    j=j+1
  }
}
t_w
word

f <- function(p){
  sample(1:K,1,prob = p)
}
f(c(.1,.8,.1))
z = apply(teta,1, f)
z
