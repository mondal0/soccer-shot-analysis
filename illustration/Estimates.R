library(bayestestR)

Beta = readRDS("output/Beta1.rds")
Beta.Fs = readRDS("output/Beta2.rds")

Beta = Beta[4*(1:2500)-1,]
Beta.Fs = Beta.Fs[4*(1:2500)-1,]

m_p = 4
m_p1 = 523
m_p2 = 111
m_p3 = 378
m_p4 = 244
m_p5 = 341

m_a = 34
m_k = 6

m_b = 1
m_g = 1
m_t = 5
m_v = 6
m_s = 10

m_h = 1
m_psi = 57*74

m_1 =57
m_2 =74

m = c(m_p,m_p1,m_p2,m_p3,m_p4,m_p5,
      m_a,m_a,m_k,
      m_b,m_g,m_t,m_v,m_s,
      m_s,m_a,m_a,
      m_h,m_psi)

m = cumsum(m)

beta.P = cbind(-rowSums(Beta[,1:m[1]]), Beta[,1:m[1]])
beta.P1 = cbind(-rowSums(Beta[,(m[1]+1):m[2]]), Beta[,(m[1]+1):m[2]])
beta.P2 = cbind(-rowSums(Beta[,(m[2]+1):m[3]]), Beta[,(m[2]+1):m[3]])
beta.P3 = cbind(-rowSums(Beta[,(m[3]+1):m[4]]), Beta[,(m[3]+1):m[4]])
beta.P4 = cbind(-rowSums(Beta[,(m[4]+1):m[5]]), Beta[,(m[4]+1):m[5]])
beta.P5 = cbind(-rowSums(Beta[,(m[5]+1):m[6]]), Beta[,(m[5]+1):m[6]])
beta.PA = cbind(beta.P1, beta.P2, beta.P3, beta.P4, beta.P5)

beta.A1 = cbind(-rowSums(Beta[,(m[6]+1):m[7]]), Beta[,(m[6]+1):m[7]])
beta.A2 = cbind(-rowSums(Beta[,(m[7]+1):m[8]]), Beta[,(m[7]+1):m[8]])
beta.K = cbind(-rowSums(Beta[,(m[8]+1):m[9]]), Beta[,(m[8]+1):m[9]])

beta.B = cbind(Beta[,(m[9]+1):m[10]], -Beta[,(m[9]+1):m[10]])
beta.G = cbind(Beta[,(m[10]+1):m[11]], -Beta[,(m[10]+1):m[11]])
beta.Ti = cbind(-rowSums(Beta[,(m[11]+1):m[12]]), Beta[,(m[11]+1):m[12]])
beta.V = cbind(-rowSums(Beta[,(m[12]+1):m[13]]), Beta[,(m[12]+1):m[13]])
beta.S = cbind(-rowSums(Beta[,(m[13]+1):m[14]]), Beta[,(m[13]+1):m[14]])

beta.SH = cbind(-rowSums(Beta[,(m[14]+1):m[15]]), Beta[,(m[14]+1):m[15]])
beta.AH1 = cbind(-rowSums(Beta[,(m[15]+1):m[16]]), Beta[,(m[15]+1):m[16]])
beta.AH2 = cbind(-rowSums(Beta[,(m[16]+1):m[17]]), Beta[,(m[16]+1):m[17]])

beta.H = cbind(Beta[,(m[17]+1):m[18]], -Beta[,(m[17]+1):m[18]])
beta.Fs = Beta.Fs


est.P = as.numeric(colMeans(beta.P))
est.PA = as.numeric(colMeans(beta.PA))

est.A1 = as.numeric(colMeans(beta.A1))
est.A2 = as.numeric(colMeans(beta.A2))
est.K = as.numeric(colMeans(beta.K))

est.B = as.numeric(colMeans(beta.B))
est.G = as.numeric(colMeans(beta.G))
est.Ti = as.numeric(colMeans(beta.Ti))
est.V = as.numeric(colMeans(beta.V))
est.S = as.numeric(colMeans(beta.S))

est.H = as.numeric(colMeans(beta.H))
est.Fs = as.numeric(colMeans(beta.Fs))

########### Comp~1 Probabilities

x.P = 3     # Player Position: 4: Son, 5: Kane 3: De Bruyne
x.PA = 647 # Player: 1021: Son, 1305: Kane, 647: De Bruyne

x.A1 = 19   # Attacking Team: 19: Man City, 20: Man Utd, 31: Tottenham
x.A2 = 20   # Defending Team: 19: Man City, 20: Man Utd, 31: Tottenham
x.K = 1     # Defending Formation: 1: 4-2-3-1, 5: 3-4-2-1

x.B = 2     # 2: Home, 1: Away
x.G = 1     # 1: Open play, 2: Free kick
x.Ti = 6    # Time: 1: 1st 15 mins,..., 6: Last 15 mins
x.V = 4     # Goal difference: 2: Losing by 2 goals, 4: Draw, 6: Winning by 2 goals
x.S = 11     # Season: 1: 2014-15,..., 11: 2024-25

x.H = 2     # 1: Right foot, 2: Left foot
x.Fs = 2490 # Spatial position: I: 2490 (2491 original), I': 1749

indc = 0
if(x.B == 1){
  indc = 1
  beta.SH = -beta.SH
  beta.AH1 = -beta.AH1
  beta.AH2 = -beta.AH2
}

x.beta = c(x.P, x.PA,
           x.A1, x.A2, x.K,
           x.B, x.G, x.Ti, x.V, x.S,
           x.S, x.A1, x.A2,
           x.H, x.Fs)


xt_beta = numeric(nrow(Beta))
for(it in 1:nrow(Beta)){
  Beta.est = list(beta.P[it,], beta.PA[it,],
                  beta.A1[it,], beta.A2[it,], beta.K[it,],
                  beta.B[it,], beta.G[it,], beta.Ti[it,], beta.V[it,], beta.S[it,],
                  beta.SH[it,], beta.AH1[it,], beta.AH2[it,],
                  beta.H[it,], beta.Fs[it,])
  
  for(i in 1:length(Beta.est)){
    temp = as.numeric(Beta.est[[i]][x.beta[i]])
    xt_beta[it] = xt_beta[it] + temp
  }
  
}
prob1 = exp(xt_beta)/(1+exp(xt_beta))

###### Comp~2 Probabilities

x.P = 3     # Player Position: 4: Son, 5: Kane 3: De Bruyne
x.PA = 647 # Player: 1021: Son, 1305: Kane, 647: De Bruyne

x.A1 = 19   # Attacking Team: 19: Man City, 20: Man Utd, 31: Tottenham
x.A2 = 20   # Defending Team: 19: Man City, 20: Man Utd, 31: Tottenham
x.K = 1     # Defending Formation: 1: 4-2-3-1, 5: 3-4-2-1

x.B = 1     # 2: Home, 1: Away
x.G = 1     # 1: Open play, 2: Free kick
x.Ti = 6    # Time: 1: 1st 15 mins,..., 6: Last 15 mins
x.V = 4     # Goal difference: 2: Losing by 2 goals, 4: Draw, 6: Winning by 2 goals
x.S = 11     # Season: 1: 2014-15,..., 11: 2024-25

x.H = 2     # 1: Right foot, 2: Left foot
x.Fs = 2490 # Spatial position: I: 2490, I': 1749


if(x.B == 1 & indc == 0){
  beta.SH = -beta.SH
  beta.AH1 = -beta.AH1
  beta.AH2 = -beta.AH2
}

x.beta = c(x.P, x.PA,
           x.A1, x.A2, x.K,
           x.B, x.G, x.Ti, x.V, x.S,
           x.S, x.A1, x.A2,
           x.H, x.Fs)


xt_beta = numeric(nrow(Beta))
for(it in 1:nrow(Beta)){
  Beta.est = list(beta.P[it,], beta.PA[it,],
                  beta.A1[it,], beta.A2[it,], beta.K[it,],
                  beta.B[it,], beta.G[it,], beta.Ti[it,], beta.V[it,], beta.S[it,],
                  beta.SH[it,], beta.AH1[it,], beta.AH2[it,],
                  beta.H[it,], beta.Fs[it,])
  
  for(i in 1:length(Beta.est)){
    temp = as.numeric(Beta.est[[i]][x.beta[i]])
    xt_beta[it] = xt_beta[it] + temp
  }
  
}
prob2 = exp(xt_beta)/(1+exp(xt_beta))

####################
mean(prob1) * 100
hdi(prob1) * 100

mean(prob2) * 100
hdi(prob2) * 100

mean(prob1/prob2) # Ratio
hdi(prob1/prob2)

##################### Estimates and differences

# Attacking ability
round(mean(beta.A1[,19]),2) # 19: MCI, 31: TOT, 16: LEI
hdi(beta.A1[,19])

# Defensive ability
round(mean(-beta.A2[,6]),2) # 6: BRN, 20: MUN, 31: TOT
hdi(-beta.A2[,31])

# Player effects
round(mean(beta.PA[,1021] + beta.P[,5]), 2) # (1021, 4): Son, (1314, 5): Hazard, (1305, 5): Kane
hdi(beta.PA[,1021] + beta.P[,5])

# Home-Away
round(mean(beta.B[,2]),2) # 2: Home
hdi(beta.B[,2])

# Situation
round(mean(beta.G[,1]),2) # 1: Openplay
hdi(beta.G[,1])

# Time effect
Dt = beta.Ti[,6] - beta.Ti[,3] # 6: last 15 minutes of 2nd half, 3: last 15 minutes of 1st half
round(mean(Dt),2)
hdi(Dt)

# Goal Difference
Dt = beta.V[,4] - beta.V[,2] # 2: Losing by 2 goals, 4: Draw, 6: Winning by 2 goals
round(mean(Dt),2)
hdi(Dt)

# Season
Dt = beta.S[,3] - beta.S[,1] # 1: 2014-15,..., 11: 2024-25
round(mean(Dt),2)
hdi(Dt)

# Situation
round(mean(beta.H[,1]),2) # 1: Right foot
hdi(beta.H[,1])

### Interactions
# Home-attacking team
round(mean(beta.AH1[,19]),2) # 19: Man City, 10: Everton
hdi(beta.AH1[,19])

# Home-defending team
round(mean(-beta.AH2[,6]),2) # 6: Burnley, 5: Brighton
hdi(-beta.AH2[,6])

# Home-season
Dt = beta.SH[,6] - beta.SH[,7] # 1: 2014-15,..., 11: 2024-25
round(mean(Dt),2)
hdi(Dt)

