library(ggplot2)
library(reshape2)
library(patchwork)
library(plotly)
library(ggdendro)
library(Matrix)
library(expm)
library(pROC)
detach(package:pROC,unload=TRUE)

Beta = readRDS("output/Beta1.rds")

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

beta.A1 = cbind(-rowSums(Beta[,(m[6]+1):m[7]]), Beta[,(m[6]+1):m[7]])
beta.A2 = -cbind(-rowSums(Beta[,(m[7]+1):m[8]]), Beta[,(m[7]+1):m[8]])
beta.K = -cbind(-rowSums(Beta[,(m[8]+1):m[9]]), Beta[,(m[8]+1):m[9]])

beta.B = cbind(Beta[,(m[9]+1):m[10]], -Beta[,(m[9]+1):m[10]])
beta.G = cbind(Beta[,(m[10]+1):m[11]], -Beta[,(m[10]+1):m[11]])
beta.Ti = cbind(-rowSums(Beta[,(m[11]+1):m[12]]), Beta[,(m[11]+1):m[12]])
beta.V = cbind(-rowSums(Beta[,(m[12]+1):m[13]]), Beta[,(m[12]+1):m[13]])
beta.S = cbind(-rowSums(Beta[,(m[13]+1):m[14]]), Beta[,(m[13]+1):m[14]])

beta.SH = cbind(-rowSums(Beta[,(m[14]+1):m[15]]), Beta[,(m[14]+1):m[15]])
beta.AH1 = cbind(-rowSums(Beta[,(m[15]+1):m[16]]), Beta[,(m[15]+1):m[16]])
beta.AH2 = -cbind(-rowSums(Beta[,(m[16]+1):m[17]]), Beta[,(m[16]+1):m[17]])

beta.H = cbind(Beta[,(m[17]+1):m[18]], -Beta[,(m[17]+1):m[18]])
#beta.Fs = Beta[,(m[18]+1):m[19]]


library(ggplot2)
library(reshape2)
library(patchwork)
library(bayestestR)
library(pROC)
detach(package:pROC,unload=TRUE)

mean_quantile_ci <- function(x) {
  qs <- quantile(x, probs = c(0.025, 0.975))
  data.frame(y = mean(x), ymin = qs[1], ymax = qs[2])
}

# 1. Home Effect
my_vector <- beta.B[,2]
CI = ci(my_vector, method = "HDI")
mu = mean(my_vector)
  
df <- data.frame(Value = my_vector)

P1 = ggplot(df, aes(x = Value)) +
  geom_histogram(aes(y = after_stat(count/ sum(count))),
                 binwidth = 0.01,      # Adjust binwidth as needed
                 fill = "grey",
                 color = "#999999",
                 alpha = 0.7) +
  geom_density(aes(y = after_stat(density * 0.01)), fill = "lightgrey",
               color = "#999999",
               alpha = 0.5) +
  geom_vline(xintercept = c(0,mu,CI$CI_low, CI$CI_high), linetype = c(2,2,4,4), color = c("black", "#999", "#999", "#999")) + 
  labs(title = "(D)",
       x = "Home Effect",
       y = "Density") +
  theme_classic() 

# 2. Situation
my_vector <- beta.G[,1]
mu = mean(my_vector)
CI = ci(my_vector, method = "HDI")

df <- data.frame(Value = my_vector)

P2 = ggplot(df, aes(x = Value)) +
  geom_histogram(aes(y = after_stat(count/ sum(count))),
                 binwidth = 0.01,
                 fill = "grey",
                 color = "#999999",
                 alpha = 0.7) +
  geom_density(aes(y = after_stat(density * 0.01)), fill = "lightgrey",
               color = "#999999",
               alpha = 0.5) +
  geom_vline(xintercept = c(0,mu,CI$CI_low, CI$CI_high), linetype = c(2,2,4,4), color = c("black", "#999", "#999", "#999")) + 
  labs(title = "(E)",
       x = "Situation Effect (Open Play)",
       y = "Density") +
  theme_classic() #+ ylim(c(0, 0.1))

# 3. Shot type
my_vector <- beta.H[,1]
mu = mean(my_vector)
CI = ci(my_vector, method = "HDI")

df <- data.frame(Value = my_vector)

P3 = ggplot(df, aes(x = Value)) +
  geom_histogram(aes(y = after_stat(count/ sum(count))),
                 binwidth = 0.01,      
                 fill = "grey",
                 color = "#999999",
                 alpha = 0.7) +
  geom_density(aes(y = after_stat(density * 0.01)), fill = "lightgrey",
               color = "#999999",
               alpha = 0.5) +
  geom_vline(xintercept = c(0,mu,CI$CI_low, CI$CI_high), linetype = c(2,2,4,4), color = c("black", "#999", "#999", "#999")) + 
  labs(title = "(I)",
       x = "Shot Type Effect (Right Foot)",
       y = "Density") +
  theme_classic() 

# 4. Time Effect
mat <- as.matrix(beta.Ti)
colnames(mat) <- c("T1","T2","T3","T4","T5","T6")

df <- melt(mat)
colnames(df) <- c("Observation", "Variable", "Value")

P4 = ggplot(df, aes(x = Variable, y = Value)) +
  geom_violin(fill = "lightgray", color = "#999999", linewidth = 0.25) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.25) +
  stat_summary(fun.data = mean_quantile_ci,
               geom = "errorbar", width = 0.25, color = "#999999", size = 0.25) +
  stat_summary(fun = mean, geom = "point", color = "#999999", size = 0.25) +
  theme_minimal() +
  labs(title = "(F)",
       x = "Time Interval",
       y = "") +
  theme_classic() +
  scale_x_discrete(labels = c("T1" = "<15","T2" = "16-30",
                              "T3" = "31-45","T4" = "46-60",
                              "T5" = "60-75","T6" = "75<"))+
  theme(axis.text.x = element_text(size = 7))

# 5. Game Advantage
mat <- as.matrix(beta.V)
colnames(mat) <- c("-2-", "-2", "-1", "0", "1", "2", "2+")

df <- melt(mat)
colnames(df) <- c("Observation", "Variable", "Value")

P5 = ggplot(df, aes(x = Variable, y = Value)) +
  geom_violin(fill = "lightgray", color = "#999999", linewidth = 0.25) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.25) +
  stat_summary(fun.data = mean_quantile_ci,
               geom = "errorbar", width = 0.25, color = "#999999", size = 0.25) +
  stat_summary(fun = mean, geom = "point", color = "#999999", size = 0.25) +
  theme_minimal() +
  labs(title = "(G)",
       x = "Game Advantage",
       y = "") +
  theme_classic() +
  theme(axis.text.x = element_text(size = 7))

# 6. Def Formation
mat <- as.matrix(beta.K)
colnames(mat) <- c("T1","T2","T3","T4","T5","T6","T7")

df <- melt(mat)
colnames(df) <- c("Observation", "Variable", "Value")

P6 = ggplot(df, aes(x = Variable, y = Value)) +
  geom_violin(fill = "lightgray", color = "#999999", linewidth = 0.25) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.25) +
  stat_summary(fun.data = mean_quantile_ci,
               geom = "errorbar", width = 0.25, color = "#999999", size = 0.25) +
  stat_summary(fun = mean, geom = "point", color = "#999999", size = 0.25) +
  theme_minimal() +
  labs(title = "(A)",
       x = "Defensive Formations",
       y = "") +
  theme_classic() +
  scale_x_discrete(labels = c("T1" = "4-2-3-1","T2" = "4-3-3",
                              "T3" = "4-4-2","T4" = "4-1-4-1",
                              "T5" = "3-4-2-1","T6" =  "3-5-2",
                              "T7" = "Others")) +
  theme(axis.text.x = element_text(size = 7))

# 7. Season Effet
mat <- as.matrix(beta.S)
colnames(mat) <- c("S2014","S2015","S2016","S2017","S2018","S2019","S2020",
                   "S2021","S2022","S2023","S2024")

df <- melt(mat)
colnames(df) <- c("Observation", "Variable", "Value")

P7 = ggplot(df, aes(x = Variable, y = Value)) +
  geom_violin(fill = "lightgray", color = "#999999", linewidth = 0.25) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.25) +
  stat_summary(fun.data = mean_quantile_ci,
               geom = "errorbar", width = 0.25, color = "#999999", size = 0.25) +
  stat_summary(fun = mean, geom = "point", color = "#999999", size = 0.25) +
  theme_minimal() +
  labs(title = "(H)",
       x = "Seasons",
       y = "Estimate") +
  theme_classic() +
  scale_x_discrete(labels = c("S2014" = 14, "S2015" = 15,
                              "S2016" = 16, "S2017" = 17,
                              "S2018" = 18, "S2019" = 19,
                              "S2020" = 20, "S2021" = 21,
                              "S2022" = 22, "S2023" = 23,
                              "S2024" = 24))

# 8. Position Effect

mat <- as.matrix(beta.P)
colnames(mat) <- c("DEF","DM","MID","AM","FWR")

df <- melt(mat)
colnames(df) <- c("Observation", "Variable", "Value")

P8 = ggplot(df, aes(x = Variable, y = Value)) +
  geom_violin(fill = "lightgray", color = "#999999", linewidth = 0.25) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.25) +
  stat_summary(fun.data = mean_quantile_ci,
               geom = "errorbar", width = 0.25, color = "#999999", size = 0.25) +
  stat_summary(fun = mean, geom = "point", color = "#999999", size = 0.25) +
  theme_minimal() +
  labs(title = "(B)",
       x = "Positions",
       y = "Estimate") +
  theme_classic()

# 9. Players Effect
beta.Pr = cbind(beta.P1+beta.P[,1],
  beta.P2+beta.P[,2],
  beta.P3+beta.P[,3],
  beta.P4+beta.P[,4],
  beta.P5+beta.P[,5])

est.Pr = colMeans(beta.Pr)
beta.Pr.new = beta.Pr[,order(est.Pr, decreasing = T)][,1:5]

mat <- as.matrix(beta.Pr.new)
colnames(mat) <- c("Son", "Hazard", "Kane", "Zaha", "Martial")

df <- melt(mat)
colnames(df) <- c("Observation", "Variable", "Value")

P9 = ggplot(df, aes(x = Variable, y = Value)) +
  geom_violin(fill = "lightgray", color = "#999999", linewidth = 0.25) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.25) +
  stat_summary(fun.data = mean_quantile_ci,
               geom = "errorbar", width = 0.25, color = "#999999", size = 0.25) +
  stat_summary(fun = mean, geom = "point", color = "#999999", size = 0.25) +
  theme_minimal() +
  labs(title = "(C)",
       x = " ",
       y = "Estimate") +
  theme_classic()


layout <- "
AAABBBCCC
DDDEEEFFF
GGGHHHIII
"

p = P6+P8+P9+P1+P2+P4+P5+P7+P3 + plot_layout(design = layout)

p

# ggsave("P2_5774.png",
#        
#        plot = p,
#        
#        width = 10,
#        
#        height = 6,
#        
#        units = "in",
#        
#        dpi = 300)
