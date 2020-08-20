source("simulateData_function2.R")

betaVal = 0.27
lowVarVal = sqrt(betaVal^2*0.5)
lowVarVal
highVarVal = sqrt(betaVal^2*1.5)
highVarVal

#Identify parameters
parmCombos <- data.frame(beta  = c(0.07, 0.07, 0.15, 0.15, 0.3, 0.3),
                         sdErr = c(0.05, 0.07, 0.12, 0.18, 0.3, 0.5),
                         Curvature = c("Hard", "Hard", "Medium", "Medium", "Easy", "Easy"),
                         Variability = c("Low", "High", "Low", "High", "Low", "High")
)


#Look at visuals
sim.data.all <- data.frame("x" = NA, "y" = NA, "Curvature" = NA, "Variability" = NA)
for(k in 1:nrow(parmCombos)){
  sim.data.new <- simulate.data(simulateFunction = simulate.exponential, 
                                N = 20, 
                                nReps = 5,
                                xRange = c(1,20), 
                                
                                # Exponential Parameters
                                alpha = 1,
                                beta = parmCombos[k, "beta"], 
                                theta = 0,
                                
                                # Quadratic Parameters
                                beta0 = 0,
                                beta1 = 0, 
                                beta2 = 0.01,
                                
                                muErr = 0, 
                                sdErr = parmCombos[k, "sdErr"],
                                errorType = "mult")
  sim.data.new$Curvature <- parmCombos[k, "Curvature"]
  sim.data.new$Variability <- parmCombos[k, "Variability"]
  sim.data.all <- rbind(sim.data.all, sim.data.new)
}

sim.data.all[-1,] %>%
  mutate(Curvature = factor(Curvature, levels = c("Easy", "Medium", "Hard"))) %>%
  mutate(Variability = factor(Variability, levels = c("Low", "High"))) %>%
  ggplot(aes(x = x, y = y)) +
  geom_point(shape = 1) +
  theme_bw() +
  facet_grid(Curvature ~ Variability, scale = "free_y")

#Run lack of fit
lofStats <- data.frame("LOF" = NA, "Curvature" = NA, "Variability" = NA)
for(k in 1:nrow(parmCombos)){
  sim.data_0.1 <- replicate(n = 1000,
                            simulate.data(simulateFunction = simulate.exponential, 
                                          N = 20, 
                                          nReps = 20,
                                          xRange = c(1,20), 
                                          
                                          # Exponential Parameters
                                          alpha = 1,
                                          beta = parmCombos[k, "beta"], 
                                          theta = 0,
                                          
                                          # Quadratic Parameters
                                          beta0 = 0,
                                          beta1 = 0, 
                                          beta2 = 0.01,
                                          
                                          muErr = 0, 
                                          sdErr = parmCombos[k, "sdErr"],
                                          errorType = "mult"), 
                            simplify = FALSE)
  lofStats.new <- map(sim.data_0.1, calcLOF) %>% unlist() %>% as.matrix() %>% as.data.frame()
  colnames(lofStats.new) <- c("LOF")
  lofStats.new$Curvature <- parmCombos[k, "Curvature"]
  lofStats.new$Variability <- parmCombos[k, "Variability"]
  lofStats <- rbind(lofStats, lofStats.new)
}

# Compare Varability within Curvature
lofStats[-1,] %>%
  mutate(Curvature = factor(Curvature, levels = c("Easy", "Medium", "Hard"))) %>%
  mutate(Variability = factor(Variability, levels = c("Low", "High"))) %>%
  ggplot(aes(x = LOF, fill = Variability, color = Variability)) +
  geom_density(alpha = 0.7) +
  scale_fill_brewer(palette = "Paired") + 
  scale_color_brewer(palette = "Paired") + 
  theme_bw() +
  facet_wrap(~Curvature, scale = "free") + 
  ggtitle("Lack of Fit by Difficulty Level \n Exponential with Multiplicative Error")

# Compare Curvature within Varability
lofStats[-1,] %>%
  mutate(Curvature = factor(Curvature, levels = c("Easy", "Medium", "Hard"))) %>%
  mutate(Variability = factor(Variability, levels = c("Low", "High"))) %>%
  ggplot(aes(x = LOF, fill = Curvature, color = Curvature)) +
  geom_density(alpha = 0.7) +
  scale_fill_brewer(palette = "Paired") + 
  scale_color_brewer(palette = "Paired") + 
  theme_bw() +
  facet_wrap(~Variability, scale = "free") + 
  ggtitle("Lack of Fit by Difficulty Level \n Exponential with Multiplicative Error")
