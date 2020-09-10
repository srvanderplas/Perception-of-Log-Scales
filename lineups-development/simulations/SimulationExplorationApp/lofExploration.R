source("simulateData_function.R")

#Identify parameters
parmCombos <- data.frame(beta = c(0.01, 0.05, 0.1),
                         sdErr = c(0.005, 0.05, 0.19),
                         Difficulty = c("Hard", "Medium", "Easy"))


#Look at visuals
sim.data.all <- data.frame("x" = NA, "y" = NA, "Difficulty" = NA)
for(k in 1:nrow(parmCombos)){
  sim.data.new <- simulate.data(simulateFunction = simulate.exponential, 
                                N = 20, 
                                nReps = 1,
                                xRange = c(1,30), 
                                
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
  sim.data.new$Difficulty <- parmCombos[k, "Difficulty"]
  sim.data.all <- rbind(sim.data.all, sim.data.new)
}

sim.data.all[-1,] %>%
  mutate(Difficulty = factor(Difficulty, levels = c("Easy", "Medium", "Hard"))) %>%
  ggplot(aes(x = x, y = y)) +
  geom_point(shape = 1) +
  theme_bw() +
  facet_wrap(~ Difficulty, scale = "free_y")

#Run lack of fit
lofStats <- data.frame("LOF" = NA, "Difficulty" = NA)
for(k in 1:nrow(parmCombos)){
  sim.data_0.1 <- replicate(n = 1000,
                            simulate.data(simulateFunction = simulate.exponential, 
                                          N = 20, 
                                          nReps = 5,
                                          xRange = c(1,30), 
                                          
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
  lofStats.new$Difficulty <- parmCombos[k, "Difficulty"]
  lofStats <- rbind(lofStats, lofStats.new)
}

lofStats[-1,] %>%
  mutate(Difficulty = factor(Difficulty, levels = c("Easy", "Medium", "Hard"))) %>%
  ggplot(aes(x = LOF, fill = Difficulty, color = Difficulty)) +
  geom_density(alpha = 0.7) +
  scale_fill_brewer(palette = "Paired") + 
  scale_color_brewer(palette = "Paired") + 
  theme_bw() +
  ggtitle("Lack of Fit by Difficulty Level \n Exponential with Multiplicative Error")
