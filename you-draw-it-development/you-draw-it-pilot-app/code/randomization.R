randomization_dataset <- tibble(parm_id = c(c("exp_1","exp_2","exp_3","exp_4"),c("exp_1","exp_2","exp_3","exp_4"), c("S","F","V","N")),
                                linear  = c(rep("true",4), rep("false",4), rep("true",4))
                                )
parm_ids <- sample(seq(1,12), 12, replace = F)
