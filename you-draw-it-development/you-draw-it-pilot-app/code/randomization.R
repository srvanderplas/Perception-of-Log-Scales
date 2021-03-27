randomization_dataset <- tibble(parm_id = c(seq(1,4,1),seq(1,4,1), c("S","F","V","N")),
                                linear  = c(rep("true",4), rep("false",4), rep("true",4))
                                )
parm_ids <- sample(seq(1,12), 12, replace = F)
