randomization_dataset <- tibble(parm_id = c(seq(1,4,1),seq(1,4,1)),
                                linear  = c(rep("true",4), rep("false",4))
                                )
parm_ids <- sample(seq(1,8), 8, replace = F)
