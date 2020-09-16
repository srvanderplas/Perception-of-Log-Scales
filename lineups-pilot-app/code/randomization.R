picture_details_randomization <- readr::read_csv(here("lineups-pilot-app", "plots", "picture-details.csv"))
picture_details_randomization$set <- rep(c(1,1,2,2),18) 
unique_param_values <- unique(picture_details_randomization$param_value)
block_ids <- tibble(param_id = sample(1:18,6)) %>%
             expand_grid(test_param = c("linear", "log")) %>%
             mutate(param_value = unique_param_values[param_id],
                    set  = sample(1:2,12, replace = T))

joinCols = c("test_param", "param_value", "set")
pic_ids <- sample(right_join(picture_details_randomization, block_ids, by = joinCols)$pic_id,12)
trial_pic_ids <- sample(1:10, size = 2)
