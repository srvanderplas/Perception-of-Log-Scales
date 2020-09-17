
# Needs to be an even number
plots_per_participant = 18

picture_details_randomization <- readr::read_csv(here("lineups-pilot-app", "plots", "picture-details.csv"))
picture_details_randomization$set <- rep(c(1,1,2,2),18) 
unique_param_values <- unique(picture_details_randomization$param_value)
block_ids <- tibble(param_id = sample(1:18,plots_per_participant/2)) %>%
             expand_grid(test_param = c("linear", "log")) %>%
             mutate(param_value = unique_param_values[param_id],
                    set  = sample(1:2,plots_per_participant, replace = T))

joinCols = c("test_param", "param_value", "set")
pic_ids <- sample(right_join(picture_details_randomization, block_ids, by = joinCols)$pic_id,plots_per_participant)
trial_pic_ids <- sample(1:10, size = 2)
