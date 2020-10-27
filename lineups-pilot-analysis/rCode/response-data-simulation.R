library(here)

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

samplingPicID <- function(participant, num_param_values = 9, param_values_per_participant = 6, num_sets = 2){
    
    picture_details_randomization <- readr::read_csv(here("lineups-pilot-app", "plots", "picture-details.csv"))
    picture_details_randomization$set <- rep(c(1,1,2,2),num_param_values) 

    # Select param values per participant
    param_details <- picture_details_randomization
    unique_param_values <- unique(param_details$param_value)
    param_block_ids <- tibble(param_id = sample(1:length(unique_param_values), param_values_per_participant)) %>%
                 mutate(param_value = unique_param_values[param_id],
                        set  = sample(1:num_sets,param_values_per_participant, replace = T)) %>%
                 expand_grid(test_param = c("linear", "log"))
    
    joinCols = c("test_param", "param_value", "set")
    pic_ids <- sample(right_join(picture_details_randomization, param_block_ids, by = joinCols)$pic_id, 2*(param_values_per_participant))
}

sampleResponse <- function(participant){
  sample(0:1,1)
}

sim_response_data <- tibble(participant = 1:5) %>%
                     mutate(pic_id = map(participant, samplingPicID)) %>%
                     unnest(pic_id) %>%
                     mutate(correct = map(participant, sampleResponse)) %>%
                     unnest(correct) %>%
                     full_join(picture_details_randomization, by = c(pic_id))
sim_response_data

