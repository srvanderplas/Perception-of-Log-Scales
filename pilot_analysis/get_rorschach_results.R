# Get evaluations of null lineups out of the set

tbl(con, "picture_details") %>%
  collect() %>%
  filter(str_detect(param_value, "r1$")) %>%
  # add in participant responses
  left_join(tbl(con, "feedback") %>% collect()) %>%
  # Count up number of panel selections for each plot
  group_by(pic_id, data_name, param_value, test_param, response_no) %>%
  count() %>%
  filter(!is.na(response_no)) %>%
  # Count up number of evaluations for each panel
  group_by(pic_id, data_name, param_value, test_param) %>%
  mutate(total_evals = sum(n)) %>%
  filter(total_evals > 3) %>%
  write_csv("pilot_analysis/data/rorschach-eval-graphics-group.csv")
