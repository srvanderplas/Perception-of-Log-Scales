gen_exp_data <- function(coef, n = 30) {
  tibble(
    x = seq(-15, 15, length.out = n),
    y = coef * x,
    err = rnorm(n, 1, .25)
  )
}

poo <- gen_exp_data(coef = 5, n = 30) %>%
  mutate(yfix = exp(y) + err) %>%
  ggplot(aes(x = x, y = yfix)) +
  geom_point() +
  theme_bw() 
  # + scale_y_log10()

tibble(c1 = rnorm(20, 1, .05),
       c2 = c(1.3, rep(1, 19)),
       .id = 1:20,
       pos = sample(.id, 20)) %>%
  pivot_longer(cols = c("c1", "c2"), names_to = "group", values_to = "coef") %>%
  mutate(data = map(coef, gen_exp_data)) %>%
  unnest(data) %>%
  group_by(.id) %>%
  # Scaling is done by plot, not by group within plot
  mutate(yfix = exp((y - mean(y))/sd(y)) + err) %>%
  ungroup() %>%
  ggplot(aes(x = x, y = yfix, color = group)) +
  geom_point() +
  facet_wrap(~pos, ncol = 5) +
  scale_color_discrete(guide = F) +
  theme(axis.text = element_blank(), axis.title = element_blank()) + 
  scale_y_log10()
