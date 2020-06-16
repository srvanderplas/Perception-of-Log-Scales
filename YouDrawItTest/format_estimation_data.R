mem_path <- here::here("data", "mem-disk-price.xlsx")
if (!file.exists(mem_path)) download.file("https://jcmit.net/MemDiskPrice-xl95.xls", mem_path, mode = "wb")

cnames <- c("dec_date", "price_per_mb", "year", "md", "ref1", "ref2", "ref3", "size_kb", "price", "speed", "memtype")
ctype <- c(rep("numeric", 3), rep("text", 4), rep("numeric", 2), rep("text", 2), rep("skip", 3))
mem <- read_xls(mem_path, sheet = "MEMORY", skip = 4, col_names= cnames, col_types = ctype)
mem$mb_per_dollar <- 1/mem$price_per_mb

df <- select(mem, dec_date, value = mb_per_dollar) %>%
  mutate(type = "Memory", unit = "MB per $1 spent", scenario = 1)

cnames <- c("obs", "dec_date", "old_hd_price_mb", "floppy_price_mb", "hd_price_mb", "year", "md", "ref1", "ref2", "sales", "manuf", "model", "size", "type", "speed", "rotate", "cache", "size_mb", "cost")
ctype <- c(rep("numeric", 5), "skip", "skip", "numeric", rep("text", 8), rep("numeric", 2), "text", "numeric", "numeric")
hd <- read_xls(mem_path, sheet = "DDRIVES", skip = 4, col_names= cnames, col_types = ctype)  %>%
  pivot_longer(old_hd_price_mb:hd_price_mb, names_to = "disk_type", values_to = "price_per_mb") %>%
  mutate(disk_type = str_remove(disk_type, "_price_mb")) %>%
  filter(dec_date >= 1980, !is.na(price_per_mb)) %>%
  # filter(disk_type == "hd") %>%
  mutate(mb_per_dollar = 1/price_per_mb) %>%
  select(dec_date, year, md, size_mb, cost, disk_type, price_per_mb, mb_per_dollar)

df <- bind_rows(
  df,
  select(hd, dec_date, value = mb_per_dollar) %>%
    mutate(type = "Hard Drive Capacity", unit = "MB per $1 spent", scenario = 2)
)

url <- "https://www.top500.org/lists/"

data_file <- here::here("data/supercomputers.Rdata")
if (!file.exists(data_file)) {
  get_table_dat <- function(url2) {
    tmp <- read_html(url2) %>%
      html_table() %>%
      unlist(recursive = F) %>%
      as_tibble() %>%
      set_names(
        names(.) %>%
          str_to_lower() %>%
          str_replace_all("[ [:punct:]]{1,}", " ") %>%
          str_trim() %>%
          str_replace_all(" ", "_")
      )
    # set_names(c("rank", "site", "system", "cores", "rmax_tflops_s", "rpeak_tflops_s", "power_kw"))

    tmp %>%
      mutate(across(matches("cores|rmax|rpeak|power"), ~(if(is.character(.)) {parse_number(.)} else {.})))
  }

  list_links <- read_html(url) %>%
    xml_nodes("#squarelist li a") %>%
    purrr::map_df(., ~tibble(link = xml_attr(., "href"), date = str_replace(link, ".*(\\d{4})/(\\d{2})", "\\1 \\2 01") %>% ymd())) %>%
    mutate(link = str_replace(link, "lists", "list") %>%
             paste("https://www.top500.org", ., sep = "")) %>%
    mutate(data = purrr::map(link, get_table_dat))

  computing_power <- list_links %>%
    select(date, data) %>%
    unnest(data) %>%
    mutate(rank = factor(rank, ordered = T)) %>%
    mutate(rmax_tflop_s = ifelse(is.na(rmax_tflop_s), rmax_gflop_s / 1000, rmax_tflop_s),
           rpeak_tflop_s = ifelse(is.na(rpeak_tflop_s), rpeak_gflop_s / 1000, rpeak_tflop_s))

  save(list_links, computing_power, file = data_file)
}
load(data_file)

df <- computing_power %>%
  mutate(dec_date = decimal_date(date)) %>%
  select(dec_date, rank, cores, rmax_tflop_s, rpeak_tflop_s) %>%
  pivot_longer(cores:rpeak_tflop_s, names_to = "var", values_to = "value") %>%
  filter(rank <= 3) %>%
  mutate(type = str_replace_all(var, c("cores" = "Computer Cores",
                                       "rmax_tflop_s" = "Sustained Calculation Speed",
                                       "rpeak_tflop_s" = "Peak Calculation Speed")),
         unit = str_replace_all(var, c("cores" = "Cores", ".*_tflop_s" = "TFlop/sec")),
         scenario = as.numeric(factor(var)) + 2) %>%
  select(-var) %>%
  bind_rows(df, .) %>%
  nest(data = c(-scenario, -type, -unit))

rm(mem_path, url, data_file, computing_power, list_links, mem, hd, cnames, ctype)

ans <- df %>%
  unnest(data) %>%
  mutate(year = floor(dec_date)) %>%
  mutate(rank = ifelse(is.na(rank), 1, rank)) %>%
  filter(rank == 1) %>%
  right_join(tibble(year = c(2020, 2000, 2020, 2000, 2019, 2000, 2019, 2000, 2019, 2000), scenario = rep(1:5, each = 2))) %>%
  group_by(scenario, year) %>%
  summarize(value = mean(value)) %>%
  mutate(year = ifelse(round(year/20) == 101, 2020, 2000)) %>%
  mutate(year = paste("q", year, sep="")) %>%
  pivot_wider(names_from = year, values_from = value) %>%
  mutate(q1 = q2020, q2 = q2020/q2000) %>%
  select(-q2000, -q2020) %>%
  pivot_longer(q1:q2, values_to = "answer", names_to = "qnum") %>%
  mutate(qnum = str_remove(qnum, "q") %>% as.numeric())
qs <- tribble(~q, ~qnum, ~scenario, ~lb, ~ub, ~delta,
  "How much memory could you purchase with $100 in 2020?\nEnter your estimate here (in MB)", 1, 1, 0, 5e4, 100,
  "In 2020, you could buy approximately ___ times the memory as in 2000 (for the same money).", 2, 1, 1, 500, 10,
  "How large of a hard drive could you purchase with $100 in 2020?\nEnter your estimate here (in MB)", 1, 2, 3e4, 8e4, 1000,
  "In 2020, you could buy approximately ___ times the hard drive space as in 2000 (for the same money).", 2, 2, 1, 700, 10,
  "How many cores would you expect in a Top-3 supercomputer in 2020, on average?", 1, 3, 5e5, 1e7, 1e5,
  "How many more cores were in a Top-3 supercomputer in 2020 than in 2000, on average?", 2, 3, 1, 500, 10,
  "What sustained calculation speed would you expect in a Top-3 supercomputer in 2020, on average?", 1, 4, 1e4, 1e6, 1e4,
  "How much faster are sustained calculations in a Top-3 supercomputer in 2020 than in 2000, on average?", 2, 4, 1e3, 1e5, 1e3,
  "What peak calculation speed would you expect in a Top-3 supercomputer in 2020, on average?", 1, 5, 1e4, 1e6, 1e4,
  "How much faster are peak calculations in a Top-3 supercomputer in 2020 than in 2000, on average?", 2, 5, 1e3, 1e5, 1e3
) %>% left_join(ans)
