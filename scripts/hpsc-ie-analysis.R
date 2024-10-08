library(data.table)
library(ggplot2)
library(dplyr)
library(magrittr)
library(stringr)
library(tidyr)
library(lubridate)
library(easystats)

lines <- c(
  readLines("data/hpsc.ie/tabula-Annual_ID_Summary_Report_for_HPSC_Web_2015-2018.tsv"),
  readLines("data/hpsc.ie/tabula-Annual_ID_Summary_Report_for_HPSC_Web_v8.0-2018-2022-21032023.tsv"),
  readLines("data/hpsc.ie/tabula-Annual_ID_Summary_Report_for_HPSC_Web_v9.0-2019-2023-03102024.tsv")
)

# Table of incidence over the years {{{

# 2015-2018
t1 <- read.delim(text = lines[1:44])
colnames(t1) <- c("disease", "blank", "2015", "2016", "2017", "2018")
t1$blank <- NULL
t2 <- read.delim(text = lines[45:82])
colnames(t2) <- c("disease", "blank", "2015", "2016", "2017", "2018")
t2$blank <- NULL

# 2018-2022
t3 <- read.delim(text = lines[256:299])
colnames(t3) <- c("disease", "blank", "2018", "2019", "2020", "2021", "2022")
t3$blank <- NULL
t4 <- read.delim(text = lines[300:337])
colnames(t4) <- c("disease", "blank", "2018", "2019", "2020", "2021", "2022")
t4$blank <- NULL

# 2019-2023
t5 <- read.delim(text = lines[510:553])
colnames(t5) <- c("disease", "blank", "2019", "2020", "2021", "2022", "2023")
t5$blank <- NULL
t6 <- read.delim(text = lines[554:591])
colnames(t6) <- c("disease", "blank", "2019", "2020", "2021", "2022", "2023")
t6$blank <- NULL

# which(str_detect(lines, "2023"))
# which(str_detect(lines, "Confirmed"))

d1 <- rbindlist(list(t1, t2)) %>%
  pivot_longer(starts_with("20"), names_to = "year", values_to = "count")
d2 <- rbindlist(list(t3, t4)) %>%
  pivot_longer(starts_with("20"), names_to = "year", values_to = "count")
d3 <- rbindlist(list(t5, t6)) %>%
  pivot_longer(starts_with("20"), names_to = "year", values_to = "count")
d <- rbindlist(list(d1, d2, d3))
d %<>% group_by(disease, year) %>%
  summarize(count = sum(count))

write_tsv(d, "data/hpsc.ie/diseases-over-time.tsv")

d$year <- as.numeric(d$year)
d$date <- lubridate::as_date(d$year, format = "%Y")

my_diseases <- (
  d %>% group_by(disease) %>%
    summarize(mean = mean(count)) %>%
    filter(mean > 100) %>%
    arrange(-mean)
)$disease
my_d <- d %>% filter(disease %in% my_diseases) %>%
  mutate(disease = factor(disease, my_diseases))

my_res <- rbindlist(lapply(my_diseases, function(my_disease) {
  my_lm <- lm("count ~ year", my_d %>% filter(disease == my_disease))
  res <- parameters(my_lm)
  res$disease <- my_disease
  res
}))
my_diseases <- (
  my_res %>% filter(Parameter == "year") %>% select(Coefficient, p, disease) %>%
    arrange(-Coefficient)
)$disease
my_d <- my_d %>%
  mutate(disease = factor(disease, my_diseases))

p <- ggplot(my_d) +
  aes(x = date, y = count, group = disease) +
  #facet_grid(rows = vars(disease), scales = "free_y") +
  facet_wrap(vars(disease), scales = "free_y", ncol = 3) +
  geom_line() +
  theme(strip.text.y = element_text(angle = 0)) +
  labs(title = "Diseases over time",
       caption = "Data from hpsc.ie")
ggsave(filename = "figures/hpsc-ie-diseases.pdf", plot = p, width = 19, height = 20)

# }}}


