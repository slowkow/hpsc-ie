library(data.table)
library(ggplot2)
library(dplyr)
library(magrittr)
library(stringr)
library(tidyr)
library(readr)
library(lubridate)
library(easystats)
library(scales)


# Table of incidence over the years {{{

# 1982-2002
lines <- readLines("data-raw/hpsc.ie/tabula-File,687,en.tsv")
t1 <- read.delim(text = lines[1:41])
t1$Infectious.Disease[6] <- "Bacterial Meningitis	(including meningococcal septicaemia)"
t1$Infectious.Disease[13] <- "Food Poisoning (bacterial other than salmonella)"
t1$Infectious.Disease[15] <- "Gastroenteritis (when contracted by children under 2 years)"
t1$Infectious.Disease[28] <- "Salmonellosis (other than typhoid or paratyphoid)"
t1 <- t1[-c(7,14,16,29),]
colnames(t1)[1] <- "disease"
t1[t1 == "NN"] <- NA
colnames(t1)[2:ncol(t1)] <- str_remove(colnames(t1)[2:ncol(t1)], "^X")
for (my_col in colnames(t1)) {
  t1[[my_col]] <- as.character(t1[[my_col]])
}
d01 <- t1 %>%
  pivot_longer(-c("disease"), names_to = "year", values_to = "count")
d01$count <- parse_number(d01$count)

# 2004-2014
lines <- readLines("data-raw/hpsc.ie/tabula-File,2393,en.tsv")
t2 <- read.delim(text = lines[1:41])
t2[9,1] <- "Carbapenem-resistant Enterobacteriaceae infection (invasive)"
t2 <- t2[-c(8),]
colnames(t2)[1] <- c("disease")
colnames(t2)[2:ncol(t2)] <- str_remove(colnames(t2)[2:ncol(t2)], "^X")
t3 <- read.delim(text = lines[42:78])
colnames(t3)[1] <- c("disease")
colnames(t3)[2:ncol(t3)] <- str_remove(colnames(t3)[2:ncol(t3)], "^X")
d02 <- rbindlist(list(t2, t3)) %>%
  pivot_longer(-c("disease"), names_to = "year", values_to = "count")


lines <- c(
  readLines("data-raw/hpsc.ie/tabula-Annual_ID_Summary_Report_for_HPSC_Web_2015-2018.tsv"),
  readLines("data-raw/hpsc.ie/tabula-Annual_ID_Summary_Report_for_HPSC_Web_v8.0-2018-2022-21032023.tsv"),
  readLines("data-raw/hpsc.ie/tabula-Annual_ID_Summary_Report_for_HPSC_Web_v9.0-2019-2023-03102024.tsv")
)

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

d <- rbindlist(list(d01, d02, d1, d2, d3))
d$disease <- str_remove(d$disease, "\\*\\*")
d$disease <- str_remove(d$disease, "\\*\\^")
d$disease <- str_remove(d$disease, "\\*\\!")
d$disease <- str_remove(d$disease, "\\*")
d$disease[d$disease == "Acute Anterior Poliomyelitis"] <- "Acute anterior poliomyelitis"
d$disease[d$disease == "Acute Encephalitis"] <- "Viral encephalitis"
d$disease[d$disease == "Acute Viral Meningitis"] <- "Viral meningitis"
d$disease[d$disease == "Bacillary Dysentery (Shigellosis)"] <- "Bacillus cereus food-borne infection or intoxication"
d$disease[d$disease == "Creutzfeldt Jakob disease"] <- "Creutzfeldt Jakob Disease"
d$disease[d$disease == "vCreutzfeldt Jakob Disease"] <- "Creutzfeldt Jakob Disease"
d$disease[d$disease == "mpox"] <- "Mpox infection"
d$disease[d$disease == "Influenza (seasonal & pandemic)"] <- "Influenza"
d$disease[d$disease == "Influenzal Pneumonia"] <- "Influenza"
d$disease[d$disease == "Viral Hepatitis Type B"] <- "Hepatitis B (acute and chronic)"
d$disease[d$disease == "Whooping Cough"] <- "Pertussis"
d$disease <- str_replace_all(d$disease, "\t", " ")
d %<>% group_by(disease, year) %>%
  summarize(count = sum(count)) %>%
  ungroup()

# d %>% count(disease) %>% filter(n < 41) %>% View()

write_tsv(d, "data/diseases-over-time.tsv")

# }}}

# Plot diseases over time {{{

d$year <- as.numeric(d$year)
d$date <- lubridate::as_date(as.character(d$year), format = "%Y")

my_diseases <- (
  d %>% group_by(disease) %>%
    summarize(total = sum(count)) %>%
    filter(total > 1000) %>%
    arrange(-total)
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
  facet_wrap(vars(disease), scales = "free_y", ncol = 5) +
  geom_line() +
  scale_x_date(date_labels = "%Y") +
  scale_y_continuous(breaks = pretty_breaks(2)) +
  theme(strip.text.y = element_text(angle = 0)) +
  labs(title = "Diseases over time",
       caption = "Data from hpsc.ie")
ggsave(filename = "figures/hpsc-ie-diseases.pdf", plot = p, width = 35, height = 20)

# }}}


