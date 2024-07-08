# Analyses commits from R Core team based on
# https://www.r-bloggers.com/2018/03/guide-to-tidy-git-analysis/

# Parts 1 and 2
library(tidyverse)
library(glue)
library(stringr)
library(forcats)
# Part 3
library(tidygraph)
library(ggraph)
library(tidytext)
library(warwickplots)

# Set path to local clone of https://github.com/r-devel/r-svn
# (not your fork of it!)
repo <- "~/Repos/r-devel/r-svn"

## make sure up-to-date
system(glue('git -C {repo} pull'))

system(glue('git -C {repo} log -3'))

log_format_options <- c(datetime = "cd", commit = "h", parents = "p",
                        author = "an", subject = "s")
option_delim <- "\t"
log_format   <- paste(glue("%{log_format_options}"), collapse = "\t")
log_options  <- glue('--pretty=format:"{log_format}" --date=format:"%Y-%m-%d %H:%M:%S"')
log_cmd      <- glue('git -C {repo} log {log_options}')
log_cmd

system(glue('{log_cmd} -3'))

history_logs <- system(log_cmd, intern = TRUE) %>%
    str_split_fixed(option_delim, length(log_format_options)) %>%
    as_tibble(.name_repair = "minimal") %>%
    setNames(names(log_format_options))

history_logs <- history_logs %>%
    mutate(author = case_when(
        str_detect(tolower(author), "thomas") ~ "tlumley",
        str_detect(tolower(author), "paul") ~ "murrell",
        str_detect(tolower(author), "martyn") ~ "plummer",
        str_detect(tolower(author), "ltierney") ~ "luke",
        TRUE ~ author
    ))

r_core <- tribble(
  ~name, ~username,
  "Douglas Bates",      "bates", # up to March 2024
  "John Chambers",      "jmc",
  "Peter Dalgaard",     "pd",
  "Robert Gentleman",   "rgentlem",
  "Kurt Hornik",        "hornik",
  "Ross Ihaka",         "ihaka",
  "Tomas Kalibera",     "kalibera",
  "Michael Lawrence",   "lawrence",
  "Friedrich Leisch",   "leisch", # up to April 2024
  "Uwe Ligges",         "ligges",
  "Thomas Lumley",      "tlumley", #thomas
  "Martin Maechler",    "maechler",
  "Sebastian Meyer",    "smeyer",
  "Paul Murrell",       "murrell", #paul
  "Martyn Plummer",     "plummer", #martyn
  "Brian Ripley",       "ripley",
  "Deepayan Sarkar",    "deepayan",
  "Duncan Temple Lang", "duncan",
  "Luke Tierney",       "luke", #ltierney
  "Simon Urbanek",      "urbaneks",
  "Heiner Schwarte",    "", # up to October 1999,
  "Guido Masarotto",    "guido", # up to June 2003,
  "Stefano Iacus",      "iacus", # up to July 2014,
  "Seth Falcon",        "falcon",# up to August 2015,
  "Duncan Murdoch",     "murdoch", # up to September 2017
  "Martin Morgan",      "morgan") # up to June 2021.

history_logs <- left_join(history_logs, r_core, by = c("author" = "username"))

# check
table(history_logs[is.na(history_logs$name),"author"])

history_logs$year <- substr(history_logs$datetime, 1, 4)

dat <- history_logs %>%
    count(name, year)

ggplot(history_logs, aes(x = year, fill = name)) +
    geom_bar(position = "stack")

dat2 <- history_logs %>%
    group_by(year) %>%
    summarize(n = n_distinct(name))


ggplot(dat2, aes(x = year, y = n)) +
    geom_bar(stat = "identity")

xtabs(~ name + year, data = history_logs)

dat3 <- history_logs %>%
    filter(!is.na(name)) %>%
    mutate(name = fct_infreq(name)) %>%
    group_by(name, year) %>%
    summarise(log_commits = log10(n()),
              commits = 10^log10(n()))

year_seq <- seq(from = 1997, to = 2024, by = 2)

dark_text <- "#2e2e2f"
mid_text <-  "#4d4e4f"
light_text <- "#747576"
pale_text <- "#ebebeb"

#option = "plasma"

ggplot(dat3, aes(year, fct_rev(name))) +
    geom_tile(aes(fill = commits)) +
    labs(x = NULL, y = NULL, title = NULL) +
    scale_fill_viridis_b(option = "plasma", trans = "log10", limits=c(0.1,5000),
                      breaks = c(0.1, 1, 10, 100, 1000), labels = c("", "1", "10", "100", "1000"),
                      direction = -1) +
    scale_x_discrete(breaks = year_seq, labels = year_seq) +
  #scale_y_discrete(expand = c(0, 0.4)) +  
  warwickplots:::theme_warwick(base_size = 20) +
    theme(axis.text.x = element_text(size = rel(1)),
          axis.text.y = element_text(colour = dark_text),
          legend.position = "right")

ggsave("r_core_commits.png", path = here::here("figures"), device = "png", dpi = 320,
       height = 8.5, width = 13, units = "in")
ggsave("r_core_commits.svg", path = here::here("figures"), device = grDevices::svg())
