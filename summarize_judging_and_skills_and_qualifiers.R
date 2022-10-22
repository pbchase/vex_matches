library(tidyverse)
library(readxl)
library(janitor)
library(httr)

team_file <- "~/Downloads/RE-VIQC-21-4704-Teams-2022-02-05.xls"
teams <- readxl::read_xls(team_file) %>% janitor::clean_names()
teams %>% str()

notebook_file <- "/Users/pbchase/Downloads/WWR 2022-02-05 notebook judging.xlsx"
notebooks <- readxl::read_xlsx(notebook_file) %>% janitor::clean_names()

scratches <- c(
  "10847A",
  "10847E",
  "12710A",
  "74801D",
  "10847D",
  "10847F",
  "11029E",
  "12710B"
)

# teams %>%
#   filter(!team %in% scratches) %>%
#   select(team) %>% write_csv("interviews.csv")

interview_file <- "interviews.csv"
interviews <- read_csv(interview_file)

# get results of skills competition
skills_results_url <- "https://www.robotevents.com/eventEntities/44704/skillsReport"
tmp_skills_file <- tempfile()
httr::GET(url = skills_results_url, write_disk(tmp_skills_file))
skills_results <- readxl::read_xls(tmp_skills_file)
skills_data <- skills_results %>%
  janitor::clean_names() %>%
  pivot_wider(
    id_cols = c("rank", "team"),
    names_from = c("type"),
    values_from = c("highscore", "attempts")
  ) %>%
  mutate(highscore_combined = highscore_driver + highscore_programming) %>%
  select(rank, team, highscore_combined, everything())

get_qualifier_results_for_division <- function(base_results_url, division) {
  results_url <- paste0(base_results_url, division)
  tmp_file <- tempfile()
  httr::GET(url = results_url, write_disk(tmp_file))
  results <- readxl::read_xls(tmp_file)
  data <-
    results %>%
    janitor::clean_names() %>%
    rename_with(., ~ gsub("_[0-9]+", "", .x), contains("_team_")) %>%
    pivot_longer(
      cols = c(red_team, blue_team),
      names_to = c("team_color"),
      values_to = c("team_number")
    ) %>%
    pivot_longer(
      cols = c(red_score, blue_score),
      values_to = c("team_score")
    ) %>%
    mutate(team_color = gsub("_team", "", team_color)) %>%
    mutate(name = gsub("_score", "", name)) %>%
    filter(team_color == name) %>%
    mutate(division = division) %>%
    select(division, match, team_color, team_number, team_score)

  return(data)
}

qualifier_results <- tribble(
  ~division, ~match, ~team_color, ~team_number, ~team_score,
  1, "a", "b", "c", 99
) %>% filter(F)

base_results_url <- "https://www.robotevents.com/eventEntities/44704/resultsReport/"

for (division in seq.int(1, 2)) {
  qualifier_results <- bind_rows(qualifier_results, get_qualifier_results_for_division(base_results_url, division = division))
  }

ranked_qualifer_results <- qualifier_results %>%
  group_by(team_number) %>%
  summarize(average_score = mean(team_score)) %>%
  arrange(desc(average_score)) %>%
  mutate(rank = row_number())

qualifier_results %>% filter(team_number == "94537X")

