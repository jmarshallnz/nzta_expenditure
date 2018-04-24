library(dplyr)
library(tidyr)
library(readxl)

nzta <- read_excel("All Expenditure_tidied.xlsx")

# hacky function to save some typing
contains <- function(x, vals) {
  ors <- lapply(vals, function(y) { grepl(y, x, ignore.case=TRUE) })
  Reduce(`|`, ors)
}

tidied <- nzta %>% mutate(AC2 = sub(".*[0-9] \\- (.*)", "\\1", AC),
                Roads = contains(AC2, "road") & !contains(AC2, "alternatives"),
                Highways = contains(AC2, "highway"),
                Transit = contains(AC2, c("passenger", "public", "supergold")),
                WalkCycle = contains(AC2, c("walking", "cycling")),
                Class = case_when(Highways ~ "State Highways",
                                  Roads ~ "Local Roads",
                                  Transit ~ "Public Transport",
                                  WalkCycle ~ "Walking & Cycling",
                                  TRUE ~ "Other")) %>%
  select(-Roads, -Highways, -Transit, -WalkCycle, -AC2)

# sanity check
tidied %>% select(AC, Class) %>% table

# write tidied file
write.csv(tidied, "nzta_expenditure_tidied.csv", row.names=FALSE)
