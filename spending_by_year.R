library(dplyr)
library(tidyr)
library(ggplot2)

totals <- tidied %>%
  group_by(Class, Year) %>% summarize(Total=sum(Data)) %>%
  ungroup %>%
  separate(Year, into=c("Year", "End"), remove=TRUE, convert=TRUE) %>%
  mutate(Class = factor(Class, levels=c("State Highways", "Local Roads", "Public Transport", "Other", "Walking & Cycling")))

# now adjust for CPI
cpi <- read.csv("CPI.csv", skip=1) %>%
  separate(X, into=c("Year", "Quarter"), sep="Q", remove=TRUE) %>%
  filter(All.groups != 0) %>%
  group_by(Year) %>% summarize(CPI = mean(All.groups)) %>%
  mutate(Year = as.numeric(Year))

current_cpi <- cpi %>% filter(Year == 2018) %>% pull(CPI)

totals_inflation <- totals %>% left_join(cpi, by="Year") %>% mutate(Total2018 = Total * current_cpi / CPI)

png("spending.png", width=640, height=480)
ggplot(totals_inflation, aes(x=Year, y=Total2018/1e6, col=Class)) + geom_line(size=1) +
  scale_y_continuous("", labels=scales::dollar_format(suffix="m")) +
  ggtitle("NZTA spending per year by class", "Inflation injusted by CPI to 2018 dollars") +
  scale_x_continuous(expand=c(0,0)) +
  theme_bw(base_size=13) +
  theme(legend.title=element_blank(),
        legend.position = c(0.12, 0.87),
        legend.background = element_blank())
dev.off()
