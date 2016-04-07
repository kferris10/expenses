
library(dplyr)
library(googlesheets)
library(ggplot2)
library(lubridate)
library(plotly)
library(ggiraph)
library(streamgraph)

exp <- gs_title("Purchases")
# re-register
exp <- exp %>% gs_gs()

dat <- exp %>% 
  gs_read() %>% 
  select(Date, Reason, Amount, Category) %>% 
  mutate(Date = mdy(Date)) 

qplot(Date, Amount, data = dat, colour = Category)

# grouping by date
by_date <- dat %>% 
  group_by(Date, Category) %>% 
  summarise(Reason = paste(Reason, collapse = "\n"), 
            Amount = sum(Amount)) %>% 
  ungroup()

qplot(Date, Amount, data = by_date, colour = Category, geom = "line")

# by month
by_month <- dat %>% 
  mutate(month = month(Date)) %>% 
  group_by(month, Category, Reason) %>% 
  summarise(Amount = round(sum(Amount))) %>% 
  ungroup() %>% 
  mutate(spending = paste(Reason, Amount, sep = ": $")) %>% 
  group_by(month, Category) %>% 
  summarise(Amount = sum(Amount), 
            Reason = paste(spending, collapse = "\n")) %>% 
  ungroup()

gg <- ggplot(by_month, aes(month, Amount)) 
gg <- gg + geom_area(aes(fill = Category), position = "stack")
ggplotly(gg + scale_fill_brewer(type = "qual", palette = 2))

# streamgraph
by_month %>% 
  rename(key = Category, value = Amount, date = month) %>% 
  streamgraph()