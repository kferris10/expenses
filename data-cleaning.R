
library(dplyr)
library(googlesheets)
library(ggplot2)
library(ggalt)
library(lubridate)
library(plotly)

exp <- gs_title("Purchases")
# re-register
exp <- exp %>% gs_gs()

dat <- exp %>% 
  gs_read() %>% 
  select(Date, Reason, Amount, Category) %>% 
  mutate(Date = ifelse(nchar(Date) <= 6, paste(Date, "/2016"), Date), 
         Date = mdy(Date))

qplot(Date, Amount, data = dat, colour = Category)

# grouping by date
by_date <- dat %>% 
  group_by(Date, Category) %>% 
  summarise(Reason = paste(Reason, collapse = "\n"), 
            Amount = sum(Amount)) %>% 
  ungroup()

qplot(Date, Amount, data = by_date, colour = Category, geom = "line")

# aggregate by month, category, Reason
month_levels <- paste(
  rep(sort(unique(year(dat$Date))), each = 12), 
  rep(1:12, n_distinct(year(dat$Date))), 
  sep = "-"
)
by_month_category_reason <- dat %>% 
  mutate(year = year(Date), 
         month = month(Date), 
         ym = factor(paste(year, month, sep = "-"), 
                     levels = month_levels)) %>% 
  group_by(ym, Category, Reason) %>% 
  summarise(Amount = round(sum(Amount))) %>% 
  ungroup()

# aggregate by month, category
by_month_category <- by_month_category_reason %>% 
  mutate(spending = paste(Reason, Amount, sep = ": $")) %>% 
  group_by(ym, Category) %>% 
  summarise(Amount = sum(Amount), 
            Reason = paste(spending, collapse = ", ")) %>% 
  ungroup()

# calculate the total amont spent each month
by_month <- by_month_category %>% 
  group_by(ym) %>% 
  arrange(Category) %>% 
  mutate(tot_Amount = cumsum(Amount)) %>% 
  ungroup()


gg <- ggplot(by_month, aes(ym, Amount)) 
# dashed line for how much I make each month
gg <- gg + 
  geom_hline(yintercept = 1468 * 2, colour = "darkgrey", linetype = "dashed")

# points for total amount spent by category
# this allows me to create custom tooltips
gg <- gg + 
  geom_point(aes(y = tot_Amount, colour = Category, text = Reason), 
             alpha = .5) + 
  scale_colour_brewer(type = "qual", palette = 2, guide = F) + 
  scale_x_discrete(name = "Date")

# shaded area plot for expenses
gg <- gg + 
  geom_area(aes(fill = Category), alpha = .3) + 
  scale_fill_brewer(type = "qual", palette = 2, guide = F)


gg <- gg + theme_bw()
gg <- gg + theme(
  panel.grid = element_blank(), 
  panel.border = element_blank(), 
  axis.text.x = element_text(angle = 45, vjust = 0.5)
)
gg

ggplotly(gg, tooltip = c("x", "y", "colour", "text"))

# lollipop plot

gg2 <- ggplot(by_month, aes(Amount, Category)) + 
  geom_lollipop(horizontal = TRUE, point.colour = "steelblue", 
                point.size = 3, alpha = .7) + 
  facet_wrap(~ym)
gg2 <- gg2 + theme_bw()
gg2 <- gg2 + theme(panel.grid.minor = element_blank(), 
                   panel.grid.major.y = element_blank())
gg2 <- gg2 + theme(axis.line.y = element_blank(), 
                   axis.ticks.y = element_blank())
gg2 <- gg2 + theme(panel.border = element_blank())
gg2 <- gg2 + theme(strip.background = element_rect(fill = "grey80", colour = NULL))
gg2

save(dat, by_month, file = "monthly-expenses.RData")



