library(dplyr)
load('final_table.RData')
View(final_table)
View(spain)
# در ابتدا تیم هایی که بیشترین و کمترین گل زده را در خانه ی حریف داشتند بررسی میکنیم.
adel <- # 
  spain%>%
  group_by(
    Season, visitor
  )%>%
  mutate(
    vgoals = sum(vgoal)
  )%>%
  arrange(vgoals)%>%
  select(c(Season,visitor,vgoals))%>%
  unique()

best_visitor = tail(adel, 1)
worst_visitor = head(adel, 1)

View(best_visitor)  # رئال مادرید فصل ۲۰۱۶ بهترین مهاجم تاریخ لالیگا در خانه ی حریف بود و بیشترین تعداد گل زده در خانه ی حریف را به ثمر رساند یعنی ۵۸ گل
View(worst_visitor) # دیپورتیو لاکرونیای فصل ۱۹۶۴ بدترین مهاجم در خانه ی حریف در تاریخ لالیگا بود و جمعا ۲ گل را در خانه ی حریف های خود به ثمر رساند
