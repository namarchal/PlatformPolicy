
d <- readRDS("./df_prop.rds")

d <- d %>%
  mutate(year = year(pubDateTime), week = week(pubDateTime), day = day(pubDateTime))%>%
  mutate(neg = ifelse(score == -1, 1, 0), other = ifelse(score %in% c('1', '0'), 1, 0))%>%
  filter(!is.na(id)) #remove rows where regex failed to identify one of the three platforms

table2 <- d %>%
  group_by(id, neg)%>%
  summarize(n=n())%>%
  ungroup()%>%
  mutate(per= prop.table(n)*100)

