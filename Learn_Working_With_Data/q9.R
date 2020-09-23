library(dplyr)
games2012 <-
  spain%>%
  filter(
    Season == 2012
  )
games2012$winner <- case_when(games2012$hgoal > games2012$vgoal ~ 'H',
                               games2012$hgoal < games2012$vgoal ~ 'V', TRUE ~ 'D')


t <-
  games2012 %>%
  group_by(home) %>%
  mutate(mp_h = n(),hgoals = sum(hgoal),h_vgoals=sum(vgoal))%>%
  filter(winner == 'H') %>%
  summarise(
    mp_h = mp_h,
    pointsh = n()*3,
    hgoals = hgoals,
    h_vgoals = h_vgoals
  )%>%
  distinct(home, .keep_all = T)
View(t)
t1 <-
  games2012%>%
  group_by(visitor)%>%
  mutate(mp_v = n(),vgoals = sum(vgoal),v_hgoals=sum(hgoal))%>%
  filter(winner == 'V')%>%
  summarise(
    pointsv = n()*3,
    vgoals = vgoals,
    mp_v = mp_v,
    v_hgoals = v_hgoals
  )%>%
  distinct(visitor, .keep_all = T)
View(t1)
t2 <-
  games2012%>%
  group_by(visitor)%>%
  filter(winner == 'D')%>%
  summarise(
    pointsdv = n()
  )
t3 <-
  games2012%>%
  group_by(home)%>%
  filter(winner == 'D')%>%
  summarise(
    pointsdh = n()
  )
tt1 =  merge(x=t,y=t1,by.x = c("home"),by.y  = c("visitor"),all = T)
tt = merge(x=t2,y=t3,by.y = c("home"),by.x  = c("visitor"),all = T)
tt = merge(x=tt,y=tt1,by.y = c("home"),by.x  = c("visitor"),all = T)
tt[is.na(tt)] = 0
View(tt)
table_2012 <- tt%>%
  summarise(
    Team = visitor,
    P = pointsdh+pointsdv+pointsh+pointsv,
    F = hgoals + vgoals,
    W = (pointsh+pointsv)/3,
    D = pointsdh+pointsdv,
    MP = mp_h+mp_v,
    L = MP - (W+D),
    A = h_vgoals+v_hgoals,
    G = F - A
  )%>%
  arrange(desc(P))
table_2012 = table_2012[,c(1,6,4,5,7,3,8,9,2)]
View(table_2012)  
