people16 = c('honest')
for(i in 2:16){
  print(i)
  if (i==2 | i==16){
    people16 = append(people16,'liar')
    next
  }
  if (people16[i-1]=='honest')
    people16[i]='liar'
  else
    people16[i]='honest'
}
people16
people12 =c('honest') #starting with an honest person
people12 = append(people12,'honest') #next one is honest (so the last one should be liar)
print(people12)
for (i in 3:12){
  if (i==12){
    people12 = append(people12, 'liar')
    break
  }
  if (people12[i-1] == "honest"){
    if (people12[i-2]=='liar')
      people12 = append(people12,'honest')
    else
      people12 = append(people12,'liar')
  }
  else{
    people12 = append(people12,people12[i-2])
  }
}
print(people12)
ans_1 = people16
ans_2 = people12
save(ans_1,ans_2,file='1273168143_2.RData')
ans_1
ans_2
