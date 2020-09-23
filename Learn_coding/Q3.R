dice = seq(6)
gholi_wins = 0
goli_wins = 0
gholi_turn = FALSE #Assume that goli starts
for(i in 1:1000000){
  num = sample(dice,1)
  if (num == 6){
    num = sample(dice,1)
    if (gholi_turn){
      if (num==1){
        gholi_wins=gholi_wins+1
        gholi_turn = FALSE
      }
    }
    else{
      if (num==6){
        goli_wins = goli_wins+1
        gholi_turn = FALSE
      }
    }
  }
  else
    gholi_turn = !gholi_turn
}
ans_1 = gholi_wins #gholi wins in one million rolling the dice
ans_2 = goli_wins #goli wins in one million rolling the dice
save(ans_1,ans_2,file='1273168143_3.RData')
ans_1
ans_2
