coin = c(0,1) #Assume 1 as heads and 0 as tails
gholi_turn = FALSE
gholi_heads = 0
goli_heads = 0
gholi_wins = 0 
goli_wins = 0
for (i in 1:1000000){
  flip = sample(coin,1)
  if (flip){
    if(gholi_turn){
      gholi_heads =gholi_heads + 1
    }
    else{
      goli_heads = goli_heads+1
    }
  }
  if (gholi_heads == 2){
    gholi_wins = gholi_wins+1
    gholi_heads = 0
    goli_heads = 0
  }
  else if(goli_heads==4){
    gholi_heads = 0
    goli_heads = 0
    goli_wins = goli_wins + 1
  }
  gholi_turn = !gholi_turn
}
gholi_wins
goli_wins
gholi_part = gholi_wins/(gholi_wins+goli_wins)*100
goli_part = goli_wins/(goli_wins+gholi_wins)*100
ans_1 = gholi_part
ans_2 = goli_part
save(ans_1,ans_2,file="1273168143_4.RData")
