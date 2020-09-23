  eq1 <- function(a1,a2,a3,a4,a5){
    e = 0
    f = a1/(a2+a3)
    s = (a2+a3)/(a3+a4+a5)
    if (f!=s){
      e=e+1
    }
    if(f>=1){
      e=e+1
    }
    if (s>=1)
      e=e+1
    return(e)
  }
  eq2 <- function(a5,a6,a7,a8,a9,a10){
    e=0
    f = a6/(a6+a7)
    s = (a6+a7)/(a7+a8+a9)
    t = (a7+a8+a9)/(a5+a9+a10)
    if (f!=s){
      e=e+1
    }
    if (s!=t)
      e=e+1
    if (f!=t)
      e=e+1
    if(f>=1){
      e=e+1
    }
    if (s>=1)
      e=e+1
    if (t>=1)
      e = e+1
    return(e)
  }
  
  eq3 <- function(a10,a11,a12,a13){
    e=0
    f = (a11+a12)/(a12+a13)
    s = (a12+a13)/(a13+a10)
    if (f!=s){
      e=e+1
    }
    if(f>=1){
      e=e+1
    }
    if (s>=1)
      e=e+1
    return(e)
  }
  solve1<-function(a5){
    domain=seq(13)
    domain=domain[domain!=a5]
    states=combn(domain,4)
    ans<-c()
    for( i in 1:length(states[1,])){
      print(states[,i])
      for(j in permn(states[,i])){
        nums<-j
        print(j)
        if(!eq1(nums[1],nums[2],nums[3],nums[4],a5)){
          print("found1!")
          ans<-c(ans,c(nums[1],nums[2],nums[3],nums[4],a5))
        }
      }
    }
    return(ans)
  }
  
  solve2<-function(ex,a5,domain){
    states=combn(domain,5)
    ans<-c()
    for( i in 1:length(states[1,])){
      for(j in permn(states[,i])){
        nums<-j
        if(!eq2(a5,nums[1],nums[2],nums[3],nums[4],nums[5])){
          print("found2!")
          ans<-c(ans,c(ex,nums[1],nums[2],nums[3],nums[4],nums[5]))
        }
      }
    }
    return(ans)
  }
  
  solve3<-function(ex,a10,dom){
    ans<-c()
    for(j in permn(dom)){
      vars<-j
      if(!eq3(a10,vars[1],vars[2],vars[3])){
        print("found3!")
        ans<-c(ans,c(ex,vars[1],vars[2],vars[3]))
      }
    }
    return(ans)
  }
  library("combinat")
  first_constraint_ans=c()
  for (i in 1:13){
    a5=i
    first_constraint_ans<-c(first_constraint_ans,solve1(a5))
  }
  first_ans=matrix(first_constraint_ans,nrow = 5)
  
  second_ans<-c()
  for(i in 1:length(first_ans[1,])){
    a5<-(first_ans[5,i])
    ex=first_ans[,i]
    tot_domain=seq(13)
    tot_domain<-tot_domain[!(tot_domain %in% ex)]
    second_ans=c(second_ans,c(solve2(ex,a5,tot_domain)))
  }
  second_ans=matrix(second_ans,nrow=10)
  third_ans<-c()
  for(i in 1:length(second_ans[1,])){
    a10<-(second_ans[10,i])
    ex=second_ans[,i]
    tot_domain=seq(13)
    tot_domain<-tot_domain[!(tot_domain %in% ex)]
    third_ans=c(third_ans,c(solve3(ex,a10,tot_domain)))
  }
  ans_1 = third_ans
  ans_1    
save(ans_1,file='1273168143_5.RData')  
