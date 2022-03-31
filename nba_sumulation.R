#Average ticket price last year and average season ticket price last year
ticket_price = 200
season_ticket_price = 5000
#???业?NBA?F???^????賽?膸???
attprob=seq(0.3,0.7,0.2)
attendence = array(data = NA)
#N????賽?校??????欠竦綀??^賽
N = 41

allexp=matrix(nrow = 3, ncol = 1000) 
for(p in 1:3){
for(m in 1:1000){
for(i in 1:N){
  attendence[i] = rbinom(1, 1, attprob[p])
}
##print(attendence)
##print(length(attendence))

#???^NBA?????????????T??量
east<- c(2,3,3,0,2,1,3,1,1,0,2,4,1,1,1)
east_actual_show = array(data = NA)
west_local_team = 2 
west_local_team_show <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)

##print(west_local_team)
count_1 = 0
count_2 = 0
for(i in 1:length(east)){
  for(j in 1:west_local_team){
    prob_1 = rtri(1,min=0,max=1,mode=0.5)
    pro_show_1 = rbinom(1, 1, prob_1)
    if(pro_show_1 == 1){
      count_1 = count_1 + 1
    }
  }
  ###print(count_1)
  west_local_team_show[i] = count_1
  ##print(west_local_team_show[j])
  for(g in 1:east[i]){
    #prob = runif()
    if(east[i] == 0){
      count_2 = count_2 + 0
    }
    else{
      prob_2 = rtri(1,min=0,max=1,mode=0.7)
      pro_show_2 = rbinom(1, 1, prob_2)
      if(pro_show_2 == 1){
        count_2 = count_2 + 1
      }
    }
  }
  east_actual_show[i] = count_2
  #print(east_actual_show[i])
  count_1 = 0
  count_2 = 0
}
#print(west_local_team_show)
#print(length(west_local_team_show))
#print(east_actual_show)
#print(length(east_actual_show))
#???^NBA?????????????T??量
#all_team <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14)
west <- c(1,2,0,3,1,2,2,0,0,3,1,1,2,3)
#print(length(west))
west_local_team_2 = 2
west_local_team_show_2 <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
west_actual_show = array(data = NA)
team_twelve = sample(1:14,12,replace = F)
#print(team_twelve)
#print(length(team_twelve))
#print(team_twelve[1])
team_twelve_member = array(data = NA)
for (i in 1:length(team_twelve)){
  team_num = team_twelve[i]
  team_twelve_member[i] = west[team_num]
}
#print(team_twelve_member)
for(i in 1:26){
  if(i <= 14){
    for(j in 1:west_local_team_2){
      prob_1 = rtri(1,min=0,max=1,mode=0.5)
      pro_show_1 = rbinom(1, 1, prob_1)
      if(pro_show_1 == 1){
        count_1 = count_1 + 1
      }
    }
    ##print(count_1)
    west_local_team_show_2[i] = count_1
    ##print(west_local_team_show[j])
    for(g in 1:west[i]){
      #prob = runif()
      if(west[i] == 0){
        count_2 = count_2 + 0
      }
      else{
        prob_2 = rtri(1,min=0,max=1,mode=0.7)
        pro_show_2 = rbinom(1, 1, prob_2)
        if(pro_show_2 == 1){
          count_2 = count_2 + 1
        }
      }
    }
    west_actual_show[i] = count_2
    #print(west_actual_show[i])
    count_1 = 0
    count_2 = 0
  }
  else{
    for(j in 1:west_local_team_2){
      prob_1 = rtri(1,min=0,max=1,mode=0.5)
      pro_show_1 = rbinom(1, 1, prob_1)
      if(pro_show_1 == 1){
        count_1 = count_1 + 1
      }
    }
    ##print(count_1)
    west_local_team_show_2[i] = count_1
    ##print(west_local_team_show[j])
    for(g in 1:team_twelve_member[i-14]){
      #prob = runif()
      if(west[i-14] == 0){
        count_2 = count_2 + 0
      }
      else{
        prob_2 = rtri(1,min=0,max=1,mode=0.7)
        pro_show_2 = rbinom(1, 1, prob_2)
        if(pro_show_2 == 1){
          count_2 = count_2 + 1
        }
      }
    }
    west_actual_show[i] = count_2
    #print(west_actual_show[i])
    count_1 = 0
    count_2 = 0
  }
}
#print(west_local_team_show_2)
#print(length(west_local_team_show_2))
#print(west_actual_show)
#print(length(west_actual_show))
all_west_show = west_local_team_show_2+west_actual_show
#print(all_west_show)
price = array(data = NA)
for(i in 1:41){
  if(i<=26){
    price[i] = log(all_west_show[i]+3)*ticket_price
  }else{
    price[i]=log(west_local_team_show[i-26]+east_actual_show[i-26]+3)*ticket_price
  }
}
#print(price)

#price multiple
for(i in 1:8){
  a = log(i+2)
  #print(a)
}

#?俜??r??
assumeAllStar = array(data = NA)
officialPrice = array(data = NA)
count=0
for(i in 1:41){
  if(i<=14){
    assumeAllStar[i]=west[i]+2
  }else if(i<=26){
    assumeAllStar[i]=team_twelve_member[i-14]+2
  }else{
    assumeAllStar[i]=east[i-26]+2
  }
}
#print(assumeAllStar)
for(i in 1:41){
  for(j in 1:assumeAllStar[i]){
    prob=rtr#琛ㄥ畾鍑哄牬姗熺巼(姣忎竴闅婇兘鍥哄畾)
    i(1,0,1,0.7)
    count=count+rbinom(1,1,prob)
  }
  officialPrice[i]=log(count+3)*ticket_price
  count=0
}
#print(officialPrice)

for(h in 1:41){
  #print(price[h]-officialPrice[h])
}

Avalibility=array(data=NA)
for(i in 1:41){
  if(i<=26){
    #Avalibility[i]=1-0.3*log(all_west_show[i]+3)
    Avalibility[i]=rtri(1,0.09,1,0.7-0.1*all_west_show[i])
  }else{
    #Avalibility[i]=1-0.3*log(west_local_team_show[i-26]+east_actual_show[i-26]+3)
    Avalibility[i]=rtri(1,0.09,1,0.7-0.1*(west_local_team_show[i-26]+east_actual_show[i-26]))
  }
}
summary(Avalibility)
expenditure=0
cheaperPrice=0
moreExpensivePrice=0
for(i in 1:length(attendence)){
  if(attendence[i]==0){
    #do nothi鐣跺ぉ鍘熸湰灏变笉渚唟
    canBuy=rbinom(2,1,Avalibility[2])
    if(price[i]<=officialPrice[i]){
      cheaperPrice=price[i]
      moreExpensivePrice=officialPrice[i]
    }else{
      cheaperPrice=officialPrice[i]
      moreExpensivePrice=price[i] 
    }
    
    if(canBuy[1]==1){
      #?I???^???说??袌?
      expenditure=expenditure+cheaperPrice
    }else if(canBuy[2]==1){
      #?I???^?F???袌?
      expenditure=expenditure+moreExpensivePrice
    }else{
  }
#print(expenditure)
allexp[p,m]=expenditure
}
}

count3=0
allexp2=matrix(nrow=3,ncol=1000)
for(h in 1:3){
for(j in 1:1000){
  allexp2[h,j]=season_ticket_price-allexp[h,j]
}
}

summary(allexp2[1,])
summary(allexp2[2,])
summary(allexp2[3,])
windows()
A=hist(allexp2[1,])
windows()
B=hist(allexp2[2,])
windows()
C=hist(allexp2[3,])
summaryTable=c()
for(j in 1:3){
for(h in 1:1000){
  if(allexp[j,h]<=season_ticket_price){
    count4=count4+1
  }
}
  summaryTable[j]=count4/1000
  count4=0
}
qtable=matrix(nrow=3,ncol=3)
for(j in 1:3){
  qtable[j,1]=quantile(allexp2[j,],0.25)
  qtable[j,2]=quantile(allexp2[j,],0.50)
  qtable[j,3]=quantile(allexp2[j,],0.75)
  
}
print(summaryTable)#省?X?臋C??
print(qtable)#?俜?位????

