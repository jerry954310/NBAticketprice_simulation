#NBA上一季之平均票價資訊，包含單場平均票價以及平均季票價
ticket_price = 200
season_ticket_price = 5000
#使用者之出席機率分佈，為0.3,0.5,0.7
attprob=seq(0.3,0.7,0.2)
attendence = array(data = NA)
#NBA每一隊在主場的比賽次數=41
N = 41

allexp=matrix(nrow = 3, ncol = 1000) 
for(p in 1:3){
for(m in 1:1000){
for(i in 1:N){
  attendence[i] = rbinom(1, 1, attprob[p])
}

#NBA東區各隊明星球員數量列表
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
  west_local_team_show[i] = count_1
  for(g in 1:east[i]){
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
  count_1 = 0
  count_2 = 0
}

#NBA西區各隊明星球員數量統計
west <- c(1,2,0,3,1,2,2,0,0,3,1,1,2,3)
west_local_team_2 = 2
west_local_team_show_2 <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
west_actual_show = array(data = NA)
team_twelve = sample(1:14,12,replace = F)

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
    west_local_team_show_2[i] = count_1
    for(g in 1:west[i]){
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
    west_local_team_show_2[i] = count_1

    for(g in 1:team_twelve_member[i-14]){
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
    count_1 = 0
    count_2 = 0
  }
}

#在西區比賽時我方加上對方的明星球員數量
all_west_show = west_local_team_show_2+west_actual_show

price = array(data = NA)
for(i in 1:41){
  if(i<=26){
    price[i] = log(all_west_show[i]+3)*ticket_price
  }else{
    price[i]=log(west_local_team_show[i-26]+east_actual_show[i-26]+3)*ticket_price
  }
}

#票價係數，使用log取值，因其ㄋ變化率最合適
for(i in 1:8){
  a = log(i+2)
}

#每日上場的明星球員數量為隨機
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
for(i in 1:41){
  for(j in 1:assumeAllStar[i]){
    #表定出場機率(每一隊都固定)
    prob=rtri(1,0,1,0.7)
    count=count+rbinom(1,1,prob)
  }
  officialPrice[i]=log(count+3)*ticket_price
  count=0
}

#模擬明星球員出席狀況
Avalibility=array(data=NA)
for(i in 1:41){
  if(i<=26){
    Avalibility[i]=rtri(1,0.09,1,0.7-0.1*all_west_show[i])
  }else{
    Avalibility[i]=rtri(1,0.09,1,0.7-0.1*(west_local_team_show[i-26]+east_actual_show[i-26]))
  }
}
summary(Avalibility)
expenditure=0
cheaperPrice=0
moreExpensivePrice=0
for(i in 1:length(attendence)){
  if(attendence[i]==0){
    #使用者原本當天就不出席，do nothing!
    {
    canBuy=rbinom(2,1,Avalibility[2])
    if(price[i]<=officialPrice[i]){
      cheaperPrice=price[i]
      moreExpensivePrice=officialPrice[i]
    }else{
      cheaperPrice=officialPrice[i]
      moreExpensivePrice=price[i] 
    }
    
    if(canBuy[1]==1){
      #便宜的購買渠道可以買得到票
      expenditure=expenditure+cheaperPrice
    }else if(canBuy[2]==1){
      #便宜的買不到票，轉買貴的渠道
      expenditure=expenditure+moreExpensivePrice
    }else{
      #因為沒有票，今日無法觀看比賽
  }
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

#結果統計表
print(summaryTable)
print(qtable)

