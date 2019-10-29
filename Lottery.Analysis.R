#Lottery.Analysis.R
source('Lottery.Utilities.R')

args=commandArgs(trailingOnly=TRUE)




if (args=="Normal"){

  k=6
  N=46
  cost_per_ticket=2
  awards=c(0, 2, 5, 150, 4000, 500000)
  X=rep(1000,1000)

  cat('\n', 'Using normal awards', '\n')

  average_ticket_return=returns_(k,N,awards,cost_per_ticket)

  tot_prof_X_tickts=unlist(lapply(X,total_profit,k=k,N=N,awards=awards,cost_per_ticket=cost_per_ticket))
  mean_prof_X_tickts=mean(tot_prof_X_tickts)
  cat('\n', 'The average value of return for 1000 tickets is', mean_prof_X_tickts,'\n')
  hist(tot_prof_X_tickts)
}

if   (args=="Rolldown"){
  k=6
  N=46
  cost_per_ticket=2
  awards=c(0, 2, 27, 807, 22096, 2000000)
  X=rep(1000,1000)

  cat('\n', 'Using normal awards', '\n')

  average_ticket_return=returns_(k,N,awards,cost_per_ticket)

  tot_prof_X_tickts=unlist(lapply(X,total_profit,k=k,N=N,awards=awards,cost_per_ticket=cost_per_ticket))
  mean_prof_X_tickts=mean(tot_prof_X_tickts)
  cat('\n', 'The average value of return for 1000 tickets is', mean_prof_X_tickts,'\n')
  hist(tot_prof_X_tickts)

}