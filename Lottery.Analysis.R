#Lottery.Analysis.R
source('Lottery.Utilities.R')

args=commandArgs(trailingOnly=TRUE)
if (length(args)!=1){stop('This script needs an input argument from "Normal" and "Rolldown"')} #defensive step to take only 1 argument
if (args!="Normal" & args!="Rolldown") {stop('invalid argument, argument should be either "Normal" or "Rolldown"  ')}

#given paramters
k=6
N=46
cost_per_ticket=2
X=rep(1000,1000) #for sapply

if (args=="Normal"){
  # if the command line argument is Normal
  awards=c(0, 2, 5, 150, 4000, 500000) #awards for Normal lottery
  cat('\n', 'Using normal awards', '\n') #printing type of lottery
  average_ticket_return=returns_(k,N,awards,cost_per_ticket) #average return per ticket probabilistically: considering infinite tickets
  tot_prof_X_tickts=sapply(X,total_profit,k=k,N=N,awards=awards,cost_per_ticket=cost_per_ticket) #this takes 1000 tickets 1000 times and get total profit 1000 times   #you can use lapply with unlisting
  mean_prof_X_tickts=mean(tot_prof_X_tickts) #mean of 1000 attempts
  cat('\n', 'The average value of return for 1000 tickets is', mean_prof_X_tickts,'\n')
  cat('\n', 'The average value of return for a ticket is', average_ticket_return,'\n')
  hist(tot_prof_X_tickts, breaks=40, xlab="Total profit from 1000 tickets") #histogram plot
}

if   (args=="Rolldown"){
  # if the command line argument is Rolldown
  awards=c(0, 2, 27, 807, 22096, 2000000) #awards for Rolldown lottery
  cat('\n', 'Using Rolldown awards', '\n') #printing type of lottery
  average_ticket_return=returns_(k,N,awards,cost_per_ticket) #average return per ticket probabilistically: considering infinite tickets
  tot_prof_X_tickts=sapply(X,total_profit,k=k,N=N,awards=awards,cost_per_ticket=cost_per_ticket) #this takes 1000 tickets 1000 times and get total profit 1000 times   #you can use lapply with unlisting
  mean_prof_X_tickts=mean(tot_prof_X_tickts) #mean of 1000 attempts
  cat('\n', 'The average value of return for 1000 tickets is', mean_prof_X_tickts,'\n')
  cat('\n', 'The average value of return for a ticket is', average_ticket_return,'\n')
  hist(tot_prof_X_tickts, breaks=40, xlab="Total profit from 1000 tickets")#histogram plot
}
