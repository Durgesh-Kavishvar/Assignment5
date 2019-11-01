#Lottery.Utilities.R

#function1: find probability of m out of k out of N
Probability_m_k_N=function(m,k,N){
  #' This function takes three argument m,k,n and calculates probability of getting m out of k numbers correct, when k numbers are drawn from a pool of N numbers
  #' @param m : picked up numbers from a small pool k
  #' @param k : picked up numbers from big pool N
  #' @param N : Mother pool of numbers

  # this returns the probability
  return(choose(k,m)*choose(N-k,k-m)/choose(N,k))
}

#function 2: return of lottery tickets
returns_=function(k,N,awards,cost_per_ticket){
  #' This function takes k,N, awards list and cost per ticket as input and gives the return per ticket
  #' @param k : picked up numbers from big pool N
  #' @param N : Mother pool of numbers
  #' @param awards : list of awards
  #' @param cost_per_ticket : cost in $ per ticket

  vector_index=1:k #making a vector of from 1 to k, this is made for the subsequent use in lapply

  # this takes Probability_m_k_N function with k and N values as given
  # then this takes a vector from 1:k as values of m and passes through the function Probability_m_k_N 'k' times (similar to using for loops)
  return_per_number=as.numeric(unlist(lapply(vector_index,Probability_m_k_N,k=k,N=N)))  #as.numeric and unlist is used for algaebric operations
  # Note: you can use sapply to avoid un-listing !! But i just wanted to try lapply
  return(sum(return_per_number*awards[1:k])-cost_per_ticket) #this returns sum of probabilities*awards and subtracts cost per ticket to give return per ticket
}

#function 3: k numbers from 1:N
random_k_in_N=function(k,N){
  #' This function takes k,N as input and makes a vector of K random numbers from 1 to N
  #' @param k : picked up numbers from big pool N
  #' @param N : Mother pool of numbers

  random_vector=sample(1:N,k,replace=F) #sample picks up k numbers from 1 to N without replacement (i.e. replace=FALSE)
  return(random_vector)
}

#function4: match two vectors
match_vectors=function(v1,v2){
  #' This function takes v1, v2 as input and matches v1 and v2 to find how similar v2 is to v1
  #' @param v1 : subject vector 1
  #' @param v2 : vector 2 you use to compare with v1
  match_vector=v1%in%v2 #match v1 and v2 to get a boolean
  return((match_vector))
}

#function 5: total profit
total_profit=function(X,k,N,awards,cost_per_ticket){
  #' This function takes X,k,N, awards list and cost per ticket as input and gives total profit for X tickets
  #' @param X : number of tickets bought
  #' @param k : picked up numbers from big pool N
  #' @param N : Mother pool of numbers
  #' @param awards : list of awards
  #' @param cost_per_ticket : cost in $ per ticket

  rep_k=rep(k,X) #Replicate k X times for lapply
  x_random=lapply(rep_k,random_k_in_N,N=N)
  mat_x_random=matrix(as.numeric(unlist(x_random)),nrow=X,ncol=k,byrow=T) # create a matrix of size [X x k] where you have X sets of k random numbers (for further application in apply)
  winning_numbers=1:k #set of numbers which we will use as winning numbers, or we can use random numbers too by #random_k_in_N(k,N)
  match_win_with_Xs=apply(mat_x_random,MARGIN=1,match_vectors,v1=winning_numbers) # we have a matrix, we use matching on each row with winning matrix to get a boolean
  per_ticket=apply(match_win_with_Xs,MARGIN=2,sum) ##summing each row to get number of matches per ticket with winning
  total_return=sum(awards[per_ticket[per_ticket!=0]]) #we are summing all such awards elements which were equal to number of numbers in the vectors matched with winning vector excluding zeros
  return(sum(total_return)-X*cost_per_ticket)
}
