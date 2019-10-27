#Lottery.Utilities.R

#function1: find probability of m out of k out of N
Probability_m_k_N=function(m,k,N){
  return(choose(k,m)*choose(N-k,k-m)/choose(N,k))
}

#function 2: return of lottery tickets
returns=function(k,N,awards,cost_per_ticket){
    vector_index=1:k
    return_per_number=as.numeric(lapply(vector_index,Probability_m_k_N,k=k,N=N))
    return(sum(return_per_number)-cost_per_ticket)
    #return(return_per_number)
}

#function 3: k numbers from 1:N
random_k_in_N=function(k,N){
  random_vector=sample(1:N,k,replace=T)
  return(random_vector)
}

#function4: match two vectors
match_vectors=function(v1,v2){
  match_vector=v1%in%v2
  return(match_vector)
}

#function 5: total profit
total_profit=function(X,k,N,awards,cost_per_ticket){
  rep_k=rep(k,X)
  x_random=lapply(rep_k,random_k_in_N,N=N)
  mat_x_random=matrix(as.numeric(unlist(x_random)),nrow=X,ncol=k,byrow=T)
  winning_numbers=random_k_in_N(k,N)
  match_win_with_Xs=apply(mat_x_random,MARGIN=1,match_vectors,v1=winning_numbers)
  sum=apply(match_win_with_Xs,MARGIN=1,sum)
  total_returns=unlist(lapply(sum,returns,N=N,awards=awards,cost_per_ticket=cost_per_ticket))
  return(total_returns)
}
