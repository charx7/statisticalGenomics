cpdag <- function(incidence){
  
  z <- order.edges(incidence)
  new_mat <- cbind(z,numeric(nrow(z)))    # edges, parents, children, order, zeros
  n_mat <- new_mat[order(new_mat[,4]),]   # sort the edges by its order
  vec <- numeric(nrow(z))
  while(any(vec==0)){                                  # while there are unlabeled edges            l.3
    if (length(vec)>1){                                  # if there are at least 2 edges
      first <- which(n_mat[,5]==0)[1]                    # first EDGE that is labeled "unknown" (0)  l.4
      parent1 <- n_mat[first,2]                          # x   parent NODE
      child1 <- n_mat[first,3]                           # y   child NODE
      comp1 <- n_mat[which(n_mat[,3]==parent1 & n_mat[,5]==1),2]      # w NODES that have an edge incident into the parent labeled compelled)
    }
    if (length(vec)==1){
      first <- which(n_mat[5]==0)                      # first edge that is labeled "unknown" (0)
      parent1 <- n_mat[2]                             # x   parent
      child1 <- n_mat[3]                              # y   child
      comp1 <- numeric(0)
    }
    for (j in comp1){                                   #                                            l.5
      if (incidence[j,child1]==0){                     # if w is not a parent of the child          l.6
        n_mat[first,5] <- 1                             # label x -> y compelled                     l.7
        n_mat[which(n_mat[,3]==child1),5] <- 1          # label every edge incident into y compelled l.7
        vec[first] <- 1
        vec[which(n_mat[,3]==child1)] <- 1
        break   
      }
      if (incidence[j,child1]!=0)    {
        n_mat[which(n_mat[,2]==j & n_mat[,3]==child1),5] <- 1  # label w -> y compelled                l.10
        vec[which(n_mat[,2]==j & n_mat[,3]==child1)] <- 1
      }
    }
    if (length(vec)>1){  
      if(n_mat[first,5]==0){
        
        moep <- n_mat[which(n_mat[,3]==child1 & n_mat[,2]!=parent1),2]      # other parents of the child
        if(length(moep)>0){                              #                     l.11
          for(o in moep){
            if(incidence[o,parent1]==0){
              vec[first] <- 1
              vec[which(n_mat[,3]==child1 & n_mat[,5]==0)] <- 1
              n_mat[first,5] <- 1                                     # label x -> y compelled
              n_mat[which(n_mat[,3]==child1 & n_mat[,5]==0),5] <- 1   # label all "unknown" edges incident into y compelled
              break
            }
            if(all(incidence[moep,parent1]!=0)){
              vec[first] <- -1
              vec[which(n_mat[,3]==child1 & n_mat[,5]==0)] <- -1
              n_mat[first,5] <- -1                                    # label x -> y reversible
              n_mat[which(n_mat[,3]==child1 & n_mat[,5]==0),5] <- -1  # label all "unknown" edges incident into y reversible
            }
          }
        }
        if(length(moep)==0){
          vec[first] <- -1
          vec[which(n_mat[,3]==child1 & n_mat[,5]==0)] <- -1
          n_mat[first,5] <- -1                                    # label x -> y reversible
          n_mat[which(n_mat[,3]==child1 & n_mat[,5]==0),5] <- -1  # label all "unknown" edges incident into y reversible
        }
      }
    }
    if (length(vec)==1){
      n_mat[5] <- -1                                    # label x -> y reversible
      vec <- -1
    }
  }
  return(n_mat)
}

order.edges <- function(incidence){  
  
  top.order <- top_order(incidence)
  n <- length(top.order)
  edges <- which(incidence!=0)
  children <- child(edges,n)
  parents <- parent(edges,n)
  m <- length(edges)
  ordered_edges  <- numeric(m)
  incidence_n <- incidence
  tog <- matrix(c(edges,parents,children,ordered_edges),ncol=4, byrow=FALSE)  
  k <- 1
  while(any(tog[,4]==0)){
    node1 <- top.order[which(colSums(incidence_n[,top.order])>0)][1]    # first node in top. order that has at least one parent
    par1<- tog[which(tog[,3]==node1),2]                # find the parents of  first child in the top. order that has an unordered edge incident into it
    g <- par1[which(par1>0)]
    f1 <- numeric(length(g))
    for (i in 1:length(g)){
      f1[i] <- which(top.order==g[i])
    }
    par2 <- g[which.max(f1)]                           # find the highest ordered node that has an edge leading into node1
    tog[which(tog[,2]==par2 & tog[,3]==node1),4] <- k     
    k <- k + 1
    incidence_n[tog[which(tog[,2]==par2 & tog[,3]==node1),1]] <- 0     # delete the edge in the "incidence" matrix
    tog[which(tog[,2]==par2 & tog[,3]==node1),2] <- 0
  }
  to <- matrix(c(edges,parents,children,tog[,4]),ncol=4,byrow=FALSE)
  return(to)                                          # return the whole matrix, the order is the fourth column
}


############################################################

child <- function(edges,n){ 
  # input: the numbers of the edges in the incidence matrix and the number of nodes
  p <- ceiling(edges/n)
  return(p)
}

##############################################################

parent <- function(edges,n){
  ch <- edges + n - child(edges,n)*n
  return(ch)
}


top_order <- function(incidence){
  
  n <- length(incidence[1,])
  Order <- numeric(n)
  fan_in <- numeric(n)
  no_fan_in <- numeric(0)
  m <- 1
  for (p in 1:n){                                       # number of parent nodes at the beginning
    fan_in[p] <- sum(incidence[,p])
  }
  no_fan_in <- which(fan_in==0)
  while (length(which(Order==0))>0){                    # as long as there is a node without an order
    fan_in[which(incidence[no_fan_in[1],]==1)] <- fan_in[which(incidence[no_fan_in[1],]==1)] - 1
    no_fan_in <- c(no_fan_in, c(which(incidence[no_fan_in[1],]==1),which(fan_in==0))[duplicated(c(which(incidence[no_fan_in[1],]==1),which(fan_in==0)))])
    Order[m] <- no_fan_in[1]
    no_fan_in <- no_fan_in[-1]
    m <- m+1
  }
  return(Order)
}

UpdateAncestorMatrix <- function(incidence,ancestor,parent,child,operation){
  
  # operation=1 <=> edge reversal 
  # i.e. the edge "parent->child" is reversed to "child <- parent"
  
  # operation=2 <=> edge deletion 
  # i.e. the edge "parent->child" is deleted
  
  # operation=3 <=> edge addition 
  # i.e. the edge "parent->child" is added
  
  ancestor_new <- ancestor
  
  #################################################################################
  # update after edge reversal
  
  if (operation==1){
    
    incidence_new = incidence;
    incidence_new[parent,child] = 0
    incidence_new[child,parent] = 1
    
    ancestor_new[c(child,which(ancestor[,child]==1)),] <- 0        
    top_name <- des_top_order(incidence_new, ancestor, child)           
    for (d in top_name){
      for(g in which(incidence_new[,d]==1)){
        ancestor_new[d,c(g,(which(ancestor_new[g,]==1)))] <- 1           
      }
    }
    
    anc_parent <- which(ancestor_new[child,]==1)                     
    des_child <- which(ancestor_new[,parent]==1)                     
    ancestor_new[c(parent,des_child),c(child,anc_parent)] <- 1
  }
  
  ###################################################################################
  ### update after edge deletion
  if (operation==2){
    
    incidence_new = incidence;
    incidence_new[parent,child] = 0
    
    ancestor_new[c(child,which(ancestor[,child]==1)),] <- 0                          
    top_name <- des_top_order(incidence_new, ancestor, child)             
    for (d in top_name){
      for(g in which(incidence_new[,d]==1)) {
        ancestor_new[d,c(g,(which(ancestor_new[g,]==1)))] <- 1           
      }
    }
  }
  
  ####################################################################################
  # update after edge addition
  if (operation==3){
    
    incidence_new = incidence;
    incidence_new[parent,child] = 1
    
    anc_parent <- which(ancestor[parent,]==1)        
    des_child <- which(ancestor[,child]==1)          
    ancestor_new[c(child,des_child),c(parent,anc_parent)] <- 1
  }
  
  return(ancestor_new)
  
}

####################################################################################
# Required inner function:

des_top_order <- function(incidence, ancestor,child){
  
  n = length(incidence[1,])
  
  top <- top_order(incidence)
  
  position_child <- which(top==child)
  
  top_all_after <- top[position_child:n]                
  
  desc <- which(ancestor[,child]==1)                     
  
  inter_step <- c(child,desc,top_all_after)
  
  des_top <- inter_step[which(duplicated(inter_step))]
  
  return(des_top)
  
}


####################################################################################
# Another required inner function:

top_order <- function(incidence){
  
  n <- length(incidence[1,])
  Order <- numeric(n)
  fan_in <- numeric(n)
  no_fan_in <- numeric(0)
  m <- 1
  for (p in 1:n){                                       # number of parent nodes at the beginning
    fan_in[p] <- sum(incidence[,p])
  }
  no_fan_in <- which(fan_in==0)
  while (length(which(Order==0))>0){                    # as long as there is a node without an order
    fan_in[which(incidence[no_fan_in[1],]==1)] <- fan_in[which(incidence[no_fan_in[1],]==1)] - 1
    no_fan_in <- c(no_fan_in, c(which(incidence[no_fan_in[1],]==1),which(fan_in==0))[duplicated(c(which(incidence[no_fan_in[1],]==1),which(fan_in==0)))])
    Order[m] <- no_fan_in[1]
    no_fan_in <- no_fan_in[-1]
    m <- m+1
  }
  return(Order)
}

logBGe <- function(Data, incidence, v=1,a=nrow(Data)+2,mu=numeric(nrow(Data)),T_0=diag(0.5,nrow(Data),nrow(Data))){
  
  n <- nrow(Data)                                
  m <- ncol(Data)                                
  T_m <- T_0 + (m-1)* cov(t(Data)) + ((v*m)/(v+m))* (mu - rowMeans(Data))%*%t(mu - rowMeans(Data)) 
  
  c_function <- function(N,A){
    fact <- numeric(N)
    for (i in 1:N){
      fact[i] <- -lgamma((A+1-i)/2)
    }
    product <- sum(fact) -(A*N/2)*log(2)- (N*(N-1)/4)*log(pi)
    return(product)}
  
  P_local_num <- numeric(n)   
  P_local_den <- numeric(n)   
  
  for (j in 1:n)  {
    n_nodes <- which(incidence[,j]==1)         
    P_local_num[j] <- (-(length(n_nodes)+1)*m/2)*log(2*pi) + ((length(n_nodes)+1)/2)*log(v/(v+m)) + c_function((length(n_nodes)+1),a)-c_function((length(n_nodes)+1),a+m)+ (a/2)*log(det(as.matrix(T_0[sort(c(n_nodes,j)),sort(c(n_nodes,j))])))+ (-(a+m)/2)*log(det(as.matrix(T_m[sort(c(n_nodes,j)),sort(c(n_nodes,j))])))
    if(sum(incidence[,j])>0){          
      P_local_den[j] <- (-(length(n_nodes))*m/2)*log(2*pi) + (length(n_nodes)/2)*log(v/(v+m)) + c_function(length(n_nodes),a)- c_function(length(n_nodes),a+m)+ (a/2)*log(det(as.matrix(T_0[n_nodes,n_nodes])))+ (-(a+m)/2)*log(det(as.matrix(T_m[n_nodes,n_nodes])))
    }
    else{                              
      P_local_den[j] <- 0
    }
  }
  
  log_bge <- sum(P_local_num) - sum(P_local_den)
  
  return(log_bge)
}

