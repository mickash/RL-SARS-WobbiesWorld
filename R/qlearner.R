#' Make Q-Learner
#'
#' This is the function to pass to the runWW function to get par performance
makeQLearner=function(maze,gamma=.9,alpha=basicAlpha,randWalk=makeBasicRandWalk(1000000)) {
  n=nrow(maze$maze)
  m=n*n
  Q=data.frame(
    A=c(rep(2,m),rep(4,m),rep(6,m),rep(8,m)),
    W=rep(rep(1:n,each=n),4),
    M=rep(rep(1:n,n),4),
    U=rep(0,4*m),
    Visits=rep(0,4*m))
  out=list(Q=Q,n=n,m=m,alpha=alpha,gamma=gamma,totalVisited=0,randWalk=randWalk,
           decideAction=decideAction1M,update=update1M,lastRow=0,lastRand=F,doRand=T)
  class(out)="qLearner1M"
  return(out)
}

#' The following functions are part of the q-learner implementation
basicAlpha=function(cont,row){
  60/(59+cont$Q$Visits[row])
}
basicRandWalk=function(cont){
  max(0,.1-(cont$totalVisited/10000000))
}
makeBasicRandWalk=function(i,base=.1){
  function(cont) max(0,base-(cont$totalVisited/i))
}
findStateNumber1M=function(loc,maze) {
  which(loc[1]==maze$maze$x & loc[2]==maze$maze$y)
}
findRow1M=function(cont,maze,act) {
  mon=findStateNumber1M(maze$monster1,maze)
  wob=findStateNumber1M(maze$wobbie,maze)
  row=((act/2)-1)*cont$m
  row=row+(wob-1)*cont$n
  row=row+mon
  return(row)
}
findActionRows1M=function(cont,maze) {
  mon=findStateNumber1M(maze$monster1,maze)
  wob=findStateNumber1M(maze$wobbie,maze)
  row=(wob-1)*cont$n
  row=row+mon
  c(row,row+cont$m,row+2*cont$m,row+3*cont$m)
}
decideAction1M=function(cont,maze){
  rows=findActionRows1M(cont,maze)
  if (cont$doRand && runif(1)<=cont$randWalk(cont)) {
    cont$lastRand=T
    rand=sample(1:4,1)
    cont$lastRow=rows[rand]
    list (move=rand*2,control=cont)
  }
  else {
    cont$lastRand=F
    u=cont$Q$U[rows]
    bestU=which(u==max(u))
    if (length(bestU)==1) {
      cont$lastRow=rows[bestU]
      list (move=bestU*2,control=cont)
    }
    else {
      bestU=sample(bestU,1)
      cont$lastRow=rows[bestU]
      list (move=bestU*2,control=cont)
    }
  }
}
update1M=function(cont,maze){
  sRow=cont$lastRow
  cont$Q$Visits[sRow]=cont$Q$Visits[sRow]+1
  cont$totalVisited=cont$totalVisited+1
  nextActionRows=findActionRows1M(cont,maze)
  q=max(cont$Q$U[nextActionRows])
  if (maze$finished || !maze$alive) {
    cont$Q$U[sRow]=cont$Q$U[sRow]+cont$alpha(cont,sRow)*(maze$reward-cont$Q$U[sRow])
  }
  else {
    cont$Q$U[sRow]=cont$Q$U[sRow]+cont$alpha(cont,sRow)*(maze$reward+cont$gamma*q-cont$Q$U[sRow])
  }
  return (cont)
}
makeRlControl_1M=function(maze,gamma,alpha,randWalk)
  makeQLearner(maze,gamma,alpha,randWalk)

learnComp=function(
  n=10000,
  maze=makeWobbiesWorld(),
  controls=list(
    makeRlControl_1M(maze,gamma=.8,alpha=basicAlpha,randWalk=makeBasicRandWalk(1000000)),
    makeRlControl_1M(maze,gamma=.9,alpha=basicAlpha,randWalk=makeBasicRandWalk(1000000)),
    makeRlControl_1M(maze,gamma=.7,alpha=basicAlpha,randWalk=makeBasicRandWalk(1000000)),
    makeRlControl_1M(maze,gamma=.95,alpha=basicAlpha,randWalk=makeBasicRandWalk(1000000,.2)),
    makeRlControl_1M(maze,gamma=.7,alpha=basicAlpha,randWalk=makeBasicRandWalk(1000000,.2))
  ),
  every=100,
  plotStarted=F,
  ymin=900
) {
  score=lapply(controls,function(cont)c())
  turns=lapply(controls,function(cont)c())
  for (i in 1:n) {
    for (j in 1:length(controls)) {
      res=run(maze,controls[[j]])
      score[[j]]=c(score[[j]],res$score)
      turns[[j]]=c(turns[[j]],res$turn)
      controls[[j]]=res$control
      if (i%%every == 0) {
        if (plotStarted) {
          points(i/every,mean(score[[j]][(i-every):i]),col=j)
        }
        else {
          plot(i/every,mean(score[[j]][(i-every):i]),
               xlim=c(0,n/every),ylim=c(ymin,1000),col=j,xlab="Iteration",ylab="Score")
          plotStarted=T
        }
      }
    }
  }
  list(controls=controls,score=score,turns=turns)
}
