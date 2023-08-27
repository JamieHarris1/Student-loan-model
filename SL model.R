library(tidyverse)
library(reshape2)
library(EnvStats)
library(gridExtra)


#30y then loan wiped
T <- 30
N <- 1000

#Assumptions
tuition <- 9.25 * 3
maint <- 4.5 * 3
thres <- 27.66
int <- 0.05
g <- 0.04
disp <- 0.15

#Income dist parameters
#Pareto
min.salary <- 25 #min income
a <- 10  #lower more unequal

#Normal
mu <- 0.02 #mean wage growth
sd <- 0.05 #sd of wage growth

#Generates N people with initial incomes following Pareto dist
#and income growth being normally distributed
ensamble.data <- function(N){
data <- data.frame(year=1:T)
for(n in 1:N){
  inc.t <- rpareto(n=1, min.salary, a)
  inc.life <- rep(0,T)
  for(t in 1:T){
    #g <- rbeta(1, inc.t/10, 100) 
    g <- rnorm(1, mu, sd)
    inc.life[t] <- inc.t
    inc.t <- inc.t * (1 + g)
  }
  data <- cbind(data, inc.life)
  colname <- paste0("n", n)
  names(data)[length(names(data))]<-colname
}
return(data)
}
ensamble.plot <- function(data, type){
  df <- melt(data ,  id.vars = 'year', variable.name = 'series')

  p <- ggplot(df, aes(year,value)) + 
        geom_line(aes(colour = series)) +
        theme(legend.position = "none")
  print(p)
  # Save the plot
  file_name <- sprintf("%s_income_plot.png", type)
  ggsave(file_name, plot = p, width = 6, height = 4)
}
compare.plot <- function(data1, data2, leg, type){
  df1 <- melt(data1 ,  id.vars = 'year', variable.name = 'series')
  df2 <- melt(data2 ,  id.vars = 'year', variable.name = 'series')
  
  
  plot1 <- ggplot(df1, aes(year,value)) + 
    geom_line(aes(colour = series)) +
    labs(title = "Payoff")+
    if(leg==FALSE){theme(legend.position = "none")}
    else{}
  
  plot2 <- ggplot(df2, aes(year,value)) + 
    geom_line(aes(colour = series)) +
    labs(title = "Invest")+
    if(leg==FALSE){theme(legend.position = "none")}
  else{}
  p = grid.arrange(plot1, plot2, nrow = 1)
  print(p)
  file_name <- sprintf("%s_invest_vs_payoff.png", type)
  ggsave(file_name, plot = p, width = 6, height = 4)
  }


#Pay off loan asap then invest
agent.payoff <- function(income){
  agent.data <- data.frame(year=1:T)
  #Disposible income availale to pay off loan or invest
  for(n in 1:N){
    loan <- tuition + maint
    invest <- 0
    loan.track <- rep(0, T)
    invest.track <- rep(0, T)
    
    for(t in 1:T){
      repay <- max(0, (income[t,n]-thres)*disp)
      if(loan>0){
        loan <- loan*(1+int) - repay
        loan.track[t] <- loan
      }
      else{
        loan.track[t-1] <- 0
        invest <-  + (abs(loan)+invest)*(1+g) + repay
        invest.track[t] <- invest
        
        loan <- 0
      }
    }
    #Forgiven after 30y
    loan.track[T] <- 0
    loan <- 0
    netw <- invest.track-loan.track
    agent.data <- cbind(agent.data, netw)
    colname <- paste0("netw", n)
    names(agent.data)[length(names(agent.data))]<-colname
  }
  #agent.data <- data.frame(year=1:T, income=income, loan=loan.track, 
  # invest=invest.track, netw=invest.track-loan.track)
  return(agent.data)
}

#Pay off min and invest rest of disp
agent.invest <- function(income){
  agent.data <- data.frame(year=1:T)
  #Disposable income available to pay off loan or invest
  for(n in 1:N){
    loan <- tuition + maint
    invest <- 0
    loan.track <- rep(0, T)
    invest.track <- rep(0, T)
    
    for(t in 1:T){
      repay <- max(0, (income[t,n]-thres)*0.09)
      if(loan>0){
        loan <- loan*(1+int) - repay
        loan.track[t] <- loan
        capital <- max(0, (income[t,n]-thres)*(disp-0.09))
        invest <- invest*(1+g) + capital
        invest.track[t] <- invest
      }
      else{
        loan.track[t-1] <- 0
        invest <- abs(loan) + invest*(1+g) + (income[t,n]-thres)*disp
        invest.track[t] <- invest
        
        loan <- 0
      }
    }
    #Loan forgiven after 30y
    loan.track[T] <- 0
    loan <- 0
    netw <- invest.track-loan.track
    agent.data <- cbind(agent.data, netw)
    colname <- paste0("netw", n)
    names(agent.data)[length(names(agent.data))]<-colname
  }
  return(agent.data)
}

ensamble <- ensamble.data(N)
ensamble.plot(ensamble, "ensamble")

#Payoff
agent.payoff.data <- agent.payoff(ensamble[-1])
#Strategy's avergage networth across all agents
rowMeans(agent.payoff.data)[T]

#Invest
agent.invest.data <- agent.invest(ensamble[-1])
#Strategy's avergage networth across all agents
rowMeans(agent.invest.data)[T]

#Plot both net worth sets on one axis
compare.plot(agent.payoff.data, agent.invest.data, FALSE, "ensamble")


##################################
#Individual
##################################
T <- 40
min.salary <-30 #starting salary in this context
int <- 0.05 #interest rate
g <- 0.07 #equity growth
disp <- 0.30 #proportion of income over threshold available

#Normal
mu <- 0.03 #mean wage growth
sd <- 0.05 #sd of wage growth


ensamble <- ensamble.data(N)
individual.payoff.individual <- function(income){
  #Disposable income available to pay off loan or invest
  loan <- tuition + maint
  invest <- 0
  loan.track <- rep(0, T)
  invest.track <- rep(0, T)
  
  for(t in 1:T){
    repay <- max(0, (income[t]-thres)*disp)
    if(loan>0){
      loan <- loan*(1+int) - repay
      loan.track[t] <- loan
    }
    else{
      loan.track[t-1] <- 0
      invest <- abs(loan) + invest*(1+g) + (income[t]-thres)*disp
      invest.track[t] <- invest
      
      loan <- 0
    }
  }
  #Loan forgiven after 30y
  loan.track[T] <- 0
  loan <- 0
  netw <- invest.track-loan.track
  agent.data <- data.frame(year=1:T, income, loan.track, invest.track, netw)
  return(agent.data)
}
individual.invest.individual <- function(income){
  #Disposable income available to pay off loan or invest
  loan <- tuition + maint
  invest <- 0
  loan.track <- rep(0, T)
  invest.track <- rep(0, T)
  
  for(t in 1:T){
    repay <- max(0, (income[t]-thres)*0.09)
    if(loan>0){
      loan <- loan*(1+int) - repay
      loan.track[t] <- loan
      capital <- max(0, (income[t]-thres)*(disp-0.09))
      invest <- invest*(1+g) + capital
      invest.track[t] <- invest
    }
    else{
      loan.track[t-1] <- 0
      invest <- abs(loan) + invest*(1+g) + (income[t]-thres)*disp
      invest.track[t] <- invest
      
      loan <- 0
    }
  }
  #Loan forgiven after 30y
  loan.track[T] <- 0
  loan <- 0
  netw <- invest.track-loan.track
  agent.data <- data.frame(year=1:T, income, loan.track, invest.track, netw)
  return(agent.data)
}

income <- ensamble[["n1"]]
individual.payoff.data <- individual.payoff.individual(income)
individual.invest.data <- individual.invest.individual(income)
compare.plot(individual.payoff.data, individual.invest.data, TRUE, "individual")

tail(individual.payoff.data$netw, n=1)
tail(individual.invest.data$netw, n=1)
