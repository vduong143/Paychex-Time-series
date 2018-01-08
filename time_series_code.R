setwd("~/Downloads/PAYX_Data_20170906")
tables <- c("Prod1","Prod2","Prod3","Prod4","Prod5","Prod6","Prod7","Prod8")
activity <- read.csv("Activity.csv",na.strings = (""))

  #t is the name of the table
  t <- "Prod7"
  prod <- read.csv(paste(t,".csv",sep = ""))
  
  prodIndex <- as.integer(substring(t,5,5)) + 1
  
  #speccific column that refer to the ineligibility data
  #prodIndex <- 10
  
  #compute the months of activity for subset of customers who had activity in probability tables
  act <- activity[!(activity[,prodIndex] %in% c(NA, "2017-01-01", "2017-02-01", "2017-03-01", "2017-04-01", "2017-05-01", "2017-06-01", "2017-07-01", "2017-08-01", "2017-09-01")),]
  customers <- act$RecID
  purchase <- prod[(prod$RecID %in% customers),]
  purchase$MonthOfActivity <- rep(NA,nrow(purchase))
  for(x in 1:nrow(purchase)) {
    for(y in 1:nrow(activity)) {
      if(activity[y,1]==purchase[x,1]) {
        if(!is.na(activity[y,prodIndex])) {
          purchase[x,14] = as.integer(substring(activity[y,prodIndex],6,7))
        }
      }
    }
  }

  #time-series plot
  ts.plot(t(purchase[,2:13]),lwd=".5",col="grey",main = t)
  for(i in 1:nrow(purchase)) {
    if(!is.na(purchase[i,14])) {
      points(x=purchase[i,14],y=purchase[i,purchase[i,14]+1],col="red")
    }
  }
  for(i in 1:nrow(purchase)) {
    if(!is.na(purchase[i,14])) {
      if(is.na(purchase[i,purchase[i,14]+1])) {
        points(x=purchase[i,14]-1,y=purchase[i,purchase[i,14]],col="blue")
      }
    }
  }
  
  #probs at the month of purchase
  prob1 <- list()
  for(i in 1:nrow(purchase)) {
    if(!is.na(purchase[i,purchase[i,14]+1])) {
      prob1 <- c(prob1,as.double(purchase[i,purchase[i,14]+1]))
    }
  }
  prob1 <- unlist(prob1)
  if(!(length(prob1)==0)) {
    hist(prob1,probability = TRUE,main = t,xlab = "Prob. at the Months of Purchases",breaks = 10)
    lines(density(prob1),lty="dotted")
    lines(density(prob1,adjust = 2),lwd=2)
  }
  
  #probs at month prior to purchase
  prob2 <- list()
  for(i in 1:nrow(purchase)) {
    if(!is.na(purchase[i,purchase[i,14]])) {
      prob2 <- c(prob2,as.double(purchase[i,purchase[i,14]]))
    }  
  }
  prob2 <- unlist(prob2)
  if(!(length(prob2)==0)) {
    hist(prob2,probability = TRUE,main = t,xlab = "Prob. at the Months Prior to Purchases",breaks = 10)
    lines(density(prob2),lty="dotted")
    lines(density(prob2,adjust = 2),lwd=2)
  }
  
  #distribution for months of purchase
  hist(purchase[,14],main = t,xlab = "Month")
  
  if(!(length(prob1)==0)) {
    plot(ecdf(prob1),col="red",pch ='.',lwd=2)
    plot(ecdf(prob2),col="blue",pch = '.',lwd=2,add=TRUE)
  } 
  
  #compute activity level for each product
  sampleSizes <- list()
  for(t in tables) {
    sample <- read.csv(file=paste(t,"Activity",".csv",sep = ""))
    sampleSizes <- c(sampleSizes,nrow(sample))
  }
  sampleSizes <- unlist(sampleSizes)
  barplot(sampleSizes,names.arg = tables,main = "Number of Purchases per Product")
  
  #put months of activity in prob tables of 25000 customers
  ts.plot(t(prod[,2:13]),lwd=".5",col="grey")
  prod$MonthOfActivity <- rep(NA,rep = nrow(prod))
  for(i in 1:nrow(prod)) {
    for(j in 1:nrow(purchase)) {
      if(prod$RecID[i] == purchase$RecID[j]) {
        prod$MonthOfActivity[i] = purchase$MonthOfActivity[j]
      }
    }
  }
  
  #slope
  prod$slope <- rep(NA,rep = nrow(prod))
  for(i in 1:nrow(prod)) {
    y <- ts(t(prod[i,2:13]))
    fit <- forecast::tslm(y ~ trend)
    prod$slope[i] <- fit$coefficients[2]
  }
  plot(prod$slope[is.na(prod$MonthOfActivity)],col = "grey",main = "Product 7 Slopes of Trends",ylab="Slope",xlab="Customer ID")
  for(i in 1:nrow(prod)) {
    if(!is.na(prod$MonthOfActivity[i])) {
      points(i,prod$slope[i], col = "red")
    }
  }
  abline(h=0,lty=2)
  
  #mean
  prod$mean <- rep(NA,rep = nrow(prod))
  for(i in 1:nrow(prod)) {
    prod$mean[i] <- mean(na.omit(t(prod[i,2:13])))
  }
  plot(prod$mean[is.na(prod$MonthOfActivity)],col = "grey",main = "Product 7 Probability Means",ylab="Probability",xlab="Customer ID")
  for(i in 1:nrow(prod)) {
    if(!is.na(prod$MonthOfActivity[i])) {
      points(i,prod$mean[i], col = "red")
    }
  }
  
  #sd
  prod$sd <- rep(NA,rep = nrow(prod))
  for(i in 1:nrow(prod)) {
    prod$sd[i] <- sd(na.omit(t(prod[i,2:13])))
  }
  plot(prod$sd[is.na(prod$MonthOfActivity)],col = "grey",main = "Product 7 Probability Standard Deviations",ylab="Probability",xlab="Customer ID")
  for(i in 1:nrow(prod)) {
    if(!is.na(prod$MonthOfActivity[i])) {
      points(i,prod$sd[i], col = "red")
    }
  }
  
  #mean absolute difference between 2 months
  prod$mean.ad <- rep(NA,rep = nrow(prod))
  diff <- array()
  for(i in 1:nrow(prod)) {
    diff[1:11] <- NA 
    for(j in 1:11) {
      if(!is.na(prod[i,j+1]) & !is.na(prod[i,j+2])) {
        diff[j] <- abs(prod[i,j+2] - prod[i,j+1])
      }
    }
    prod$mean.ad[i] <- mean(na.omit(diff))
  }                  
  plot(prod$mean.ad[is.na(prod$MonthOfActivity)],col = "grey",main = "Product 7 Mean Absolute Differences",ylab="Mean Abs. Diff.",xlab="Customer ID")
  for(i in 1:nrow(prod)) {
    if(!is.na(prod$MonthOfActivity[i])) {
      points(i,prod$mean.ad[i], col = "red")
    }
  }
  
  
  #purchase
  prod$purchased <- ifelse(!is.na(prod$MonthOfActivity),1,0)
  #prod$RecID <- toString(prod$RecID)
  data <- prod[,which(names(prod) %in% c("mean","sd"))]

  #clustering
  install.packages("ClusterR")
  clusters <- ClusterR::KMeans_rcpp(data = data, clusters = 3, num_init = 10,max_iters = 1000, initializer = "kmeans++",fuzzy = TRUE)
  
  #assign clusters to customers
  prod$clusters <- clusters$clusters
  for(i in 1:max(prod$clusters)) {
    print(paste("Prob. of Purchase Cluster ",i,": ",as.double(sum(prod$purchased[prod$clusters==i])/length(prod$purchased[prod$clusters==i])),sep = ""))
  }
  
  #density curves
  plot(density(prod$mean[prod$clusters == 2],adjust=2),lwd=2,col="blue",ylim=c(0,15),xlim=c(0,.8),xlab = "Probability Means", main="Density Curve for Probability Means")
  lines(density(prod$mean[prod$clusters == 1],adjust=2),lwd=2,col="green")
  lines(density(prod$mean[prod$clusters == 3],adjust=2),lwd=2,col="red")
  
  plot(density(prod$sd[prod$clusters == 2],adjust=2),lwd=2,col="blue",ylim=c(0,25),xlim=c(0,.4),xlab = "Probability SDs", main="Density Curve for Probability SDs")
  lines(density(prod$sd[prod$clusters == 1],adjust=2),lwd=2,col="green")
  lines(density(prod$sd[prod$clusters == 3],adjust=2),lwd=2,col="red")
  
  #scatter plot mean vs sd
  plot(prod$mean, prod$sd, col = "white", main="Mean vs. Standard Deviation", xlab = "Mean", ylab="Standard Deviation")
  for(i in 1:nrow(prod)) {
    if(prod$clusters[i]==4)
    {
      points(prod$mean[i], prod$sd[i], col = "black")
    }
    else if(prod$clusters[i]==1)
    {
      points(prod$mean[i], prod$sd[i], col = "green")
    }
    else if(prod$clusters[i]==3)
    {
      points(prod$mean[i], prod$sd[i], col = "red")
    }
    else if(prod$clusters[i]==2)
    {
      points(prod$mean[i], prod$sd[i], col = "blue")
    }
    else
    {
      points(prod$mean[i], prod$sd[i], col = "yellow")
    }
  }
  
  #test
  act <- activity[(activity[,prodIndex] %in% c("2017-01-01", "2017-02-01", "2017-03-01", "2017-04-01", "2017-05-01", "2017-06-01", "2017-07-01", "2017-08-01", "2017-09-01")),]
  customers <- act$RecID
  purchase <- prod[(prod$RecID %in% customers),]
  purchase$MonthOfActivity <- rep(NA,nrow(purchase))
  for(x in 1:nrow(purchase)) {
    for(y in 1:nrow(activity)) {
      if(activity[y,1]==purchase[x,1]) {
        if(!is.na(activity[y,prodIndex])) {
          purchase[x,14] = as.integer(substring(activity[y,prodIndex],6,7))
        }
      }
    }
  }
  prod$MonthOfActivity17 <- rep(NA,rep = nrow(prod))
  for(i in 1:nrow(prod)) {
    for(j in 1:nrow(purchase)) {
      if(prod$RecID[i] == purchase$RecID[j]) {
        prod$MonthOfActivity17[i] = purchase$MonthOfActivity[j]
      }
    }
  }
  prod$purchased17 <- ifelse(!is.na(prod$MonthOfActivity17),1,0)
  for(i in 1:max(prod$clusters)) {
    print(paste("Prob. of Purchase Cluster ",i,": ",as.double(sum(prod$purchased17[prod$clusters==i])/length(prod$purchased17[prod$clusters==i])),sep = ""))
  }
  