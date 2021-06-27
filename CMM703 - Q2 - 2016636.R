#Question 2
library(plyr)
#Test Data Frame
x = data.frame(v=c('Ben','Shaed','Peter','Brian',NA,'Damian','Cassian','Cafka','Wanda','Cafka'),
               k=c(2,7,NA,9,6,2,22,7,5,2), y=c(55,83,41,22,77,49,72,38,NA,153 ), 
               z=c(25,30,50,45,65,15,95,20,40,NA))
x

#Get Mode Function
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

my_funct <- function(a, res){
  
  #2.1 Quantitative or Qualitative variable
  cat('\n --------  2.1 QUNATITATIVE OR QUALITATIVE VARIABLES   -------- \n \n')
  for (val in a){
    if(is.numeric(val)){
      print(paste(val[1],' - is Quantitative'))
    }
    else{
      print(paste(val[1],' - is Qualitative'))
    }
  }
  
  #2.2 Mean, Median, Mode
  cat('\n --------  2.2 MEAN, MEDIAN, MODE   -------- \n \n')
  n = 0
  for (val in a){
    if(is.numeric(val)){
      print(paste('-------------','Variable',n,'-------------'))
      print(paste('Mean:',mean(val, na.rm = TRUE)))
      print(paste('Median:',median(val, na.rm = TRUE)))
      print(paste('Mode:',getmode(val)))
    }
  n = n + 1
  }
  
  #2.3 Handling Missing Values
  cat('\n --------  2.3 MISSING VALUES   -------- \n \n')
  n = 0
  for (val in a){
    print(paste('-------------','Variable',n,'-------------'))
    for (rec in 1:length(val)){
      if(is.na(val[rec])){
        if(is.numeric(val)){
          val[rec] = mean(val, na.rm = TRUE)
        }
        else{
          val[rec] = getmode(val)
        }
      }
    }
    n = n + 1
    print(val)
  }
  
  #2.4 Univariate Outliers
  cat('\n --------  2.4 OUTLIERS   -------- \n \n')
  n = 0
  for (val in a){
    if(is.numeric(val)){
      print(paste('-------------','Variable',n,'-------------'))
      print(val)
      print(paste('Outliers: ',boxplot.stats(val)$out))
    }
    n = n + 1
  }
  
  #2.5 Visualization
  n = 0
  cat('\n --------  2.5 VISUALIZATION   -------- \n \n')
  for (val in a){
    if(is.numeric(val)){
      print(paste('-------------','Variable',n,'-------------'))
      plot(val, main=paste('Variable',n,'Scatterplot'),
           xlab="Index", ylab=paste('Variable',n), col = "red",)
      hist(val, main=paste('Variable',n,'Histogram'),)
    }
    else{
      print(paste('-------------','Variable',n,'-------------'))
      
      pc <- data.frame(x = c(val))
      c = ddply(pc,.(x),nrow)
      print(c[1])
      print(c[2])
      labels <- c(c$x)
      pie(c$V1, labels)
    }
    n = n + 1
  }
}

my_funct(x, x[1])