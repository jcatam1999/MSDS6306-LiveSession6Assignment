# Author: Jeff Chandler
library(plyr) 
#set the working directory
setwd('C:\\RProjects\\LiveSession6Assign\\Data')
#set the files you wish to process through t his code
Files = c('Manhatten2011.csv', 'BrooklynSales.csv','rollingsales_bronx.csv', 'rollingsales_queens.csv', 'rollingsales_statenisland.csv' )
#Loop through each file and perform some clean up
for(file in Files){
  #File you are processing
  print(file)
  bk = read.csv(file, sep = ',') 

 
  #create new column for price and clean it up
  bk$SALE.PRICE.N = as.numeric(gsub("[^[:digit:]]","", bk$SALE.PRICE))
  bk$SALE.PRICE.N[is.na(bk$SALE.PRICE.N)] <- 0
  count(is.na(bk$SALE.PRICE.N)) 
  #change column names to lower case
  names(bk) = tolower(names(bk))


  ## clean/ format the data with regular expressions 
  bk$gross.sqft = as.numeric(gsub("[^[:digit:]]","", bk$gross.square.feet)) 
  bk$land.sqft = as.numeric(gsub("[^[:digit:]]","", bk$land.square.feet))
  bk$sale.date = as.Date(bk$sale.date, 'm/d/y') 
  
  #print summary of fields I am interested in
  print(summary(bk$sale.price.n))
  print(summary(bk$gross.square.feet))
  print(summary(bk$land.square.feet))
  

  ## do a bit of exploration to make sure there's not anything ## weird going on with sale prices 
  attach(bk) 
  hist(sale.price.n, main = file) 
  hist(sale.price.n[sale.price.n > 0], main = file) 
  detach(bk) 

  ## keep only the actual sales 
  bk.sale = bk[ bk$sale.price.n > 0,] 
  #print out hisogram to PDF
  HistFile = paste('Hist',file,'.pdf', sep = '_')
  pdf(HistFile)
  hist(bk.sale$sale.price.n[bk.sale$sale.price.n < 500000], breaks = 5, xlim=c(50000, 500000), main = file)
  dev.off()
  plot(bk.sale$gross.sqft, bk.sale$sale.price.n, main = file) 
  plot(log(bk.sale$gross.sqft), log(bk.sale$sale.price.n), main = file)
  
 
  ## for now, let's look at 1-, 2-, and 3-family homes 
  bk.homes = bk.sale[which(grepl("FAMILY", bk.sale$building.class.category)),] 
  plot( log(bk.homes$gross.sqft), log(bk.homes$sale.price.n), main = file)
  bk.homes[which(bk.homes$sale.price.n < 100000),] [order(bk.homes[which(bk.homes$Sale.price.n < 100000),] $sale.price.n),] 
  
  ## remove outliers that seem like they weren't actual sales 
  bk.homes$outliers = (log( bk.homes$sale.price.n) <= 5) + 0 
  bk.homes = bk.homes[ which( bk.homes$outliers == 0),] 
  #print out plot to pdf
  plotfile = paste('plot', file,'.pdf',sep = '_')
  pdf(plotfile)
  plot(log(bk.homes$gross.sqft), log(bk.homes$sale.price.n), main = file)
  dev.off()

  OutFile = paste('Clean',file,sep = '_')
  write.csv( bk, file = OutFile)
}  

