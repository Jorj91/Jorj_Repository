library(pacman)

p_load(cowplot,arules,arulesViz, TSP, prabclus, Matrix, trimcluster,lattice,caret,rpart,rpart.plot,corrplot,RColorBrewer,randomForest,plotly,data.table,dplyr,doParallel,grid,crayon)
#######################

# Prepare Parallel Process
cluster <- makeCluster(detectCores() - 1) # convention to leave 1 core for OS
registerDoParallel(cluster)

#import the data set as a data frame
transactiondf <- read.csv("C:\\Users\\giorgia\\Documents\\Giorgia Felline\\UBIQUM\\2. PREDICTING CUSTOMERS PREFERENCES\\Task_4_Discover Associations Between Products\\ElectronidexTransactions2017.csv",header = FALSE)

#import the data set as a trasaction list
transactionslist <- read.transactions("C:\\Users\\giorgia\\Documents\\Giorgia Felline\\UBIQUM\\2. PREDICTING CUSTOMERS PREFERENCES\\Task_4_Discover Associations Between Products\\ElectronidexTransactions2017.csv",
                                      format = "basket", 
                                      sep = ",")

head(transactiondf)
View(transactiondf)

#create categories for each group of items
laptop <- c("LG Touchscreen Laptop",
            "Acer Aspire",
            "HP Laptop",
            "ASUS Chromebook",
            "Apple MacBook Pro",
            "Apple MacBook Air",
            "Dell Laptop",
            "Eluktronics Pro Gaming Laptop",
            "Alienware AW17R4-7345SLV-PUS 17 Laptop",
            "HP Notebook Touchscreen Laptop PC",
            "Alienware Laptop")

desktop <- c("Lenovo Desktop Computer",
             "iMac",
             "HP Desktop",
             "ASUS Desktop",
             "Dell Desktop",
             "Intel Desktop",
             "Acer Desktop",
             "CYBERPOWER Gamer Desktop",
             "Dell 2 Desktop")

monitor <- c("Acer Monitor",
             "LG Monitor",
             "ASUS Monitor",
             "ASUS 2 Monitor",
             "Dell Monitor",
             "Samsung Monitor",
             "Sceptre Monitor",
             "ViewSonic Monitor",
             "AOC Monitor",
             "HP Monitor")

mouse <- c("3-Button Mouse",
           "Logitech Wireless Mouse",
           "Microsoft Basic Optical Mouse",
           "Logitech 3-button Mouse",
           "Redragon Gaming Mouse",
           "HP Wireless Mouse",
           "Generic Black 3-Button",
           "Wireless Portable Mouse",
           "Gaming Mouse Professional",
           "Slim Wireless Mouse")

keyboard <- c("HP USB Keyboard",
              "Logitech Wireless Keyboard",
              "Rii LED Keyboard",
              "Logitech Keyboard",
              "Backlit LED Gaming Keyboard",
              "Dell Wired Keyboard",
              "Apple Wired Keyboard",
              "Apple Wireless Keyboard",
              "Apple Magic Keyboard")

mousekeycombo <- c("Logitech MK550 Wireless Wave Keyboard and Mouse Combo",
                   "Logitech Desktop MK120 Mouse and keyboard Combo",
                   "Logitech MK270 Wireless Keyboard and Mouse Combo",
                   "Dell KM117 Wireless Keyboard & Mouse",
                   "EagleTec Wireless Combo Keyboard and Mouse",
                   "Microsoft Wireless Comfort Keyboard and Mouse",
                   "Microsoft Wireless Desktop Keyboard and Mouse",
                   "Rii LED Gaming Keyboard & Mouse Combo",
                   "Logitech MK360 Wireless Keyboard and Mouse Combo")

compheadphone <- c("Zombie Gaming Headset",
                   "Logitech ClearChat Headset",
                   "Panasonic On-Ear Stereo Headphones RP-HT21",
                   "PC Gaming Headset",
                   "Kensington Headphones",
                   "Logitech Stereo Headset",
                   "Koss Home Headphones",
                   "Microsoft Headset",
                   "Ailihen Stereo Headphones",
                   "XIBERIA Gaming Headset")

actvheadphone <- c("Apple Earpods",
                   "Monster Beats By Dr Dre",
                   "Otium Wireless Sports Bluetooth Headphone",
                   "Panasonic In-Ear Headphone",
                   "APIE Bluetooth Headphone",
                   "Philips Flexible Earhook Headphone",
                   "Panasonic On-Ear Stereo Headphones")

cords <- c("HDMI Cable 6ft",
           "Ethernet Cable",
           "Etekcity Power Extension Cord Cable",
           "Audio Cable",
           "VGA Monitor Cable",
           "iPhone Charger Cable",
           "HDMI Adapter",
           "USB Cable",
           "Samsung Charging Cable")

software <- c("Microsoft Office Home and Student 2016",
              "Computer Game")

accesories <- c ("Belkin Mouse Pad",
                 "Large Mouse Pad")

speaker <- c("Cambridge Bluetooth Speaker",
             "JBL Splashproof Portable Bluetooth Speaker",
             "DOSS Touch Wireless Bluetooth",
             "Logitech Multimedia Speakers",
             "Rokono Mini Speaker",
             "Cyber Acoustics",
             "Bose Companion Speakers",
             "Mackie CR Speakers",
             "Sonos")

printer <- c("Epson Printer",
             "HP Wireless Printer",
             "Canon Office Printer",
             "Brother Printer",
             "DYMO Label Manker")

printerink <- c("Epson Black Ink",
                "HP Black & Tri-color Ink",
                "Canon Ink",
                "Brother Printer Toner",
                "DYMO Labeling Tape")

computerstand <- c("Halter Acrylic Monitor Stand",
                   "Height-Adjustable Standing Desk",
                   "Multi Media Stand",
                   "Halter Mesh Metal Monitor Stand",
                   "Full Motion Monitor Mount")

tablets <- c("iPad",
             "iPad Pro",
             "Fire HD Tablet",
             "Samsung Galaxy Tab",
             "Samsung Galaxy Tablet",
             "Kindle")


harddrive <- c("1TB Portable External Hard Drive",
               "2TB Portable External Hard Drive",
               "5TB Desktop Hard Drive",
               "Slim 2TB Portable External Hard Drive",
               "3TB Portable External Hard Drive")

smarthomedevice <- c("Apple TV",
                     "Google Home",
                     "Smart Light Bulb",
                     "Fire TV Stick",
                     "Roku Express")

#subset the trasactions by BUSSINES OR RETAIL.

#initialize vector with the indexes of transactions by category
item = 0
business <- c()
retail <- c()


#making rules to split the trasactions in business and retail.
for (m in 1:nrow(transactiondf)){
  trasactionsvector <- unname(unlist(transactiondf[m,]))
  trasactionsvector <- trasactionsvector[!trasactionsvector %in% ""] #remove empty columns
  item = length(trasactionsvector)
  
  if ((length(which(trasactionsvector %in% desktop | trasactionsvector %in% laptop)) >= 3) | 
      (item > 10) | 
      (length(which(trasactionsvector %in% monitor)) >= 3) |
      (length(which(trasactionsvector %in% printer)) >= 3) |
      (length(which(trasactionsvector %in% tablets)) >= 3) |
      (length(which(trasactionsvector %in% compheadphone | trasactionsvector %in% actvheadphone)) >= 6) |
      (length(which(trasactionsvector %in% mouse | trasactionsvector %in% keyboard)) >= 4) |
      (length(which(trasactionsvector %in% mousekeycombo)) >= 3) |
      (length(which(trasactionsvector %in% cords)) >= 4) |
      (length(which(trasactionsvector %in% accesories)) >= 4) |
      (length(which(trasactionsvector %in% software)) >= 3) |
      (length(which(trasactionsvector %in% speaker)) >= 3) |
      (length(which(trasactionsvector %in% printerink)) >= 4) |
      (length(which(trasactionsvector %in% computerstand)) >= 3) |
      (length(which(trasactionsvector %in% harddrive)) >= 3) |
      (length(which(trasactionsvector %in% printerink)) >= 3)
      ){
    business <- append(business,m)
  }
                                
}


retail <- c(1:nrow(transactiondf))
retail <- retail[!retail %in% business ]

business #indexes of business trasactions
retail   #indexes of retail trasactions

#create two trasaction list data sets, one for business and one for retail
transactionsbusiness <- transactionslist[-c(retail),] #removes the retails trasactions

transactionsretail <- transactionslist[-c(business),] #removes the business trasactions

#check transactions
transactionsbusiness
transactionsretail


#You can view the transactions.
inspect(transactionslist)   

#check an specific transaction
inspect(transactionslist[259]) 

#check an specific transaction
inspect(transactionslist[862])

#change the rules of visualization
inspect(transactionslist,ruleSep = "---->", itemSep = " // ", setStart = "***", setEnd ="***", 
        linebreak = TRUE)       

# Number of transactions.
length(transactionslist) 

# Number of items per transaction
hist(size(transactionslist))
#with ggplot
hist(size(transactionslist) , breaks=40 , col=rgb(0.2,0.8,0.5,0.5) , border=F, main="Frequency of numbers of items",xlab = "Number of items", xlim=c(0,30),ylim=c(0,2500))

#Draw the boxplot and the histogram 
toptrasactions <-which(size(transactionslist) > 29)
toptrasactions

# Lists the transactions by conversion (LIST must be capitalized). You can access each trasaction with [[i][j]
LIST(transactionslist)   

# To see the item labels
itemLabels(transactionslist) 

#count how many distinc items are in the trasaction set
length(itemInfo(transactionslist)[[1]])


#initialize levels, missing values names, missing values index, and missing item counter.
firstlevel <- c()
naindex <- c()
nanames <- c()
cont <- 0

#testing the way of accesing the "data.frame"transaction-list"
itemInfo(transactionslist)[[1]][125]
itemInfo(transactionslist)[[1]][3] %in% mouse

c(transactionsbusiness,transactionsretail)

#creates a fuction to add hierarchy and categories
newfunction<-function(Data){
  
#add Hierarchy - A Level to the BUSINESS trasaction list.
for (i in 1:length(itemInfo(Data)[[1]])){
  if (i == 1){
    firstlevel <- c()
    naindex <- c()
    nanames <- c()
    cont <- 0
  }
  if (itemInfo(Data)[[1]][i] %in% laptop){
    firstlevel[i] <- "laptop"
  }
  else if (itemInfo(Data)[[1]][i] %in% desktop){
    firstlevel[i] <- "desktop"
  }
  else if (itemInfo(Data)[[1]][i] %in% monitor){
    firstlevel[i] <- "monitor"
  }
  else if (itemInfo(Data)[[1]][i] %in% mouse){
    firstlevel[i] <- "mouse"
  }
  else if (itemInfo(Data)[[1]][i] %in% keyboard){
    firstlevel[i] <- "keyboard"
  }
  else if (itemInfo(Data)[[1]][i] %in% mousekeycombo){
    firstlevel[i] <- "mouse and keyboard combo"
  }
  else if (itemInfo(Data)[[1]][i] %in% compheadphone){
    firstlevel[i] <- "computer headphones"
  }
  else if (itemInfo(Data)[[1]][i] %in% actvheadphone){
    firstlevel[i] <- "active headphones"
  }
  else if (itemInfo(Data)[[1]][i] %in% cords){
    firstlevel[i] <- "computer cords"
  }
  else if (itemInfo(Data)[[1]][i] %in% accesories){
    firstlevel[i] <- "accesories"
  }
  else if (itemInfo(Data)[[1]][i] %in% speaker){
    firstlevel[i] <- "speaker"
  }
  else if (itemInfo(Data)[[1]][i] %in% printer){
    firstlevel[i] <- "printer"
  }
  else if (itemInfo(Data)[[1]][i] %in% printerink){
    firstlevel[i] <- "printer ink"
  }
  else if (itemInfo(Data)[[1]][i] %in% computerstand){
    firstlevel[i] <- "computer stand"
  }
  else if (itemInfo(Data)[[1]][i] %in% tablets){
    firstlevel[i] <- "tablets"
  }
  else if (itemInfo(Data)[[1]][i] %in% harddrive){
    firstlevel[i] <- "external hardrive"
  }
  else if (itemInfo(Data)[[1]][i] %in% smarthomedevice){
    firstlevel[i] <- "smart home devices"
  }
  else if (itemInfo(Data)[[1]][i] %in% software){
    firstlevel[i] <- "software"
  }
  #check for NA values, products that don't match a category
  else{
    naindex <- c(naindex,i)
    productname <- itemInfo(Data)[[1]][i]
    nanames <- c(nanames,productname)
    cont = cont + 1 
     }
}
#warning the tells the products without category
sprintf("There are %i products that dont corrrespond to a category, please review", cont)

#check the names of the products without category
nanames

#check the index of the products without category
naindex

#select with the fuction which() the true value for the vector is.na - it means picking the NA values (another way of doing it)
which(is.na(firstlevel))

#check the hierarchy
firstlevel

#assing the hierarchy to the first level of your trasactions data set.
Data@itemInfo$level1 <- firstlevel

#check for an empty transaction
emptytrasactions <-which(size(Data) == 0)
emptytrasactions
#inspect the empty trasactions in your dataset
if (length(emptytrasactions) != 0){
  for (i in 1:length(emptytrasactions)){
    contenido <- emptytrasactions[i]
    inspect(Data[contenido])
  }
} else {print("there are no empty transactions")}
  

#remove the empty trasactions and create a new dataset (two trasactions were removed)
#one way of removing values. pick the empty values and substract the rows
if (length(emptytrasactions) != 0){
  Data <- Data[-c(emptytrasactions),] 
  Data
}

return(Data)}

transactionsbusiness <- newfunction(transactionsbusiness)

transactionsretail <- newfunction(transactionsretail)
#another way way of removing values. picks the rows with values, ignores the empty. 
#transactionslist <- transactionslist[which(size(transactionslist)!=0),] 

transactionsbusiness
transactionsretail

transactionsbusinesslvl <- aggregate(transactionsbusiness, by = "level1")

transactionsretaillvl <- aggregate(transactionsretail, by = "level1")


#plot using relative frequency the top 20 items - with arules itemfrequency plot function

p1 <- itemFrequencyPlot(transactionsbusiness, type = "relative", weighted = FALSE, topN = 20,cex.names = 0.8,ylim=c(0,1),col=rgb(0.2,0.8,0.5,0.5),main = "Product Type Frequency - Business")
p2 <- itemFrequencyPlot(transactionsretail, type = "relative", weighted = FALSE, topN = 20,cex.names = 0.65, ylim=c(0,0.2),col=rgb(0.2,0.8,0.5,0.5),main = "Product Type Frequency - Retail")
p3 <- itemFrequencyPlot(transactionsbusinesslvl, type = "relative", weighted = FALSE, topN = 20,cex.names = 0.8,ylim=c(0,1),col=rgb(0.2,0.8,0.5,0.5),main = "Category Frequency - Business")
p4 <- itemFrequencyPlot(transactionsretaillvl, type = "relative", weighted = FALSE, topN = 20,cex.names = 0.65,ylim=c(0,0.5),col=rgb(0.2,0.8,0.5,0.5),main = "Category Frequency - Retail")



#You can visualize all of the transactions within your dataset.
image(transactionslist)
image(sample(transactionsbusinesslvl,15))

#rules selection using the apriori fuction: Support, Confidence and Lift
rulesbusiness <- apriori (transactionsbusiness, parameter = list(supp = 0.01, conf = 0.6, maxlen =10, minlen =2))
rulesretail <- apriori (transactionsretail, parameter = list(supp = 0.001, conf = 0.3, maxlen =10, minlen =2))
rulesbusinesslvl <- apriori (transactionsbusinesslvl, parameter = list(supp = 0.01, conf = 0.6, maxlen =10, minlen =2))
rulesretaillvl <- apriori (transactionsretaillvl, parameter = list(supp = 0.001, conf = 0.3, maxlen =10, minlen =2))

rulesset <- list(rulesbusiness, rulesretail, rulesbusinesslvl, rulesretaillvl)
rulesset

#check redundant rules
for (i in 1:length(rulesset)){
  rulesset[[i]] <- rulesset[[i]][!is.redundant(rulesset[[i]])]
  
}

#remove redundat rules  
rulesbusiness<- rulesset[[1]]
rulesretail<- rulesset[[2]]
rulesbusinesslvl<- rulesset[[3]]
rulesretaillvl<- rulesset[[4]]

#final rules
rulesset

#The top three rules with respect to the Support, Lift, Confidence
for (p in c("support","confidence","lift")){
    cat(red("######################################################################################################################"))
  
  for (o in 1:length(rulesset)){
    inspect(head(sort(rulesset[[o]], by = p),10))
     }
}


#top 10 rules by lift BUSINESS SUBSET
inspect(head(sort(rulesset[[1]], by = "lift"),10))
inspect(head(sort(rulesset[[3]], by = "lift"),10))

#filter rules by Product Type and Category
business_iMac <- apriori(transactionsbusiness, parameter = list(supp=0.0005, conf=0.1,maxlen =10, minlen =2),appearance = list(default="rhs",lhs="iMac"))
business_Hplaptop <- apriori(transactionsbusiness, parameter = list(supp=0.005, conf=0.6,maxlen =10, minlen =2),appearance = list(default="lhs",rhs="HP Laptop"))
business_lenovodesktop <- apriori(transactionsbusiness, parameter = list(supp=0.010, conf=0.6,maxlen =10, minlen =2),appearance = list(default="lhs",rhs="Lenovo Desktop Computer"))
business_gamerpc <- apriori(transactionsbusiness, parameter = list(supp=0.005, conf=0.6,maxlen =10, minlen =2),appearance = list(default="lhs",rhs="CYBERPOWER Gamer Desktop"))


business_iMac <- business_iMac[!is.redundant(business_iMac)]
business_Hplaptop <- business_Hplaptop[!is.redundant(business_Hplaptop)]
business_lenovodesktop <- business_lenovodesktop[!is.redundant(business_lenovodesktop)]
business_gamerpc <- business_gamerpc[!is.redundant(business_gamerpc)]

business_iMac
business_Hplaptop
business_lenovodesktop
business_gamerpc

inspect(head(sort(business_iMac, by = "lift"),50))
inspect(head(sort(business_Hplaptop, by = "lift"),10))
inspect(head(sort(business_lenovodesktop, by = "lift"),10))
inspect(head(sort(business_gamerpc, by = "lift"),10))


#top 10 rules by lift RETAIL SUBSET
inspect(head(sort(rulesset[[2]], by = "lift"),10))
inspect(head(sort(rulesset[[3]], by = "lift"),10))

#Using Rule Explorer
ruleExplorer(rulesset[[1]]) #Bussiness
ruleExplorer(rulesset[[2]]) #Retail
ruleExplorer(rulesset[[3]]) #Bussiness Level
ruleExplorer(rulesset[[4]]) #Retail Level


######## PLOTS BY CODE ######
plot(rulesset[[1]], measure=c("support", "confidence"), shading="lift", interactive=TRUE)
plot(rulesset[[2]], measure=c("support", "confidence"), shading="lift", interactive=TRUE)
plot(rulesset[[3]], measure=c("support", "confidence"), shading="lift", interactive=TRUE)
plot(rulesset[[4]], measure=c("support", "confidence"), shading="lift", interactive=TRUE)


#xaxis: support, yaxis: confidence shading: lift
for (o in 1:length(rulesset)){
    plot(rulesset[[o]], method = "scatter", measure=c("support", "confidence"), shading="lift")
}
#Two key plot
for (o in 1:length(rulesset)){
  plot(rulesset[[o]], shading="order", control=list(main = "Two-key plot"))
}

#Graph plot for the first 20 rules
for (o in 1:length(rulesset)){
  plot(rulesset[[o]][1:20], method="graph", control=list(type="items"))
}

#general plot for the quality of each rule
rulesbusiness@quality
plot(quality(rulesbusiness))

##################################################################