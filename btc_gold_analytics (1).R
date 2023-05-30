# this r file is aiming to analyze the correlation between the 
# historical price of the cryptocurrency bitcoin and gold 
# since November 1st, 2014.
# this will use ggplot2 to visualize the graph and to provide 
# visual clues as to where certain events happened that caused the
# price of both to rise or fall

# install package ggplot2 and zoo(used for rolling correlation)

  install.packages("ggplot2")
  install.packages("zoo")
  library(ggplot2)
  library(zoo)

# load in both csv files, btc price history and gold price history
  btc <- read.csv("btc_weekly.csv")
  gold_pre <- read.csv("gold_weekly.csv")

# we only need the closing price of both commodities here, as
# well as the date

  btc <- data.frame(btc[6])
  gold_pre <- data.frame(gold_pre[2])

# the data frames are sorted oppositely, sort gold in ascending order

  gold_pre <- as.data.frame(apply(gold_pre, 2, rev))

# cut top value

  gold <- as.data.frame(gold_pre[-1,])
  colnames(gold)[1] <- "Price"

# note that the time that the data was measured varies slightly

# now, how to analyze data... goal here is to see 
# 1) how correlated the price of each commodity are
# 2) are there significant drops in the price of each that could be 
# explained by an effect on one
# 3) visualize the graph

  # firstly, we will visualize the price in terms of # of weeks
  # that have passed since ~ 11/01/2014. There are some reasons
  # this is done and not individual dates. The main reason is that
  # the price of oil is not traded/counted during the weekdays, 
  # which results in significantly less data as BTC is traded
  # 24/7. Secondary, we can ditch the date and instead use weeks since 
  # 11/01/2014. This is less useful but matches up for the sake of the 
  # provided data a little better.
  
  # this is hardly useful in itself, however we will progress
  # and attempt to find weeks that are strongly correlated

  ggplot(data = btc, aes(x = seq_along(Adj.Close), y = Adj.Close)) +
    geom_line() +
    scale_x_continuous(limits = c(1, nrow(btc)), expand = c(0, 0)) +
    xlab("Weeks") +
    ylab("Dollar Value")
  
  gold$Price <- as.numeric(gsub(",", "", gold$Price))
  
  ggplot(data=gold, aes(x = as.numeric(seq_along(Price)), y = Price)) +
    geom_line(na.rm = TRUE) +
    scale_y_continuous(limits = c(1000, 2100)) +
    scale_x_continuous(limits = c(0, 500), expand = c(0, 0)) +
    xlab("Weeks") +
    ylab("Dollar Value")

# comparing both data frames

  # pearson correlation

  pearson_cor <- cor(btc$Adj.Close, gold$Price, method = "pearson")
  
  # 0.755727
  # pearson correlation reports strong correlation
  # however, these are financial commodities and as such,
  # are likely to already be strongly correlated 
  
  # next, we will graph a scatterplot, using both btc and gold, and 
  # attempt to put a smoothing line on it.
  #
  # to do this, we need to standardize the pricing. the scatterplot
  # would not be useful given that btc max is >60000 and gold max is 
  # 2000. so, we will normalize the price of btc and gold 
  
  btc_normalized <- as.data.frame((btc$Adj.Close - min(btc$Adj.Close)) / 
                       (max(btc$Adj.Close) - min(btc$Adj.Close)))
  colnames(btc_normalized)[1] <- "Adj.Close"
  
  gold_normalized <- as.data.frame((gold$Price - min(gold$Price)) / 
    (max(gold$Price) - min(gold$Price)))
  colnames(gold_normalized)[1] <- "Price"
  
  ggplot() +
    geom_point(data = btc_normalized, aes(x = 1:length(Adj.Close), 
               y = Adj.Close), color = "blue") +
    geom_smooth(data = btc_normalized, aes(x = 1:length(Adj.Close), 
                                           y = Adj.Close), color = "blue") +
    geom_point(data = gold_normalized, aes(x = 1:length(Price), y = Price), 
               color = "red") +
    geom_smooth(data = gold_normalized, aes(x = 1:length(Price), y = Price), 
                color = "red") +
    xlab("Weeks") + 
    ylab("Normalized Price")
  
  # now there is a normalized scatterplot that shows how gold and btc
  # rise and fall together. note that the trend line is LOESS, or locally
  # estimated scatterplot smoothing.
  
  # what this next graph shows is a single scatterplot that displays
  # points on the graph representing the price of both assets on a
  # given week.
  
  comb_norm <- cbind(btc_normalized, Price = gold_normalized$Price)
  colnames(comb_norm)[1] = "BtcPrice"
  colnames(comb_norm)[2] = "GoldPrice"
  
  correlation <- cor(comb_norm$BtcPrice, comb_norm$GoldPrice)
  
  ggplot(comb_norm, aes(x = BtcPrice, y = GoldPrice)) +
    geom_point() +
    geom_smooth(method = "loess", se = FALSE, color = "blue") +
    xlab("Btc") +
    ylab("Gold") +
    ggtitle(paste("Scatterplot of Historical Prices (Correlation =", 
                  round(correlation, 2), ")")) +
    theme(plot.title = element_text(hjust = 0.5))
  
  
  # next: attempt to find periods of time where the correlation is 
  # strongest, weakest. for this purpose i think that importing another
  # library, zoo, will work best for finding rolling correlation
  
  # this will be done in a three-week rolling correlation
  
  three_week_roll <- rollapply(comb_norm, width=3, function(x) cor(x[,1],x[,2]),
                              by.column=FALSE)
  
  three_week_roll <- as.data.frame(three_week_roll)
  colnames(three_week_roll)[1] <- "RollCor"
  
  ggplot(data = three_week_roll, aes(x = seq_along(RollCor), 
                                     y = RollCor)) +
    geom_line() +
    geom_smooth(method = "loess", se=FALSE, color = "red") +
    xlab("Weeks") +
    ylab("Correlation Value")
    
  # this graph looks visually unappealing, as the correlation is all over
  # the place. however, the LOESS line does show the areas (or more
  # broadly, the vague years), where correlation is either positive
  # or negative. however, merely by looking at it, we can see that there are 
  # three areas of interest of this high level view.
  # 100 - 200 weeks strong correlation
  # 200 - 325 weeks negative correlation
  # 325 - 500 weeks strong correlation
  
  
  
