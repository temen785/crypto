library(coinmarketcapr)


key <- "0a35f78f-0272-466a-9a48-edb2b31231b3"
setup(key)
latest_marketcap <- get_global_marketcap('USD')
btc_dom <-latest_marketcap$btc_dominance
M_cap <-latest_marketcap$USD_total_market_cap*10^(-12)
M_cap <- round(M_cap,dig=3)
btc_dom <- round(btc_dom,digits = 2)
print(paste0("BTC_Dom: ", btc_dom," % "))
print(paste0("Market cap:" ,M_cap, "  Trillion USD"))
datatop100 <-get_crypto_listings()
head(datatop100)
datatop100$symbol
names(datatop100)
names_to_watch <- c("BTC","ETH","ADA","DOT","AR", "ONE","RUNE")
avg_price <- numeric()
price <- numeric()
diference <- numeric()
loop <- function(names_to_watch){
  latest_marketcap <- get_global_marketcap('USD')
  datatop100 <-get_crypto_listings()
  names_to_watch_to_calc <- names_to_watch
  avg_price <- c(64097,3122,1.1185,30.92,44.48,0.0808356,7.495)
  for(i in 1:length(names_to_watch_to_calc)){
   price[i] <- datatop100$USD_price[which(names_to_watch_to_calc[i]==datatop100$symbol)]
   diference[i]<-((price[i]-avg_price[i])/avg_price[i])*100
  }
  avg_price <- round(avg_price,dig=2)
  price <-round(price,dig=2)
  diference <- round(diference, dig=2)
  outp <- data.frame(names_to_watch_to_calc,price,diference,avg_price)
  colnames(outp) <- c("Coin name","Price now [USDT]", "diff [%] ", "AVG price [USDT] " )
  return(outp)
}
ll <-loop(names_to_watch = names_to_watch)
print(ll)
