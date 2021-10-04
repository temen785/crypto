library(devtools)
#install_github("EriqLaplus/discordr")
library(discordr)
library(coinmarketcapr)
library(dplyr)
#install("dplyr")
#install.packages(c("httr", "jsonlite"))
library(httr)
library(jsonlite)

flag2 <- 0
key <- "0a35f78f-0272-466a-9a48-edb2b31231b3"
setup(key)
for (i in 1:25) {
  key <- "0a35f78f-0272-466a-9a48-edb2b31231b3"
  setup(key)
  test <-get_crypto_listings()
  latest_marketcap <- get_global_marketcap('USD')
  btc_dom <-latest_marketcap$btc_dominance
  btc_dom <- round(btc_dom,digits = 2)
  M_cap <-latest_marketcap$USD_total_market_cap*10^(-12)
  M_cap <- round(M_cap,dig=3)
  volm <-latest_marketcap$USD_total_volume_24h*10^-9
  volm <- round(volm,dig=2)
  domin_name <- paste(as.character(btc_dom),"  %")
  
  topG <- test %>% arrange(desc(USD_percent_change_24h))
  topl <- test %>% arrange((USD_percent_change_24h))
  #topG <- dplyr::arrange(topG,desc(USD_percent_change_24h))
  conn_obj <- create_discord_connection(webhook = "https://discord.com/api/webhooks/893599213445853184/9-Zqw4v6KsaWjs7GfmiOLQlWPmyTOyb7z7VbMLk7C9ESZcDunix38P0RbMSuPqIpR2VP",
                                        username = domin_name, set_default = TRUE)
  send_webhook_message(paste0("----------------------", 
                              "\n Market cap: " ,M_cap, " [Trillion USD]",
                              "\n BTC_Dom: ", btc_dom," [%] ",
                              "\n Daily volume: ", volm ," [Billion USD]",
                              "\n ----------------------",
                              "\n BTC: ", round(test$USD_price[which(test$name=="Bitcoin")],dig=1), "  [USDT]   ",
                              "Daily change: ", round(test$USD_percent_change_24h[which(test$name=="Bitcoin")],dig=2), " [%]",
                              "\n ETH:   ", round(test$USD_price[which(test$name=="Ethereum")],dig=1), "   [USDT]   ",
                              "Daily change: ", round(test$USD_percent_change_24h[which(test$name=="Ethereum")],dig=2), " [%]",
                              "\n ADA:     ", round(test$USD_price[which(test$name=="Cardano")],dig = 3) ," [USDT]   ",
                              "Daily change: ", round(test$USD_percent_change_24h[which(test$name=="Cardano")],dig=2), " [%]"
                              ))
  
  if(i%%5==0){
    conn_obj <- create_discord_connection(webhook = 'https://discord.com/api/webhooks/893599656901242900/5gZSMaHMbKzbBdNlaLrkeITsmk0W5OYXV4iUTOftNglLUKDhDiqi2p00DTe62_yqWIL-',
                                        username = ' Daily movers', set_default = TRUE)
  
    send_webhook_message(paste0("----------------------",
                              
                              "\n Top 5 daily Gainers: ",
                              "\n 1) ", topG$name[1], "  ",round(topG$USD_percent_change_24h[1],dig=2), " [%]","  rank: ",topG$cmc_rank[1],
                              "\n 2) ", topG$name[2], "  ",round(topG$USD_percent_change_24h[2],dig=2), " [%]","  rank: ",topG$cmc_rank[2],
                              "\n 3) ", topG$name[3], "  ",round(topG$USD_percent_change_24h[3],dig=2), " [%]","  rank: ",topG$cmc_rank[3],
                              "\n 4) ", topG$name[4], "  ",round(topG$USD_percent_change_24h[4],dig=2), " [%]","  rank: ",topG$cmc_rank[4],
                              "\n 5) ", topG$name[5], "  ",round(topG$USD_percent_change_24h[5],dig=2), " [%]","  rank: ",topG$cmc_rank[5],
                              
                              "\n ----------------------", 
                              
                              "\n Top 5 daily Losers: ",
                              "\n 1) ", topl$name[1], "  ",round(topl$USD_percent_change_24h[1],dig=2), " [%]","  rank: ",topl$cmc_rank[1],
                              "\n 2) ", topl$name[2], "  ",round(topl$USD_percent_change_24h[2],dig=2), " [%]","  rank: ",topl$cmc_rank[2],
                              "\n 3) ", topl$name[3], "  ",round(topl$USD_percent_change_24h[3],dig=2), " [%]","  rank: ",topl$cmc_rank[3],
                              "\n 4) ", topl$name[4], "  ",round(topl$USD_percent_change_24h[4],dig=2), " [%]","  rank: ",topl$cmc_rank[4],
                              "\n 5) ", topl$name[5], "  ",round(topl$USD_percent_change_24h[5],dig=2), " [%]","  rank: ", topl$cmc_rank[5]
    ))
    }
  #----------Fear and greed index-----
  FandG <-httr::GET("https://api.alternative.me/fng/")
  data <- fromJSON(rawToChar(FandG$content))
  flag <- data$data$value
 
    
  if((data$data$time_until_update>=86000)| (flag2 != flag)){
    conn_obj <- create_discord_connection(webhook = "https://discord.com/api/webhooks/893599545412423721/gWYDXNhXNQnGIvQN6P_WoVGm7RG6fLZ1kLsfhwPX34D-w4HxtM3h8QwTTzxVv_Ap7p0T",
                                          username = ' Sell Greed Buy Fear', set_default = TRUE)
    send_webhook_message(paste0("\n ----------------------",
                                "\n " , data$name, " : ", data$data$value ," --" ,data$data$value_classification," --",
                                "\n ----------------------") )
    flag2 <- data$data$value
  }
  #test$name[which(test$USD_percent_change_24h==max(test$USD_percent_change_24h))]," ", round(max(test$USD_percent_change_24h),dig=1), " [%]"
  print(data$data$time_until_update)       
  Sys.sleep(600)
}




FV <- data$name


