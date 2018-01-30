library(shiny)
library(shinyjs)
library(shinyBS)
library(bizdays)

#analysis packages
library(dplyr)
library(tidyr)
library(quantmod)
library(fTrading)
library(TTR)

#plotting packages
library(rbokeh)

rm(list=ls())

#important variables
#pathDir = dirname(rstudioapi::getSourceEditorContext()$path)
dflt_cal = create.calendar("default_cal", holidaysANBIMA, weekdays=c("saturday", "sunday"))

init_date <- "2015-12-29" #default
start_date <- "2015-12-30" #default
end_date <- "2018-01-02" #default
indexETF <- 'SPY'
stock_1 <- "AAPL" #default
symbols <- c("SPY", "AAPL") #default
adjustment <- TRUE #default

index_all = NULL #declare
stock1_all = NULL #declare

function(input, output, session) {
  
  addTooltip(session, id="stockInput", title=HTML('<font size="1">Select the stock you want to backtest a strategy on. The selection of stocks include the top 100 companies in the S&P 500 and some ETFs</font>'),
             placement = "bottom", trigger = "hover", options = list(delay = list(show=5000)))
  addTooltip(session, id="entryStrat", title=HTML('<font size="1">This sets the indicator to be used for the exit strategy. if the indicator is below the set value, the strategy will buy the security</font>'),
             placement = "bottom", trigger = "hover", options = list(delay = list(show=5000)))
  addTooltip(session, id="exitStrat", title=HTML('<font size="1">This sets the indicator to be used for the exit strategy. if the indicator is above the set value, the strategy will sell the security if we have it.  The backtest is long-only and will not short the security</font>'),
             placement = "bottom", trigger = "hover", options = list(delay = list(show=5000)))
  addTooltip(session, id="buyAtStart", title=HTML('<font size="1">Selecting yes means that the backtest will start with a long position at the start of the backtest period</font>'),
             placement = "bottom", trigger = "hover", options = list(delay = list(show=5000)))
  
  addTooltip(session, id="enterSMAset", title=HTML('<font size="1">The simple moving average is the most commonly used indicator to identify support and resistance</font>'),
             placement = "bottom", trigger = "hover", options = list(delay = list(show=5000)))
  addTooltip(session, id="enterRSIset", title=HTML('<font size="1">An RSI below 30 means that a stock is oversold</font>'),
             placement = "bottom", trigger = "hover", options = list(delay = list(show=5000)))
  addTooltip(session, id="enterStochset", title=HTML('<font size="1">A stochastic oscillator reading below 0.20 means that the stock is oversold</font>'),
             placement = "bottom", trigger = "hover", options = list(delay = list(show=5000)))
  
  addTooltip(session, id="exitSMAset", title=HTML('<font size="1">The simple moving average is the most commonly used indicator to identify support and resistance</font>'),
             placement = "bottom", trigger = "hover", options = list(delay = list(show=5000)))
  addTooltip(session, id="exitRSIset", title=HTML('<font size="1">An RSI above 70 means that a stock is overbought</font>'),
             placement = "bottom", trigger = "hover", options = list(delay = list(show=5000)))
  addTooltip(session, id="exitStochset", title=HTML('<font size="1">A stochastic oscillator reading above 0.80 means that the stock is overbought</font>'),
             placement = "bottom", trigger = "hover", options = list(delay = list(show=5000)))
  
  t1 <- eventReactive(input$button1, {
    HTML(paste0('<div style="text-align:center"><font size="2"><b>',input$stockInput, ' with SMA, RSI, and Stochastics</b></font></div>'))
  })
  
  output$text_1 <- renderText({
    t1()
  })
  
  t2 <- eventReactive(input$button2, {
    HTML(paste0('<div style="text-align:center"><font size="2"><b>',input$stockInput, ' Backtesting Results</b></font></div>'))
  })
  
  output$text_2 <- renderText({
    t2()
  })
  
  
  
  p1 <- eventReactive(input$button1, {
    init_date <- "2016-12-29" #default
    start_date <- "2016-12-30" #default
    end_date <- "2018-01-02" #default
    indexETF <- 'SPY'
    stock_1 <- "AAPL" #default
    symbols <- c("SPY", "AAPL") #default
    adjustment <- FALSE #default
    
    init_date <- adjust.previous(adjust.previous(input$date1, dflt_cal)-1, dflt_cal)
    start_date <- adjust.previous(input$date1, dflt_cal)
    end_date <- adjust.next(input$date2, dflt_cal)
    stock_1 <- input$stockInput
    
    index_all = getSymbols(Symbols = c(indexETF), src = "yahoo", index.class = "POSIXct",
                                 from = start_date, to = end_date, adjust = adjustment, auto.assign = FALSE)
    stock1_all = getSymbols(Symbols = c(stock_1), src = "yahoo", index.class = "POSIXct",
                             from = start_date, to = end_date, adjust = adjustment, auto.assign=FALSE)
    close_base1 = data.frame(cbind(index_all[,6],stock1_all[,6]), date=index(index_all))
    close_base1 <- close_base1 %>% mutate(ind_c = .[[1]], stk1_c = .[[2]]) %>% select(date, ind_c, stk1_c)
    close_base1 <- close_base1 %>% transform(ind_c=ind_c/ind_c[1],stk1_c=stk1_c/stk1_c[1])
    stk1_SMA = close_base1 %>% select(date, stk1_c)
    stk1_SMA$stk1_SMA50 = TTR::SMA(stk1_SMA$stk1_c, n=50)
    stk1_SMA$stk1_SMA100 = TTR::SMA(stk1_SMA$stk1_c, n=100)
    
    stk1_RSI = close_base1 %>% select(date, stk1_c)
    stk1_RSI$RSI14 = TTR::RSI(stk1_RSI$stk1_c, n=14)
    
    tmp_df = data.frame(stock1_all[,1:4], date=index(stock1_all))
    tmp_df = tmp_df %>% mutate(Opn = .[[1]], Hi = .[[2]], Low = .[[3]], Clo = .[[4]]) %>%
      select(date, Opn, Hi, Low, Clo)
    
    stk1_Stoch = tmp_df
    stk1_Stoch = cbind(stk1_Stoch, TTR::stoch(stk1_Stoch[,3:5], nFastK = 5, nFastD = 1, nSlowD = 5, smooth=5))
    stk1_Stoch = stk1_Stoch %>% select(date, fastK, slowD)
    stk1_Stoch = stk1_Stoch %>% gather(key="Legend", value="level", c(2,3))
    
    #for graphing
    colnames(stk1_SMA) = c("date", stock_1, "SMA 50", "SMA 100")
    stk1_SMA <- stk1_SMA %>% gather(key="Legend", value="level", 2:4)
    stk1_RSI <- stk1_RSI %>% gather(key="Legend", value="level", c(3))
    
    color1 = c("#27777d","#000000", "#00008B")
    color2 = c("#000000")
    color3 = c("#800000", "#008080")
    sXLabel = paste0(stock_1," with SMA")
    
    rm(plt_1)
    rm(plt_2)
    plt_1 <- figure(width=600, height=200) %>%
      ly_lines(data=stk1_SMA, x=date, y=level, color=Legend, legend=TRUE) %>%
      x_axis(label=NULL) %>% y_axis(label="Price (base 1.00)", number_formatter="numeral",format="0.00") %>%
      set_palette(discrete_color = pal_color((color1))) %>% 
      theme_legend(label_text_font_size = "6pt", glyph_height = 7, glyph_width = 7) %>%
      theme_axis(major_label_text_font_size = "7.5pt", axis_label_text_font = "1") %>%
      theme_plot(background_fill_color="white")
    
    plt_2 <- figure(width=600, height=100, ylim=c(0,100)) %>%
      ly_lines(data=stk1_RSI, x=date, y=level, color=Legend, legend=FALSE) %>%
      x_axis(label=NULL) %>% 
      y_axis(label="RSI", grid=FALSE, number_formatter="numeral",format="0",desired_num_ticks = 3) %>%
      set_palette(discrete_color = pal_color(color2)) %>%
      theme_axis(major_label_text_font_size = "7.5pt", axis_label_text_font = "1") %>%
      theme_plot(background_fill_color="white") %>%
      theme_legend(label_text_font_size = "6pt", glyph_height = 7, glyph_width = 7)
    
    plt_3 <- figure(width=600, height=100, ylim=c(0,1)) %>%
      ly_lines(data=stk1_Stoch, x=date, y=level, color=Legend, legend=FALSE) %>%
      x_axis(label=NULL) %>% 
      y_axis(label="Stoch (5K,5D)", grid=FALSE, number_formatter="numeral",format="0.00",desired_num_ticks = 3) %>%
      set_palette(discrete_color = pal_color(color3)) %>%
      theme_axis(major_label_text_font_size = "7.5pt", axis_label_text_font = "1") %>%
      theme_plot(background_fill_color="white") %>%
      theme_legend(label_text_font_size = "6pt", glyph_height = 7, glyph_width = 7)
    
    grid_plot(list(plt_1, plt_2, plt_3), nrow=3, ncol=1, same_axes=c(TRUE,FALSE, FALSE))
    })
  
  output$plot_1 <- renderRbokeh({
    p1()
    })
  
  #ANSEL ANSEL THIS IS OKAY ALREADY!!!!
  observeEvent(input$button1, {
    enable("button2")
    })
  #IMPORTANT. ANSEL THIS IS OKAY ALREADY!!!!
  
  p2 <- eventReactive(input$button2, {
    init_date <- "2016-12-29" #default
    start_date <- "2016-12-30" #default
    end_date <- "2018-01-02" #default
    indexETF <- 'SPY'
    stock_1 <- "AAPL" #default
    symbols <- c("SPY", "AAPL") #default
    adjustment <- FALSE #default
    
    init_date <- adjust.previous(adjust.previous(input$date1, dflt_cal)-1, dflt_cal)
    start_date <- adjust.previous(input$date1, dflt_cal)
    end_date <- adjust.next(input$date2, dflt_cal)
    stock_1 <- input$stockInput
    
    index_all = getSymbols(Symbols = c(indexETF), src = "yahoo", index.class = "POSIXct",
                           from = start_date, to = end_date, adjust = adjustment, auto.assign = FALSE)
    stock1_all = getSymbols(Symbols = c(stock_1), src = "yahoo", index.class = "POSIXct",
                            from = start_date, to = end_date, adjust = adjustment, auto.assign=FALSE)
    all_return = data.frame(cbind(index_all[,6],stock1_all[,6]), date=index(index_all))
    all_return <- all_return %>% mutate(ind_c = .[[1]], stk1_c = .[[2]]) %>% select(date, ind_c, stk1_c)
    all_return <- all_return %>% transform(ind_c=ind_c/ind_c[1],stk1_c=stk1_c/stk1_c[1])
    n <- nrow(all_return)
    print(n)
    all_return$ind_ret = c(NA, ((all_return[2:n, 2] - all_return[1:(n-1), 2])/all_return[1:(n-1), 2]))
    all_return$stk1_ret = c(NA, ((all_return[2:n, 3] - all_return[1:(n-1), 3])/all_return[1:(n-1), 3]))
    
    stk1_data = all_return %>% select(date, stk1_c, stk1_ret)
    
    tmp_df = data.frame(stock1_all[,1:4], date=index(stock1_all))
    tmp_df = tmp_df %>% mutate(Opn = .[[1]], Hi = .[[2]], Low = .[[3]], Clo = .[[4]]) %>%
      select(date, Opn, Hi, Low, Clo)
    
    stk1_Stoch = tmp_df
    stk1_Stoch = cbind(stk1_Stoch, TTR::stoch(stk1_Stoch[,3:5], nFastK = 5, nFastD = 1, nSlowD = 5, smooth=5))
    stk1_Stoch = stk1_Stoch %>% select(date, fastK, slowD)

    if (input$entryStrat=="SMA") {
      stk1_data$entryInd = TTR::SMA(stk1_data$stk1_c, n=input$enterSMAset)
    } else if (input$entryStrat=="RSI") {
      stk1_data$entryInd = TTR::RSI(stk1_data$stk1_c, n=14)
    } else if (input$entryStrat=="Stoch") {
      stk1_data$entryInd = stk1_Stoch$fastK
    }
    
    if (input$exitStrat=="SMA") {
      stk1_data$exitInd = TTR::SMA(stk1_data$stk1_c, n=input$exitSMAset)
    } else if (input$exitStrat=="RSI") {
      stk1_data$exitInd = TTR::RSI(stk1_data$stk1_c, n=14)
    } else if (input$exitStrat=="Stoch") {
      stk1_data$exitInd = stk1_Stoch$fastK
    }
    
    #entry strat check
    if (input$entryStrat=="SMA") {
      stk1_data$entryCheck = ifelse(stk1_data$stk1_c > stk1_data$entryInd, "B", "H")
    } else if (input$entryStrat=="RSI") {
      stk1_data$entryCheck = ifelse(stk1_data$entryInd < input$enterRSIset, "B", "H")
    } else if (input$entryStrat=="Stoch") {
      stk1_data$entryCheck = ifelse(stk1_data$entryInd < input$enterStochset, "B", "H")
    }
    
    # exit strat check
    if (input$exitStrat=="SMA") {
      stk1_data$exitCheck = ifelse(stk1_data$stk1_c < stk1_data$exitInd, "S", "H")
    } else if (input$exitStrat=="RSI") {
      stk1_data$exitCheck = ifelse(stk1_data$exitInd > input$exitRSIset, "S", "H")
    } else if (input$exitStrat=="Stoch") {
      stk1_data$exitCheck = ifelse(stk1_data$exitInd > input$exitStochset, "S", "H")
    }
    
    #decision column
    stk1_data$decision = paste0(ifelse(is.na(stk1_data$entryCheck),"",stk1_data$entryCheck), "",
                                ifelse(is.na(stk1_data$exitCheck),"",stk1_data$exitCheck))
    stk1_data$decision = ifelse( (stk1_data$decision=="") | (is.na(stk1_data$decision)) | (stk1_data$decision==paste0(NA,NA)),
                                 "H", stk1_data$decision)
    stk1_data$decision = ifelse(stk1_data$decision=="HH","H", stk1_data$decision)
    stk1_data$decision = ifelse(stk1_data$decision=="HS","S", stk1_data$decision)
    stk1_data$decision = ifelse(stk1_data$decision=="BH","B", stk1_data$decision)
    stk1_data$decision = ifelse(stk1_data$decision=="BS","S", stk1_data$decision)
    
    if (input$buyAtStart=="Yes") {
      stk1_data[1,8] = "B"
    } else {
      stk1_data[1,8] = "S"
    }
    
    #decision2 column
    stk1_data$decision2 = c(NA, stk1_data$decision[1:nrow(stk1_data)-1])
    
    #stratMult column
    tmp = c()
    for (i in 1:nrow(stk1_data)) {
      if (i==1) {
        tmp = c(tmp, 1.0)
      } else if (i>1) {
        r = ifelse( (trimws(stk1_data$decision2[i])=="H"), tmp[i-1], ifelse( trimws(stk1_data$decision2[i])=="B", 1, 0))
        tmp = c(tmp, r)
      }
    }
    stk1_data$stratMult = tmp
    
    #stratRet column
    stk1_data$strat_ret = stk1_data$stratMult*stk1_data$stk1_ret
    stk1_data$strat_ret[1] = NA
    #problem when S is the decision it does not output zero
    
    
    print (head(stk1_data,30))
    
    tmp = c()
    for (i in 1:nrow(stk1_data)) {
      if (i==1) {
        tmp = c(tmp, 1.0)
      } else if (i>1) {
        price = tmp[i-1]*(1+stk1_data$strat_ret[i])
        tmp = c(tmp, price)
      }
    }
    stk1_data$strat_c = tmp
    
    stk1_data = stk1_data %>% select(date, stk1_c, stk1_ret, entryInd, exitInd, strat_c, strat_ret)
    
    print (head(stk1_data,30))
    
    #dataframe which combines stk, strat and index return and price
    final_data = cbind(stk1_data, all_return %>% select(ind_c, ind_ret))
    
    print (head(final_data,30))
      
    #for graphing:
    #1)stock with indicators
    ##1.a)stock with upper indicators
    if (input$entryStrat=="SMA" & input$exitStrat=="SMA") {
      if (input$enterSMAset==input$exitSMAset){
        stk1_Uindic = final_data %>% select(date, stk1_c, entryInd)
        nameTmp = paste0("SMA ", input$entrySMAset)
        colnames(stk1_Uindic) = c("date", stock_1, nameTmp)
        stk1_Uindic <- stk1_Uindic %>% gather(key="Legend", value="level", c(2,3))
      } else if (input$enterSMAset!=input$exitSMAset) {
        stk1_Uindic = final_data %>% select(date, stk1_c, entryInd, exitInd)
        nameTmp1 = paste0("SMA ", input$entrySMAset)
        nameTmp2 = paste0("SMA ", input$exitSMAset)
        colnames(stk1_Uindic) = c("date", stock_1, nameTmp1, nameTmp2)
        stk1_Uindic <- stk1_Uindic %>% gather(key="Legend", value="level", c(2,3,4))
      }
    } else if (input$entryStrat=="SMA") {
      stk1_Uindic = final_data %>% select(date, stk1_c, entryInd)
      nameTmp = paste0("SMA ", input$enterSMAset)
      colnames(stk1_Uindic) = c("date", stock_1, nameTmp)
      stk1_Uindic <- stk1_Uindic %>% gather(key="Legend", value="level", c(2,3))
    } else if (input$exitStrat == "SMA") {
      stk1_Uindic = final_data %>% select(date, stk1_c, exitInd)
      nameTmp = paste0("SMA ", input$exitSMAset)
      colnames(stk1_Uindic) = c("date", stock_1, nameTmp)
      stk1_Uindic <- stk1_Uindic %>% gather(key="Legend", value="level", c(2,3))
    } else if (input$entryStrat!="SMA" & input$exitStrat!="SMA") {
      stk1_Uindic = final_data %>% select(date, stk1_c)
      stk1_Uindic$stk1_SMA50 = TTR::SMA(stk1_Uindic$stk1_c, n=50)
      stk1_Uindic$stk1_SMA100 = TTR::SMA(stk1_Uindic$stk1_c, n=100)
      colnames(stk1_Uindic) = c("date", stock_1, "SMA 50", "SMA 100")
      stk1_Uindic <- stk1_Uindic %>% gather(key="Legend", value="level", c(2,3,4))
    }

    color1 = c("#27777d","#000000", "#00008B")
    color2a = c("#000000")
    color2b = c("#800000", "#008080")
    pltResult_1 <- figure(width=600, height=200) %>%
      ly_lines(data=stk1_Uindic, x=date, y=level, color=Legend, legend=TRUE) %>%
      x_axis(label=NULL) %>% y_axis(label="Price (base 1.00)", number_formatter="numeral",format="0.00") %>%
      set_palette(discrete_color = pal_color((color1))) %>% 
      theme_legend(label_text_font_size = "6pt", glyph_height = 7, glyph_width = 7) %>%
      theme_axis(major_label_text_font_size = "7.5pt", axis_label_text_font = "1") %>%
      theme_plot(background_fill_color="white")
    
    ##1.b)lower indicator
    if (input$entryStrat=="RSI" | input$exitStrat=="RSI") {
      if (input$entryStrat=="RSI") {
        stk1_Lindic = final_data %>% select(date, entryInd)
      } else {
        stk1_Lindic = final_data %>% select(date, exitInd)
      }
      colnames(stk1_Lindic) = c("date", "RSI14")
      stk1_Lindic <- stk1_Lindic %>% gather(key="Legend", value="level", c(2))
      
      pltResult_2a <- figure(width=600, height=100, ylim=c(0,100)) %>%
        ly_lines(data=stk1_Lindic, x=date, y=level, color=Legend, legend=FALSE) %>%
        x_axis(label=NULL) %>% 
        y_axis(label="RSI", grid=FALSE, number_formatter="numeral",format="0",desired_num_ticks = 3) %>%
        set_palette(discrete_color = pal_color(color2a)) %>%
        theme_axis(major_label_text_font_size = "7.5pt", axis_label_text_font = "1") %>%
        theme_plot(background_fill_color="white") %>%
        theme_legend(label_text_font_size = "6pt", glyph_height = 7, glyph_width = 7)
      
    }
    
    if (input$entryStrat=="Stoch" | input$exitStrat=="Stoch") {
      stk1_Stoch = tmp_df
      stk1_Stoch = cbind(stk1_Stoch, TTR::stoch(stk1_Stoch[,3:5], nFastK = 5, nFastD = 1, nSlowD = 5, smooth=5))
      stk1_Stoch = stk1_Stoch %>% select(date, fastK, slowD)
      stk1_Stoch = stk1_Stoch %>% gather(key="Legend", value="level", c(2,3))
      
      pltResult_2b <- figure(width=600, height=100, ylim=c(0,1)) %>%
        ly_lines(data=stk1_Stoch, x=date, y=level, color=Legend, legend=FALSE) %>%
        x_axis(label=NULL) %>% 
        y_axis(label="Stoch (5K, 5D)", grid=FALSE, number_formatter="numeral",format="0",desired_num_ticks = 3) %>%
        set_palette(discrete_color = pal_color(color2b)) %>%
        theme_axis(major_label_text_font_size = "7.5pt", axis_label_text_font = "1") %>%
        theme_plot(background_fill_color="white") %>%
        theme_legend(label_text_font_size = "6pt", glyph_height = 7, glyph_width = 7)
    }
 
    #2)stock vs strat
    
    stk1_vs_strat = final_data %>% select(date, stk1_c, strat_c)
    colnames(stk1_vs_strat) = c("date", stock_1, "Strat")
    stk1_vs_strat <- stk1_vs_strat %>% gather(key="Legend", value="level", c(2,3))
    
    color3 = c("#737373","#0800e5")
    pltResult_3 <- figure(width=600, height=200) %>%
      ly_lines(data=stk1_vs_strat, x=date, y=level, color=Legend, legend=TRUE) %>%
      x_axis(label=NULL) %>% y_axis(label="Price (base 1.00)", number_formatter="numeral",format="0.00") %>%
      set_palette(discrete_color = pal_color((color3))) %>% 
      theme_legend(label_text_font_size = "6pt", glyph_height = 7, glyph_width = 7) %>%
      theme_axis(major_label_text_font_size = "7.5pt", axis_label_text_font = "1") %>%
      theme_plot(background_fill_color="white")
    
    #3)index vs strat
    
    index_vs_strat = final_data %>% select(date, ind_c, strat_c)
    colnames(index_vs_strat) = c("date", "Market", "Strat")
    index_vs_strat <- index_vs_strat %>% gather(key="Legend", value="level", c(2,3))
    
    color4 = c("#ffb400","#00a5e5")
    pltResult_4 <- figure(width=600, height=200) %>%
      ly_lines(data=index_vs_strat, x=date, y=level, color=Legend, legend=TRUE) %>%
      x_axis(label=NULL) %>% y_axis(label="Price (base 1.00)", number_formatter="numeral",format="0.00") %>%
      set_palette(discrete_color = pal_color((color4))) %>% 
      theme_legend(label_text_font_size = "6pt", glyph_height = 7, glyph_width = 7) %>%
      theme_axis(major_label_text_font_size = "7.5pt", axis_label_text_font = "1") %>%
      theme_plot(background_fill_color="white")
    
    if (input$entryStrat=="RSI" & input$exitStrat=="RSI") {
      grid_plot(list(pltResult_1, pltResult_2a, pltResult_3, pltResult_4), nrow=4, ncol=1, 
                same_axes=c(TRUE,FALSE, FALSE, FALSE))
    } else if (input$entryStrat=="SMA" & input$exitStrat=="SMA") {
      grid_plot(list(pltResult_1, pltResult_3, pltResult_4), nrow=3, ncol=1, 
                same_axes=c(TRUE,FALSE, FALSE))
    } else if (input$entryStrat=="Stoch" & input$exitStrat=="Stoch") {
      grid_plot(list(pltResult_1, pltResult_2b, pltResult_3, pltResult_4), nrow=4, ncol=1, 
                same_axes=c(TRUE,FALSE, FALSE, FALSE))
    } else if ((input$entryStrat=="Stoch" & input$exitStrat=="RSI") |
      (input$entryStrat=="RSI" & input$exitStrat=="Stoch")) {
        grid_plot(list(pltResult_1, pltResult_2a, pltResult_2b, pltResult_3, pltResult_4), nrow=5, ncol=1, 
                  same_axes=c(TRUE,FALSE, FALSE, FALSE, FALSE))
    } else if (input$entryStrat=="RSI" | input$exitStrat=="RSI") {
      grid_plot(list(pltResult_1, pltResult_2a, pltResult_3, pltResult_4), nrow=4, ncol=1, 
                same_axes=c(TRUE,FALSE, FALSE, FALSE))
    } else if (input$entryStrat=="Stoch" | input$exitStrat=="Stoch") {
      grid_plot(list(pltResult_1, pltResult_2b, pltResult_3, pltResult_4), nrow=4, ncol=1, 
                same_axes=c(TRUE,FALSE, FALSE, FALSE))
    }
    
    })
  
  output$plot_2 <- renderRbokeh({
    p2()
    })

  observeEvent(input$button2, {
    if (input$button2[1]>0) {
      shinyjs::hide("text_1", anim=TRUE, animType = "slide", time = 0.1)
      shinyjs::hide("plot_1", anim=TRUE, animType = "slide", time = 0.1)
      
      shinyjs::show("text_2", anim=TRUE, animType = "fade", time = 2)
      shinyjs::show("plot_2", anim=TRUE, animType = "fade", time = 2)
    }
  })
  
  observeEvent(input$button1, {
    if (input$button1[1]>1) {
      shinyjs::show("text_1", anim=TRUE, animType = "fade", time = 2)
      shinyjs::show("plot_1", anim=TRUE, animType = "fade", time = 2)
      
      shinyjs::hide("text_2", anim=TRUE, animType = "slide", time = 0.1)
      shinyjs::hide("plot_2", anim=TRUE, animType = "slide", time = 0.1)
    }
  })
  
} #this is the bracket of server function