# brinson模型备注：
## 传统模型&&新的多期模型
### 1. 每日的资产选择贡献考虑期初资产
### 2. 每日的资产配置贡献考虑期初资产
### 3. 每日的外部资金贡献考虑期末资产
### 4. 每日汇总表中的第一个交易日只考虑期末资产作为下一个交易日的期初资产，不考虑交易行为
### 5. 新模型的最终收益率考虑最后时期的外部资金流入流出


# csv格式备注
# 每日汇总表
## 字段： 
### 公司名称: name
### 公司代码: code
### 行业分类: sector
### 交易日期: date
### 开盘价: open(暂无用)
### 收盘价: close  
### 资产权重: port_shares
### 总股数: total_shares
### 基准权重: benchmark(暂无用)

# 交易表
# 公司名称:name
# 公司代码: code
# 交易价格: trade
# 交易股数: share
# 交易时间: date

# 注意
###  时间格式必须是: yyyy-mm-dd

# 自定义函数
CategoryMapper <- function(map, unique_categories, code)
{
  index <- 0
  for (i in c(1:length(code_category_map[,1])))
  {
    if (code_category_map[i,1] == code)
    {
      index <- i
      break
    }
  }
  return(which(unique_categories == code_category_map[index,2]))
}
IndexCodeMapper <- function(unique_codes, code)
{
  index <- 0
  for (i in c(1:length(unique_codes)))
  {
    if (code == unique_codes[i])
    {
      index <- i
      break
    }
  }
  return(index)
}
IndexDateMapper <- function(unique_dates, date)
{
  index <- 0
  for (i in c(1:length(unique_dates)))
  {
    if (date == unique_dates[i])
    {
      index <- i
      break
    }
  }
  return(index)
}
NameCodeMapper <- function(name_code_map, unique_names, code)
{
  index <- 0
  for (i in c(1:length(name_code_map[,2])))
  {
    if (code == name_code_map[i,2])
    {
      index <- i
      break
    }
  }
  return(which(unique_names == name_code_map[i,1]))
}

# 基本参数设置
setwd("~/accomplishment/txt_notebook/广发基金")
## 类型选择
## 不同类型对应不同的csv格式
### 1 - 多阶段,默认几何法
### 2 - 新的多期模型，默认几何法
type <- 1 # 目前只能也只需要选2
file_name <- "sample_data.csv"
trading_file_name <- "sample_trade_data2.csv"
# outside_cash_flow_file_name <- "outside_cash_flow.csv" # 通过前两张表可以计算外部资金的流入流出
company_name <- "name"
code_name <- "code"
category_name <- "sector"
date_name <- "date"
close_name <- "close"
port_share_name <- "port_shares"
total_share_name <- "total_shares"
data <- read.csv(file=file_name, header=T,sep=";") #选择数据
trading_data <- read.csv(file=trading_file_name, header=T,sep=";") #选择交易数据

# 初始化处理
row_num <- nrow(data)
counts <- c(1:row_num)

# 模型计算
## 多期
## 找出所有的种类并赋给向量
categories<-character(0) #用于存储各个分类标签
categories <- data[1:row_num,c(category_name)]
unique_categories <- levels(factor(categories))
category_count <- length(unique_categories)
category_counts <- c(1:length(unique_categories))
## 找出所有公司代码并付给向量
codes <- data[,c(code_name)]
unique_codes <- levels(factor(codes))
code_count <- length(unique_codes)
code_counts <- c(1:length(unique_codes))
## 找出公司名称
names <- data[,c(company_name)]
unique_names <- levels(factor(names))
name_count <- length(unique_names)
name_counts <- c(1:length(unique_names))
# 找出所有时间并赋给向量
dates <- as.Date(data[,c(date_name)])
unique_dates <- levels(factor(dates)) # 注意首日和末日的特殊处理
date_count <- length(unique_dates)
date_counts <- c(1:length(unique_dates))
# 公司代码和种类对应关系表
temp_code_category_map <- data[,c(code_name,category_name)]
code_category_map <- unique(temp_code_category_map)
# 公司代码和公司名称对应关系表
temp_name_code_map <- data[,c(company_name,code_name)]
name_code_map <- unique(temp_name_code_map)
# 资产收盘价格矩阵(1:T)
close_price_mat <- matrix(c(1:(date_count*code_count)),byrow = T, nrow = code_count)
# 资产股数矩阵(1:T)
port_share_mat <- matrix(c(1:(date_count*code_count)),byrow = T, nrow = code_count)
for (k in counts)
{
  for (j in date_counts)
  {
    for (n in code_counts)
    {
      if ((dates[k] == unique_dates[j]) && (codes[k] == unique_codes[n]))
      {
        close_price_mat[n,j] <- data[k,c(close_name)]
        port_share_mat[n,j] <- data[k,c(port_share_name)]
      }
    }    
  }
}
# 实际期初权重矩阵(1:T-1)
port_weight_mat <- matrix(c(1:((date_count-1)*code_count)),byrow = T, nrow = code_count)
# 实际资产规模矩阵(1:T)
port_amount_mat <- port_share_mat*close_price_mat
for (k in counts)
{
  for (j in c(1:(date_count-1)))
  {
    for (n in code_counts)
    {
      if ((dates[k] == unique_dates[j]) && (codes[k] == unique_codes[n]))
      {
        port_weight_mat[n,j] <- port_amount_mat[n,j]/colSums(port_amount_mat)[j]
      }
    }    
  }
}
# 基准总股数矩阵(1:T-1)
total_share_mat <- matrix(c(1:((date_count-1)*code_count)),byrow = T, nrow = code_count)
for (k in counts)
{
  for (j in c(1:(date_count-1)))
  {
    for (n in code_counts)
    {
      if ((dates[k] == unique_dates[j]) && (codes[k] == unique_codes[n]))
      {
        total_share_mat[n,j] <- data[k,c(total_share_name)]
      }
    }    
  }
}
# 基准期初权重矩阵(1:T-1)
bench_weight_mat <- matrix(c(1:((date_count-1)*code_count)),byrow = T, nrow = code_count)
# 基准期初规模矩阵(1:T-1)
bench_amount_mat <- total_share_mat*close_price_mat[,c(1:(date_count-1))]

for (k in counts)
{
  for (j in c(1:(date_count-1)))
  {
    for (n in code_counts)
    {
      if ((dates[k] == unique_dates[j]) && (codes[k] == unique_codes[n]))
      {
        bench_weight_mat[n,j] <- bench_amount_mat[n,j]/(colSums(bench_amount_mat))[j]
      }
    }    
  }
}

# 各资产的实际收益率矩阵(1:T-1)
port_ret_mat <- (close_price_mat[,c(2:date_count)]-close_price_mat[,c(1:(date_count-1))])/close_price_mat[,c(1:(date_count-1))]
# 实际行业资产规模矩阵(1:T-1)
port_category_amount_mat <- matrix(c(1:((date_count-1)*category_count)),byrow = T, nrow = category_count)
# 实际行业资产权重矩阵(1:T-1)
port_category_weight_mat <- matrix(c(1:((date_count-1)*category_count)),byrow = T, nrow = category_count)
# 基准行业资产规模矩阵(1:T-1)
bench_category_amount_mat <- matrix(c(1:((date_count-1)*category_count)),byrow = T, nrow = category_count)
# 基准行业资产权重矩阵(1:T-1)
bench_category_weight_mat <- matrix(c(1:((date_count-1)*category_count)),byrow = T, nrow = category_count)
for (j in c(1:(date_count-1)))
{
  for (i in category_counts)
  {
    port_category_amount_mat[i,j] <- 0
    bench_category_amount_mat[i,j] <- 0
    for (k in counts)
    {
      if (data[k,c(category_name)] == unique_categories[i]&&data[k,c(date_name)]==unique_dates[j])
      {
        port_category_amount_mat[i,j] <- port_category_amount_mat[i,j] + data[k,c(close_name)]*data[k,c(port_share_name)]
        bench_category_amount_mat[i,j] <- bench_category_amount_mat[i,j] + data[k,c(close_name)]*data[k,c(total_share_name)]
      }
    }
  }
  port_category_weight_mat[,j] <- port_category_amount_mat[,j]/sum(port_category_amount_mat[,j])
  bench_category_weight_mat[,j] <- bench_category_amount_mat[,j]/sum(bench_category_amount_mat[,j])  
}

# 收盘价交易情况下行业的收益率矩阵
port_category_close_ret_mat <- matrix(c(1:((date_count-1)*category_count)),byrow = T, nrow = category_count) 
# 基准行业收益率矩阵
bench_category_ret_mat <- matrix(c(1:((date_count-1)*category_count)),byrow = T, nrow = category_count)
for(j in c(1:(date_count-1)))
{
  for (i in category_counts)
  {
    port_category_close_ret_mat[i,j] <- 0
    bench_category_ret_mat[i,j] <- 0
    for (k in code_counts)
    {
      index <- CategoryMapper(code_category_map, unique_categories, unique_codes[k])
      if (index == i)
      {
        port_category_close_ret_mat[i,j] <- port_category_close_ret_mat[i,j] + port_ret_mat[k,j]*port_weight_mat[k,j]/port_category_weight_mat[i,j]
        bench_category_ret_mat[i,j] <- ifelse(bench_category_weight_mat[i,j] == 0, 0,bench_category_ret_mat[i,j] + port_ret_mat[k,j]*bench_weight_mat[k,j]/bench_category_weight_mat[i,j])
      }
    }
  }
}
# 收盘价交易收益率
port_close_rets <- colSums(port_category_weight_mat*port_category_close_ret_mat)
# 半名义基金收益率
half_nominal_rets <- colSums(port_category_weight_mat*bench_category_ret_mat)
# 基准收益率向量(1:T-1)
bench_rets <- colSums(bench_weight_mat*port_ret_mat)
# 资产配置的贡献
## 各个分类的资产配置贡献
category_allocation_mat <- (t(t(1+bench_category_ret_mat)/(1+bench_rets))-1)*(port_category_weight_mat - bench_category_weight_mat)
## 总的资产配置贡献
total_allocations <- (1 + half_nominal_rets)/(1 + bench_rets) - 1
# 资产选择贡献
## 总的资产选择贡献
total_selections <- (1 + port_close_rets)/(1 + half_nominal_rets) - 1
## 各个行业的选股贡献
category_selection_mat <- t(t(port_category_close_ret_mat - bench_category_ret_mat)/(1 + half_nominal_rets))*port_category_weight_mat
## 各个股票的选择贡献
stock_selection_mat <- matrix(c(1:(code_count*(date_count-1))), byrow = T, nrow = code_count)
for (i in c(1:date_count-1))
{
  for (j in code_counts)
  {
    tmp_index <- CategoryMapper(code_category_map,unique_categories,unique_codes[j])
    tmp_bi <- bench_category_ret_mat[tmp_index,i]
    stock_selection_mat[j,i] <- (port_ret_mat[j,i] - tmp_bi)*port_weight_mat[j,i]/(1+half_nominal_rets[i])
  }
}
# 当type = 2的时候，假设只能收盘价成交，真实收益率就是收盘价成交的收益率
# 当type = 3的时候，真实收益率不等于收盘价成交的收益率
#以下为type = 3的时候才需要计算
# 实际收益率向量(1:T-1)
# 交易时机的贡献
total_tradings <- c(1:(code_count-1)) 
# 每笔交易的贡献
trade_count <- 1
if ( type == 2)
{
  trade_count <- nrow(trading_data)
}
each_tradings <- c(1:trade_count) # 将每笔交易的贡献存在向量里
# 外部现金流的贡献
outer_cash_flow_amounts <- c(1:(date_count-1))
## 初始化为0
for (i in c(1:length(outer_cash_flow_amounts)))
{
  outer_cash_flow_amounts[i] <- 0
}
outer_cash_flows <- c(1:(date_count-1))
port_rets <- c(1:(date_count-1))
port_total_amounts <- colSums(port_amount_mat) # 每个时期总的实际资产规模
if (type == 1)
{
  tmp_mat <- port_share_mat*close_price_mat
  port_rets <- (colSums(tmp_mat[,c(2:date_count)])-colSums(tmp_mat[,c(1:(date_count-1))]))/colSums(tmp_mat[,c(1:(date_count-1))])
  port_relative_rets <- (1+port_rets)/(1+bench_rets) - 1 
}
if (type == 2)
{
  tmp_mat <- port_share_mat*close_price_mat
  port_rets <- (colSums(tmp_mat[,c(2:date_count)])-colSums(tmp_mat[,c(1:(date_count-1))]))/colSums(tmp_mat[,c(1:(date_count-1))])
  port_relative_rets <- (1+port_rets)/(1+bench_rets) - 1  
  trading_dates <- trading_data[,c("date")]
  trading_names <- trading_data[,c("name")]
  trading_shares <- trading_data[,c("share")]
  trading_prices <- trading_data[,c("price")]
  trading_codes <- trading_data[,c("code")]
  trading_counts <- c(1:length(trading_dates))
  trading_count <- length(trading_counts)
  amount_changes <- c(1:length(trading_dates)) 
  ## 无交易收益和交易收益的和
  tmp_items <- c(1:(length(unique_dates)-1))
  ## tmp_items初始化
  tmp_items <- port_total_amounts[1:date_count-1]*(1+port_close_rets[1:date_count-1])
  # 无外部资金的收益率
  port_inner_rets <- c(1:(date_count-1))
  for (i in trading_counts)
  {
    tmp_end_date_index <- IndexDateMapper(unique_dates = unique_dates, date = trading_dates[i])
    tmp_code_index <-IndexCodeMapper(unique_codes = unique_codes, code =  trading_codes[i])
    amount_changes[i] <- trading_shares[i]*(close_price_mat[tmp_code_index,tmp_end_date_index] - trading_prices[i])
    tmp_begin_total_wealth <- (colSums(port_amount_mat))[tmp_end_date_index-1]
    each_tradings[i] <- (amount_changes[i]/tmp_begin_total_wealth)/(1 + port_close_rets[tmp_end_date_index-1]) 
    
    tmp_items[tmp_end_date_index-1] <- tmp_items[tmp_end_date_index-1] + amount_changes[i]
  }
  outer_cash_flow_amounts <- port_total_amounts[2:date_count] - tmp_items
  outer_cash_flows <- port_total_amounts[2:date_count]/(port_total_amounts[2:date_count] - outer_cash_flow_amounts) - 1
  
  port_inner_rets <- (port_total_amounts[c(2:date_count)] - port_total_amounts[c(1:(date_count-1))] - outer_cash_flow_amounts)/port_total_amounts[c(1:(date_count-1))]
  total_tradings <- (1 + port_inner_rets)/(1 + port_close_rets) - 1
  #total_tradings <- (1 + port_rets)/( (1 + port_close_rets)*(1 + outer_cash_flows) ) - 1#存在一点误差
  
  
  # 其他用于基金经理分析的变量
  ## 基金经理行业偏好
  category_excess_weight <- port_category_weight_mat - bench_category_weight_mat
  ## 基金经理擅长
  ### 交易时机的总的收益率
  total_trading <- 1
  ### 行业配置的总的收益率
  total_allocation <- 1
  ### 股票选择的总的收益率
  total_selection <- 1
  ### 总的基准收益率
  total_bench_ret <- 1
  ###总的绝对收益率
  total_ab_ret <- 1
  ###外部资金贡献率
  total_outer_cash_flow <- 1
  ###总的超额收益
  total_excess_ret <- 1
  for (i in c(1:(date_count-1)))
  {
    total_trading <- total_trading*(1+total_tradings[i])
    total_allocation <- total_allocation*(1+total_allocations[i])
    total_selection <- total_selection*(1+total_selections[i])
    total_bench_ret <- total_bench_ret*(1+bench_rets[i])
    total_ab_ret <- total_ab_ret*(port_rets[i]+1)
    total_outer_cash_flow <- total_outer_cash_flow*(1+outer_cash_flows[i])
  }
  total_trading <- total_trading - 1
  total_allocation <- total_allocation - 1
  total_selection <- total_selection - 1
  total_ab_ret <- total_ab_ret - 1
  total_outer_cash_flow <- total_outer_cash_flow - 1
  total_bench_ret <- total_bench_ret - 1
  total_excess_ret <- (1+total_ab_ret)/(1+total_bench_ret)-1
  total_adjsut_excess_ret <- (1+total_excess_ret)/(1+total_outer_cash_flow)-1
  ### 行业交易时机贡献，跨越整个交易周期的汇总
  
  
  
  
}
# 结果输出
# 标题列
# 日期列（多列）
first_line <- character(date_count)
first_line[1] <- "Date"
for (i in c(2:date_count))
{
  first_line[i] <- as.character(unique_dates[i])
}
write.table(t(first_line), file = "mydata.csv", append = FALSE, sep = ";", row.names = FALSE, col.names = FALSE)
if (type == 1)
{
  line_count <- 1 +  1 + category_count + 1 + category_count + code_count
  # 第一列:字符说明列
  introduction_col <- character(line_count)
  # 数据展示矩阵
  numeric_mat <- matrix(c(1:((date_count-1)*line_count)),byrow = T, nrow = line_count)
  location <- 1
  introduction_col[1] <- "Total Excess Return"
  numeric_mat[location,] <- port_relative_rets
  location <- location + 1
  introduction_col[location] <- "Allocation Effect"
  numeric_mat[location,] <- total_allocations
  for ( i in category_counts)
  {
    location <- location + 1
    introduction_col[location] <- paste("   ", unique_categories[i], " Effect", sep="")
    numeric_mat[location,] <- category_allocation_mat[i,]
  }
  location <- location + 1
  introduction_col[location] <- "Selection Effect"
  numeric_mat[location,] <- total_selections
  for (i in category_counts)
  {
    location <- location + 1
    introduction_col[location] <- paste("   ", unique_categories[i], " Selection Effect", sep="")
    numeric_mat[location,] <- category_selection_mat[i,]
    for (j in code_counts)
    {
      # 判断是否属于当前这个category
      category_index <- CategoryMapper(code_category_map,unique_categories,unique_codes[j])
      if (unique_categories[category_index] == unique_categories[i])
      {
        location <- location + 1
        name_index <- NameCodeMapper(name_code_map,unique_names,unique_codes[j])
        introduction_col[location] <- paste("      ",unique_names[name_index]," Selction Effect",sep="")
        numeric_mat[location,] <- stock_selection_mat[j,]
      }
    }
  }
  data_portion <- data.frame(introduction_col,numeric_mat)
  write.table(data_portion, file = "mydata.csv", append = TRUE, sep = ";", row.names = FALSE, col.names = FALSE)  
}
if (type == 2)
{
  line_count <- 1 + 1 + 1 + trade_count + 1 + category_count + 1 + category_count + code_count
  # 第一列:字符说明列
  introduction_col <- character(line_count)
  # 数据展示矩阵
  numeric_mat <- matrix(c(1:((date_count-1)*line_count)),byrow = T, nrow = line_count)
  location <- 1
  introduction_col[1] <- "Total Return"
  numeric_mat[location,] <- port_relative_rets
  location <- location + 1
  introduction_col[2] <- "Outer Cash Flow Effect"
  numeric_mat[location,] <- outer_cash_flows
  location <- location + 1
  introduction_col[3] <- "Trading Effect"
  numeric_mat[location,] <- total_tradings
  for (i in trading_counts)
  {
    location <- location + 1
    introduction_col[i+3] <- paste("   (",trading_names[i],",", trading_shares[i],",", trading_prices[i],",",trading_dates[i],")",sep="")
    for (j in c(1:length(numeric_mat[1,])))
    {
      numeric_mat[location,j] <- 0
    }
    numeric_mat[location,(IndexDateMapper(unique_dates,trading_dates[i])-1)] <- each_tradings[i]
  }
  location <- location + 1
  introduction_col[location] <- "Allocation Effect"
  numeric_mat[location,] <- total_allocations
  for ( i in category_counts)
  {
    location <- location + 1
    introduction_col[location] <- paste("   ", unique_categories[i], " Effect", sep="")
    numeric_mat[location,] <- category_allocation_mat[i,]
  }
  location <- location + 1
  introduction_col[location] <- "Selection Effect"
  numeric_mat[location,] <- total_selections
  for (i in category_counts)
  {
    location <- location + 1
    introduction_col[location] <- paste("   ", unique_categories[i], " Selection Effect", sep="")
    numeric_mat[location,] <- category_selection_mat[i,]
    for (j in code_counts)
    {
      # 判断是否属于当前这个category
      category_index <- CategoryMapper(code_category_map,unique_categories,unique_codes[j])
      if (unique_categories[category_index] == unique_categories[i])
      {
        location <- location + 1
        name_index <- NameCodeMapper(name_code_map,unique_names,unique_codes[j])
        introduction_col[location] <- paste("      ",unique_names[name_index]," Selction Effect",sep="")
        numeric_mat[location,] <- stock_selection_mat[j,]
      }
    }
  }
  data_portion <- data.frame(introduction_col,numeric_mat)
  write.table(data_portion, file = "mydata.csv", append = TRUE, sep = ";", row.names = FALSE, col.names = FALSE)  
}
