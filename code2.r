library(httr)
library(jsonlite)
library(lubridate)
library(logging)
library(base64enc)
library(dplyr)

# 配置日志
basicConfig()
addHandler(writeToFile, file = "app.log")

# 设置代理
Sys.setenv(http_proxy = "http://127.0.0.1:7890")
Sys.setenv(https_proxy = "http://127.0.0.1:7890")

# 定义凭证
username <- "root"
password <- "123456"

# 函数：登录并获取Cookie
get_cookies <- function(url, username, password) {
  tryCatch({
    resp <- POST(
      url = modify_url(url, path = "/api/user/login"),
      body = list(username = username, password = password),
      encode = "json",
      config = config(ssl_verifypeer = FALSE)
    )
    
    if (status_code(resp) != 200) {
      logerror(paste("登录失败，URL:", url, "状态码：", status_code(resp)))
      return(NULL)
    }
    
    cookies <- cookies(resp)[["value"]]
    if (is.null(cookies)) {
      logerror(paste("未能获取到Cookie，URL:", url))
      return(NULL)
    }
    
    return(cookies)
  }, error = function(e) {
    logerror(paste("登录时发生错误，URL:", url, "错误信息：", e$message))
    return(NULL)
  })
}

# 函数：通过Cookie获取Access Token
get_access_token <- function(url, cookies) {
  tryCatch({
    resp <- GET(
      url = modify_url(url, path = "/api/user/self"),
      add_headers("Cookie" = cookies, "New-Api-User" = "1")
    )
    
    if (status_code(resp) != 200) {
      logerror(paste("获取Access Token失败，URL:", url, "状态码：", status_code(resp)))
      return(NULL)
    }
    
    at <- content(resp)$data$access_token
    if (is.null(at)) {
      logerror(paste("未能获取到Access Token，URL:", url))
      return(NULL)
    }
    
    return(at)
  }, error = function(e) {
    logerror(paste("获取Access Token时发生错误，URL:", url, "错误信息：", e$message))
    return(NULL)
  })
}

# 函数：获取所有渠道信息（支持分页）
get_all_channels <- function(url, at) {
  all_channels <- list()
  page <- 0
  page_size <- 20
  repeat {
    tryCatch({
      resp <- GET(
        url = modify_url(url, path = "/api/channel/"),
        query = list(p = page, page_size = page_size),
        add_headers(Authorization = paste("Bearer", at), "New-Api-User" = "1")
      )
      
      if (status_code(resp) != 200) {
        logerror(paste("获取渠道信息失败，URL:", url, "状态码：", status_code(resp)))
        break
      }
      
      data <- content(resp)$data
      if (length(data) == 0) {
        break
      }
      
      all_channels <- c(all_channels, data)
      page <- page + 1
    }, error = function(e) {
      logerror(paste("获取渠道信息时发生错误，URL:", url, "错误信息：", e$message))
      break
    })
  }
  
  if (length(all_channels) == 0) {
    logwarn(paste("未获取到任何渠道信息，URL:", url))
  }
  
  return(all_channels)
}

# 函数：获取每个渠道的Key
get_channel_keys <- function(url, at, cha_ids) {
  keys <- list()
  
  for (x in cha_ids) {
    tryCatch({
      resp_1 <- PUT(
        url = modify_url(url, path = "/api/channel"),
        add_headers(Authorization = paste("Bearer", at), "New-Api-User" = "1"),
        body = list(id = x),
        encode = "json"
      )
      
      if (status_code(resp_1) != 200) {
        logerror(paste("更新渠道", x, "失败，URL:", url, "状态码：", status_code(resp_1)))
        next
      }
      
      cha_df <- content(resp_1)$data
      keys[[as.character(x)]] <- cha_df
    }, error = function(e) {
      logerror(paste("获取渠道", x, "的Key时发生错误，URL:", url, "错误信息：", e$message))
    })
  }
  
  return(keys)
}

# 主处理函数
process_url <- function(url, username, password) {
  loginfo(paste("开始处理URL:", url))
  
  cookies <- get_cookies(url, username, password)
  if (is.null(cookies)) {
    logerror(paste("跳过URL，因登录失败:", url))
    return(NULL)
  }
  
  access_token <- get_access_token(url, cookies)
  if (is.null(access_token)) {
    logerror(paste("跳过URL，因获取Access Token失败:", url))
    return(NULL)
  }
  
  channels <- get_all_channels(url, access_token)
  if (length(channels) == 0) {
    logerror(paste("跳过URL，因没有渠道信息可处理:", url))
    return(NULL)
  }
  
  cha_ids <- sapply(channels, function(x) { x$id })
  channel_keys <- get_channel_keys(url, access_token, cha_ids)
  
  loginfo(paste("完成处理URL:", url))
  return(channel_keys)
}

# 多个URL列表
url_list <- read.table("clipboard",header = T,sep = "\t")$V1

# 使用 lapply 遍历URL列表
results <- lapply(url_list, function(u) {
  process_url(u, username, password)
})

selected_url<-url_list[sapply(results,function(x)(!is.null(x)))]
results<-results[sapply(results,function(x)(!is.null(x)))]

a=lapply(1:length(selected_url),function(x){
    my_url=selected_url[x]
    print(my_url)
    x=results[[x]]
    lapply(x,function(y){
      data.frame(id=y$id,
                 type=y$type,
                 key=y$key,
                 models=y$models,
                 #model_mapping=y$model_mapping,
                 status=y$status,
                 base_url=ifelse(is.null(y$base_url),"",y$base_url))
    })%>%Reduce(rbind,.)->df
    df$url<-my_url
    df
  
})%>%Reduce(rbind,.)%>%filter(key!="sk-fastgpt")%>%filter(key!="sk-freeapi")

