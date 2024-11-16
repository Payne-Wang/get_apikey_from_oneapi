library(httr)
library(jsonlite)
library(lubridate)
library(logging)
library(base64enc)
library(dplyr)
library(stringr)
setwd("")
# 设置代理
Sys.setenv(http_proxy = "http://127.0.0.1:7890")
Sys.setenv(https_proxy = "http://127.0.0.1:7890")

# 登录函数获取cookie
login <- function(url, username, password) {
  tryCatch({
    resp <- POST(
      url = modify_url(url, path = "/api/user/login"),
      body = list(username = username, password = password),
      encode = "json",
      config = config(ssl_verifypeer = FALSE)
    )
    if (status_code(resp) == 200) {
      return(cookies(resp)[["value"]])
    } else {
      warning("登录失败，URL: ", url, "，状态码：", status_code(resp))
      return(NULL)
    }
  }, error = function(e) {
    warning("登录请求出错，URL: ", url, "，错误：", e$message)
    return(NULL)
  })
}

# 获取access token
get_access_token <- function(url, cookies) {
  tryCatch({
    resp <- GET(
      url = modify_url(url, path = "/api/user/self"),
      add_headers(`Cookie` = cookies, "New-Api-User" = "1")
    )
    if (status_code(resp) == 200) {
      return(content(resp)$data$access_token)
    } else {
      warning("获取access token失败，URL: ", url, "，状态码：", status_code(resp))
      return(NULL)
    }
  }, error = function(e) {
    warning("获取access token出错，URL: ", url, "，错误：", e$message)
    return(NULL)
  })
}

# 获取渠道信息
get_channels <- function(url, at) {
  tryCatch({
    resp <- GET(
      url = modify_url(url, path = "/api/channel/?p=0&page_size=1000&id_sort=true"),
      add_headers(Authorization = paste("Bearer", at), "New-Api-User" = "1")
    )
    if (status_code(resp) == 200) {
      return(sapply(content(resp)$data, function(x) x$id))
    } else {
      warning("获取渠道信息失败，URL: ", url, "，状态码：", status_code(resp))
      return(NULL)
    }
  }, error = function(e) {
    warning("获取渠道信息出错，URL: ", url, "，错误：", e$message)
    return(NULL)
  })
}

# 测试单个渠道的模型
test_channel_model <- function(url, at, channel_id, model) {
  tryCatch({
    resp <- GET(
      url = modify_url(url, path = paste0("/api/channel/test/", channel_id, "?model=", model)),
      add_headers(Authorization = paste("Bearer", at), "New-Api-User" = "1"),
      timeout(20)
    )
    if (status_code(resp) == 200) {
      return(content(resp)$success)
    } else {
      warning("测试渠道模型失败，URL: ", url, "，渠道ID：", channel_id, 
              "，模型：", model, "，状态码：", status_code(resp))
      return(FALSE)
    }
  }, error = function(e) {
    warning("测试渠道模型出错，URL: ", url, "，渠道ID：", channel_id, 
            "，模型：", model, "，错误：", e$message)
    return(FALSE)
  })
}

# 主函数处理单个URL
main <- function(url, username, password) {
  # 登录获取cookie
  cookies <- login(url, username, password)
  if (is.null(cookies)) return(NULL)
  
  # 获取access token
  at <- get_access_token(url, cookies)
  if (is.null(at)) return(NULL)
  
  # 获取渠道ID列表
  cha_ids <- get_channels(url, at)
  if (is.null(cha_ids)) return(NULL)
  
  # 获取渠道的key等信息
  apikey_channelid <- lapply(cha_ids, function(x) {
    tryCatch({
      resp_1 <- PUT(
        url = modify_url(url, path = "/api/channel"),
        add_headers(Authorization = paste("Bearer", at), "New-Api-User" = "1"),
        body = list(id = x),
        encode = "json"
      )
      content(resp_1)$data
    }, error = function(e) {
      warning("获取渠道key出错，URL: ", url, "，渠道ID：", x, "，错误：", e$message)
      return(NULL)
    })
  }) %>%
    Filter(Negate(is.null), .) %>%
    sapply(function(x) c(x$id, x$type, x$key, x$base_url, x$models), simplify = TRUE) %>%
    t() %>%
    as.data.frame(stringsAsFactors = FALSE) %>%
    setNames(c("id", "type", "key", "base_url", "models"))
  
  # 定义感兴趣的模型
  interested_models <- c("gpt-4o,", "chatgpt-4o-latest", "gpt-4o-2024-08-06", "gpt-4o-2024-05-13",
                         "o1-mini,", "o1-preview,",
                         "claude-3-5-sonnet-20240620", "claude-3-5-sonnet-20241022") %>%
    paste(collapse = "|")
  
  # 过滤并测试模型，并添加成功的模型列
  results <- apikey_channelid %>%
    filter(str_detect(models, interested_models)) %>%
    mutate(models = str_extract_all(models, interested_models) %>%
             sapply(function(x) paste0(x, collapse = ",")%>%str_replace_all(.,",,",",")%>%str_remove(",$"))) %>%
    rowwise() %>%
    mutate(
      # 测试每个模型的可用性
      response = list(sapply(str_split(models, ",")[[1]], function(y) {
        test_channel_model(url, at, id, y)
      })),
      # 将测试结果转为字符串
      response_str = paste(unlist(response), collapse = ","),
      # 提取测试结果为TRUE的模型
      successful_models = paste(str_split(models, ",")[[1]][which(unlist(response))], collapse = ",")
    ) %>%
    ungroup() %>%
    select(-response)  # 如果不需要保留列表形式的response，可移除
  
  return(results)
}

# 新增函数：处理多个URL
process_urls <- function(my_url_list, username, password) {
  results_list <- lapply(my_url_list, function(current_url) {
    cat("正在处理 URL:", current_url, "\n")
    tryCatch({
      df <- main(current_url, username, password)
      if (!is.null(df)) {
        df$source_url <- current_url  # 添加来源URL列
        return(df)
      } else {
        warning("未返回数据，URL: ", current_url)
        return(NULL)
      }
    }, error = function(e) {
      warning("处理 URL 失败，URL: ", current_url, "，错误：", e$message)
      return(NULL)
    })
  })
  
  # 过滤掉NULL值，并合并所有数据框
  combined_results <- do.call(rbind, Filter(Negate(is.null), results_list))%>%filter(successful_models!="")%>%filter(type==1)
  
  return(combined_results)
}

# 定义URL列表
my_url_list<-lapply(list.files(pattern = paste0("api",Sys.Date())),function(x){read.table(x,sep = "\t")})%>%Reduce(rbind,.)%>%distinct()%>%.[,1]
# 执行多URL处理
username <- "root"
password <- "123456"

apikey_channelid_response <- process_urls(my_url_list, username, password)

# 查看合并后的结果
print(apikey_channelid_response)

write.table(apikey_channelid_response,paste0("apikey_channelid_response_",Sys.Date(),".txt"),quote = F,sep = "\t")
