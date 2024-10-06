# 安装并加载必要的包
# 如果尚未安装，请取消下面两行的注释并运行
# install.packages("httr")
# install.packages("jsonlite")
library(httr)
library(jsonlite)
my_url<-"https://XXXX"
myroot<-"root"
mypwd<-"123456"

# 定义 Cookie 存储文件
COOKIE_FILE <- "cookies.txt"

# 步骤 1：登录并保存 Cookie
login_url <- paste0(my_url,'/api/user/login')

login_response <- POST(
  url = login_url,
  add_headers(
    `sec-ch-ua-platform` = '"Windows"',
    Referer = paste0(my_url,'/login'),
    `User-Agent` = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/129.0.0.0 Safari/537.36',
    Accept = 'application/json, text/plain, */*',
    `sec-ch-ua` = '"Google Chrome";v="129", "Not=A?Brand";v="8", "Chromium";v="129"',
    `Content-Type` = 'application/json',
    `sec-ch-ua-mobile` = '?0'
  ),
  body = list(
    username = myroot,
    password = mypwd
  ),
  encode = "json",
  config = config(
    cookiejar = COOKIE_FILE,  # 保存 Cookie 到文件
    accept_encoding = "gzip"  # 支持压缩
  )
)

# 检查登录是否成功
if (http_error(login_response)) {
  stop("登录请求失败: ", status_code(login_response), 
       "\n响应内容: ", content(login_response, as = "text", encoding = "UTF-8"))
} else {
  message("登录成功，Cookie 已保存到 ", COOKIE_FILE)
}

# 步骤 2：使用保存的 Cookie 获取用户信息
user_info_url <- paste0(my_url,'/api/user/self')

user_info_response <- GET(
  url = user_info_url,
  add_headers(
    `sec-ch-ua-platform` = '"Windows"',
    Referer = paste0(my_url,'/panel'),
    `User-Agent` = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/129.0.0.0 Safari/537.36',
    Accept = 'application/json, text/plain, */*',
    `sec-ch-ua` = '"Google Chrome";v="129", "Not=A?Brand";v="8", "Chromium";v="129"',
    `sec-ch-ua-mobile` = '?0'
  ),
  config = config(
    cookiefile = COOKIE_FILE,  # 使用之前保存的 Cookie
    accept_encoding = "gzip"    # 支持压缩
  )
)

# 检查获取用户信息是否成功
if (http_error(user_info_response)) {
  stop("获取用户信息请求失败: ", status_code(user_info_response), 
       "\n响应内容: ", content(user_info_response, as = "text", encoding = "UTF-8"))
} else {
  message("成功获取用户信息")
}

# 将响应内容保存到 JSON 文件
response_content <- content(user_info_response, as = "text", encoding = "UTF-8")
writeLines(response_content, "response_self.json")

# 可选：显示获取到的用户信息
cat("用户信息:\n")
cat(response_content, "\n\n")

# 步骤 3：获取所有频道的列表并提取频道 ID
# 由于频道可能跨多页，需遍历所有页码
get_all_channels <- function(base_url, cookie_file) {
  all_channels <- list()
  page <- 0
  repeat {
    message("获取频道列表，第 ", page + 1, " 页")
    channels_url <- paste0(base_url, "?p=", page)
    
    channels_response <- GET(
      url = channels_url,
      add_headers(
        `Accept` = 'application/json, text/plain, */*',
        `Accept-Language` = 'zh-CN,zh;q=0.9,ja;q=0.8',
        `Priority` = 'u=1, i',
        Referer = paste0(my_url,'/panel/channel'),
        `sec-ch-ua` = '"Google Chrome";v="129", "Not=A?Brand";v="8", "Chromium";v="129"',
        `sec-ch-ua-mobile` = '?0',
        `sec-ch-ua-platform` = '"Windows"',
        `sec-fetch-dest` = 'empty',
        `sec-fetch-mode` = 'cors',
        `sec-fetch-site` = 'same-origin',
        `User-Agent` = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/129.0.0.0 Safari/537.36'
      ),
      config = config(
        cookiefile = cookie_file,  # 使用之前保存的 Cookie
        accept_encoding = "gzip"    # 支持压缩
      )
    )
    
    # 检查获取频道列表是否成功
    if (http_error(channels_response)) {
      stop("获取频道列表请求失败: ", status_code(channels_response), 
           "\n响应内容: ", content(channels_response, as = "text", encoding = "UTF-8"))
    }
    
    message("成功获取第 ", page + 1, " 页频道列表")
    
    # 将响应内容保存到 JSON 文件（可选）
    channels_content <- content(channels_response, as = "text", encoding = "UTF-8")
    
    # 解析渠道列表 JSON，以提取频道列表
    channels_data <- fromJSON(channels_content)
    
    if (!is.null(channels_data$data) && length(channels_data$data) > 0) {
      all_channels[page+1] <- channels_data$data
      message("当前页获取到 ", length(channels_data$data), " 个频道")
      # 继续下一页
      page <- page + 1
    } else {
      message("第 ", page + 1, " 页没有更多频道，停止遍历")
      
      break
    }
  }
  # 提取所有频道的 ID
  channel_ids <- unlist(all_channels)
  
  return(channel_ids)
}

# 获取所有频道的 ID
all_channel_ids <- get_all_channels(paste0(my_url,'/api/channel/'), COOKIE_FILE)

message("总共获取到 ", length(all_channel_ids), " 个频道")

# 将频道 ID 和名称放入数据框
channels_df <- data.frame(
  id = all_channel_ids,
  name = paste0("Channel",1:length(all_channel_ids)),
  stringsAsFactors = FALSE
)

# 查看频道数据框（可选）
print(channels_df)

# 步骤 4：遍历所有渠道并执行 PUT 请求以更新频道信息
# 初始化一个列表存储所有 PUT 响应
put_responses <- data.frame()

# 定义 PUT 请求的基础 URL
put_url <- paste0(my_url,'/api/channel/')
put_response_df<-list()

# 遍历所有频道
for (i in seq_along(all_channel_ids)) {
  channel_id <- all_channel_ids[i]
  channel_name <- names(all_channel_ids)[i]
  message("更新频道 ", i, "/", length(all_channel_ids), " - ID: ", channel_id, ", 名称: ", channel_name)
  
  # 定义 PUT 请求的 JSON 数据
  put_data <- list(
    id = channel_id
  )
  
  # 将列表转换为 JSON 字符串
  put_data_json <- toJSON(put_data, auto_unbox = TRUE, pretty = TRUE)
  
  # 打印生成的 JSON 数据（可选，用于调试）
  # cat("生成的 PUT 请求 JSON 数据:\n")
  # cat(put_data_json, "\n\n")
  
  # 发送 PUT 请求
  put_response <- PUT(
    url = put_url,
    add_headers(
      `sec-ch-ua-platform` = '"Windows"',
      Referer = paste0(my_url,'/panel/channel'),
      `User-Agent` = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/129.0.0.0 Safari/537.36',
      Accept = 'application/json, text/plain, */*',
      `sec-ch-ua` = '"Google Chrome";v="129", "Not=A?Brand";v="8", "Chromium";v="129"',
      `Content-Type` = 'application/json',
      `sec-ch-ua-mobile` = '?0'
    ),
    body = put_data_json,
    encode = "json",
    config = config(
      cookiefile = COOKIE_FILE,  # 使用之前保存的 Cookie
      accept_encoding = "gzip"    # 支持压缩
    )
  )
  
  # 解析 PUT 响应
  put_response_content <- content(put_response, as = "text", encoding = "UTF-8")
  put_response_json <- fromJSON(put_response_content, simplifyVector = TRUE)
  
  # 检查 PUT 请求是否成功
  if (http_error(put_response)) {
    message("PUT 请求失败: ", status_code(put_response))
    put_success <- FALSE
  } else {
    put_success <- isTRUE(put_response_json$success)
    if (put_success) {
      message("PUT 请求成功，频道信息已更新")
    } else {
      message("PUT 请求返回失败信息: ", put_response_json$message)
    }
  }
  put_response_df<-rbind(put_response_df,as.data.frame(put_response_json))
  
  
  # 可选：延迟以避免过于频繁的请求
  Sys.sleep(0.5)  # 暂停 0.5 秒
}


# 保存 PUT 响应数据框到 CSV 文件
write.csv(put_response_df, "put_responses_gptproxy.csv", row.names = FALSE)

