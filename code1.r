library(httr)
library(jsonlite)
library(lubridate)
library(logging)
library(base64enc)
library(dplyr)

#使用代理请求
Sys.setenv(http_proxy="http://127.0.0.1:7890")
Sys.setenv(https_proxy="http://127.0.0.1:7890")


#先login获取cookie
url<-""
username<-"root"
password<-"123456"

resp<-POST(
  url = modify_url(url,path = "/api/user/login"),
  body = list(username=username,password=password),
  encode = "json",
  config = config(ssl_verifypeer = FALSE)
)

cookies<-cookies(resp)[["value"]]

#通过cookie获取access token
resp<-GET(
  url=modify_url(url,path = "/api/user/self"),
  add_headers(headers=c("Cookie"=cookies),"New-Api-User"="1")
)
at<-content(resp)$data$access_token

#通过at获取渠道信息
resp<-GET(
  url=modify_url(url,path = "/api/channel/?p=0&page_size=1000"),  #这里默认只检测第一页，可以写个判断，让其检测所有页
  add_headers(Authorization=paste("Bearer",at),"New-Api-User"="1")
)

cha_id<-sapply(content(resp)$data,function(x){x$id})

#通过at获取每个渠道的key
lapply(cha_id,function(x){
  resp_1<-PUT(
    url=modify_url(url,path = "/api/channel"),
    add_headers(Authorization=paste("Bearer",at),"New-Api-User"="1"),
    body = list(id=x),
    encode = "json"
  )
  cha_df<-content(resp_1)$data
  
})


# #通过at获取每个渠道的key,并浅测一下key
# lapply(cha_id,function(x){
#   resp_1<-PUT(
#     url=modify_url(url,path = "/api/channel"),
#     add_headers(Authorization=paste("Bearer",at),"New-Api-User"="1"),
#     body = list(id=x),
#     encode = "json"
#   )
#   cha_df<-as.data.frame(content(resp_1)$data)
#   sapply(cha_df$models%>%str_split(",",simplify = T)%>%as.character,function(y){
#     resp_2<-GET(
#       url = modify_url(url,path = paste0("/api/channel/test/",x,"?model=",y)),
#       add_headers(Authorization=paste("Bearer",at),"New-Api-User"="1"),
#       timeout(3)
#     )
#     content(resp_2)$success
#   })%>%paste(.,collapse = ",")->resp_2
#   cha_df%>%mutate(success=resp_2)
#   
#   
# })%>%Reduce(rbind,.)




# #渠道增删改
# #增
# post_body<-list(
#   id=101,
#   name = "add_test",
#   type = 1,
#   key = "sk-fastgpt",
#   base_url = "",
#   other = "",
#   model_mapping = "{\n\"a\":\"a\"\n\n}",
#   models = "360GPT_S2_V9",
#   groups = list("default"),
#   config = "{}",
#   is_edit = FALSE,
#   group = "default"
# )
# 
# POST(
#   url=modify_url(url,path = "/api/channel"),
#   add_headers(Authorization=paste("Bearer",at),"New-Api-User"="1"),
#   body = post_body,
#   encode = "json"
# )
# 
# #删
# 
# DELETE(
#   url=modify_url(url,path = "/api/channel/100"),
#   add_headers(Authorization=paste("Bearer",at),"New-Api-User"="1")
# )
# #改
# put_body<-list(
#   id=101,
#   name = "modify_test",
#   type = 1,
#   key = "sk-fastgpt",
#   base_url = "",
#   other = "",
#   model_mapping = "{\n\"a\":\"a\"\n\n}",
#   models = "360GPT_S2_V9",
#   groups = list("default"),
#   config = "{}",
#   is_edit = FALSE,
#   group = "default"
# )
# PUT(
#   url=modify_url(url,path = "/api/channel"),
#   add_headers(Authorization=paste("Bearer",at),"New-Api-User"="1"),
#   body = put_body,
#   encode = "json"
# )
