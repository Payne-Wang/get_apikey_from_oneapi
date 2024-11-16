library(httr)
library(jsonlite)
library(lubridate)
library(logging)
library(base64enc)
library(dplyr)
library(stringr)

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
  url=modify_url(url,path = "/api/channel/?p=0&page_size=1000&id_sort=true"),  #这里默认只检测第一页，可以写个判断，让其检测所有页
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
  
})%>%sapply(., function(x){c(x$id,x$type,x$key,x$base_url,x$models)},simplify = T)%>%
  
  as.matrix()%>%t()%>%as.data.frame()%>%setNames(c("id","type","key","base_url","models"))->apikey_channelid

#只验证其中包含gpt-4o,o1-mini,o1-preview,claude-3-5-sonnet-20240620,claude-3-5-sonnet-20241022的通道是否可用
interested_models<-c("gpt-4o","chatgpt-4o-latest","gpt-4o-2024-08-06","gpt-4o-2024-05-13",
                     "o1-mini","o1-preview",
                     "claude-3-5-sonnet-20240620","claude-3-5-sonnet-20241022")%>%{
                       type1=paste0(.,",")%>%paste0(.,collapse = "|")
                       type2=paste0(.,"$")%>%paste0(.,collapse = "|")
                       paste(type1,type2,sep = "|")
                     }
apikey_channelid%>%
  filter(models%>%str_detect(.,interested_models))%>%
  mutate(models=str_extract_all(models,interested_models)%>%sapply(.,function(x){paste0(x,collapse = "")}))%>%{
    sapply(1:nrow(.),function(x){
      models=str_split(.[x,"models"],",",simplify = T)%>%as.character%>%.[.!=""]
      id=.[x,"id"]
      sapply(models,function(y){
        tryCatch({
          resp<-GET(
            url=modify_url(url,path = paste0("/api/channel/test/",id,"?model=",y)),
            add_headers(Authorization=paste("Bearer",at),"New-Api-User"="1"),
            timeout(20)
          )
          content(resp)$success
        },error=function(e){return("FALSE")})
      })%>%paste(.,collapse = ",")
    })->res
    
    data.frame(id=.$id,models=.$models,response=res)
  }

#只对openai的渠道进行测活以便将获取的新的key加上
interested_model_2<-c("gpt-4o","o1-mini","o1-preview")%>%{
  type1=paste0(.,",")%>%paste0(.,collapse = "|")
  type2=paste0(.,"$")%>%paste0(.,collapse = "|")
  paste(type1,type2,sep = "|")
}
apikey_channelid%>%
  filter(models%>%str_detect(.,interested_model_2))%>%
  mutate(models=str_extract_all(models,interested_model_2)%>%sapply(.,function(x){paste0(x,collapse = "")}))%>%{
    sapply(1:nrow(.),function(x){
      models=str_split(.[x,"models"],",",simplify = T)%>%as.character%>%.[.!=""]
      id=.[x,"id"]
      sapply(models,function(y){
        tryCatch({
          resp<-GET(
            url=modify_url(url,path = paste0("/api/channel/test/",id,"?model=",y)),
            add_headers(Authorization=paste("Bearer",at),"New-Api-User"="1"),
            timeout(20)
          )
          content(resp)$success
        },error=function(e){return("FALSE")})
      })%>%paste(.,collapse = ",")
    })->res
    
    data.frame(id=.$id,models=.$models,response=res)
  }

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
