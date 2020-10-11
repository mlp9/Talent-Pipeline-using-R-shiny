level<-function(path)
{
  x <- read.csv(path)
  data <- data.frame(x)
  level<-unique(x['level'])
  return(level)
}
fun<-function(path)
{
  f <- read.csv(path)
  data <- data.frame(f)
  fun<-unique(data$empfunction)
  return(fun)
}
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

plot<-function(result,name){
  value1 <- result 
  #x<-df[1,]
  
  don <- data.frame(
    x<-name,
    
    val=c(value1))
  
  #arrange(val) %<%
  #mutate(x=factor(x, x))
  
  # With a bit more style
  p<-ggplot(don) +
    geom_segment( aes(x=x, xend=x, y=0, yend=val), color="red") +
    geom_point( aes(x=x, y=val), size=5) +
    coord_flip()+
    theme_ipsum() +
    theme(
      legend.position = "none",
      panel.border = element_blank(),
      panel.spacing = unit(1, "lines"),
      strip.text.x = element_text(size = 10)
    ) 
  #ggplotly(p,height=400,width=800)
  ggplotly(p)
}  



#dist(stren,input$file$datapath,input$subid1,input$subid2,input$level,input$fun,input$empid,head_input)


dist<-function(stren1,datapath,ip_subid1,ip_subid2,ip_level,ip_fun,ip_empid,number){
  stren=stren1
  comp <<- as.data.frame(stren[,ip_subid1])
  features <<- as.data.frame(stren[,ip_subid2])
  #
  #datapath
  x <- read.csv(datapath)
  data <- data.frame(x)
  
  data<-subset(data, level==ip_level)##ip_level
  data<-subset(data,empfunction==ip_fun)
  data_name<<-subset(data,level==ip_level)##name
  #print(data)
  data2<<-subset(data, select = c(ip_subid1,ip_subid2))
  data2<<-rbind(data2)
  #print(data2,digits=2)
  
  data <- data.frame(x)
  employee_id<<-subset(data,empid==ip_empid)##ip_empid
  
  employee_id<-subset(employee_id, select = c(ip_subid1,ip_subid2))
  #########
  a<-as.matrix(data2)
  b<-as.matrix(employee_id)
  
  df_new_1<<-data.frame(sqrt(abs(a-b[1,])))
  df_name_data<<-data.frame(data_name$name,df_new_1)
  #print(employee_id,digits = 2)
  ref<-employee_id
  result<-apply(data2,1,function(x) sqrt(sum((x-ref)^2)))
  #result<-round(c(result), digits=2)
  
  #print(result)
  
  result<-normalize(result)
  result<-signif(c(result), digits=3)
  result<<-as.data.frame(result)
  
  #print(result)
  result
  data <- data.frame(x)
  data<-subset(data, level==ip_level)#ip_level
  data<-subset(data,empfunction==ip_fun)
  newDf<<-data.frame("Name"=data[,2],
                     "Distance"=result)
  
  #print(newDf)
  df_sorted_asc <<- newDf[with(newDf, order(result)), ]
  final<-head(df_sorted_asc)
  #head(df_sorted_asc[,c("Name","result")])
  final<<-head(df_sorted_asc[c(1:nrow(df_sorted_asc)),c(1:2)])
  
  #   library(data.table)
  head(df_sorted_asc[c(2:nrow(df_sorted_asc)),c(1:2)])
  final_names<-final[,1]
  
  abc<<-df_name_data[df_name_data$data_name.name %in% final_names,]
  print(head(df_sorted_asc[c(2:nrow(df_sorted_asc)),c(1:2)],n=abs(number)),row.names = FALSE)
  
}




# #############################################################################
inter_dist<-function(stren1,datapath,ip_subid1,ip_subid2,ip_level,ip_fun,ip_empid,number){
  stren=stren1
  comp <- as.data.frame(stren[,ip_subid1])
  features <- as.data.frame(stren[,ip_subid2])
  
  x <- read.csv(datapath)
  data <- data.frame(x)
  
  data<-subset(data, level==ip_level)##ip_level
  data<-subset(data,empfunction==ip_fun)
  data_name<-subset(data,level==ip_level)##name
  #
  data2<-subset(data, select = c(ip_subid1,ip_subid2))
  data2<-rbind(data2)
  
  data <- data.frame(x)
  employee_id<-subset(data,empid==ip_empid)##ip_empid
  
  employee_id<-subset(employee_id, select = c(ip_subid1,ip_subid2))
  
  a<-as.matrix(data2)
  b<-as.matrix(employee_id)
  
  df_new_1<<-data.frame(sqrt(abs(a-b[1,])))
  df_name_data<<-data.frame(data_name$name,df_new_1)
  
  result<-normalize(result)
  #result<-signif(c(result), digits=3)
  #result<<-as.data.frame(result)
  data <- data.frame(x)
  data<-subset(data, level==ip_level)#ip_level
  data<-subset(data,empfunction==ip_fun)
  newDf<<-data.frame("Name"=data[,2],
                     "Distance"=result)
  
  df_sorted_asc <<- newDf[with(newDf, order(result)), ]
  final<-head(df_sorted_asc)
  
  final<<-head(df_sorted_asc[c(2:nrow(df_sorted_asc)),c(1:2)])
  
  # library(data.table)
  final_names<<-head(df_sorted_asc[c(2:nrow(df_sorted_asc)),c(1:2)],n=abs(number))
  #final_names<-final[,1]
  final<-final_names[,1]
  #abc<<-df_name_data[df_name_data$data_name.name %in% final_names,]
  abc<<-df_name_data[df_name_data$data_name.name %in% final,]
  #print(head(df_sorted_asc[c(2:nrow(df_sorted_asc)),c(1:2)]),row.names = FALSE)
  abc
}

############################################################################################
benchmark_dist<-function(stren1,datapath,ip_subid1,ip_subid2,ip_level,ip_fun,ip_competencyvalue,number){
  stren=stren1
  comp <<- as.data.frame(stren[,ip_subid1])
  features <<- as.data.frame(stren[,ip_subid2])
  
  #datapath
  x <- read.csv(datapath)
  data <- data.frame(x)
  
  data<-subset(data, level==ip_level)##ip_level
  data<-subset(data,empfunction==ip_fun)
  data_name<<-subset(data,level==ip_level)##name 
  #print(data)
  data2<<-subset(data, select = c(ip_subid1,ip_subid2))
  data2<<-rbind(data2)
  #print(data2,digits=2)
  
  #data <- data.frame(x)
  #employee_id<<-subset(data,empid==ip_empid)##ip_empid
  
  employee_id1<-as.data.frame(ip_competencyvalue)
  #########
  #a<-as.matrix(data2)
  #b<-as.matrix(employee_id1)
  
  # df_new_1<<-data.frame(sqrt(abs(a-b[1,])))
  #  df_name_data<<-data.frame(data_name$name,df_new_1)
  #print(employee_id,digits = 2)
  ref<-employee_id1 
  result<-apply(data2,1,function(x) sqrt(sum((x-ref)^2)))
  #result<-round(c(result), digits=2)
  
  #print(result)
  
  result<-normalize(result)
  result<-signif(c(result), digits=3)
  result<<-as.data.frame(result)
  
  #print(result)
  result
  data <- data.frame(x)
  data<-subset(data, level==ip_level)#ip_level
  data<-subset(data,empfunction==ip_fun)
  newDf<<-data.frame("Name"=data[,2],
                     "Distance"=result)
  
  #print(newDf)
  df_sorted_asc <<- newDf[with(newDf, order(result)), ]
  final<-head(df_sorted_asc)
  #head(df_sorted_asc[,c("Name","result")])
  final<<-head(df_sorted_asc[c(1:nrow(df_sorted_asc)),c(1:2)])
  
  #library(data.table)
  #head(df_sorted_asc[c(2:nrow(df_sorted_asc)),c(1:2)])
  #final_names<-final[,1]
  
  #abc<<-df_name_data[df_name_data$data_name.name %in% final_names,]
  print(head(df_sorted_asc[c(2:nrow(df_sorted_asc)),c(1:2)],n=abs(number)),row.names = FALSE)
}