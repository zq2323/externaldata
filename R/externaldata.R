externaldata<-function(){
  
  test <- file("test.log")
  
  sink(test,append = TRUE)
  
  sink(test,append = TRUE,type = "message")
  
  if(!require(pacman)){install.packages('pacman')}
  
  library(pacman)
  
  p_load('shiny','stringr','caTools','shinyjs')
  
  
  
  ui<-shinyUI(fluidPage(
    
    
    
    useShinyjs(),
    
    tags$div(style = "height: 10em;"),
    
    column(5,align='center',offset = 3,
           
           h2('外部数据下载'),
           
           tags$div(style = "height: 2em;"),
           
           textInput('username','用户名 :'),
           
           passwordInput('psw','密码 :'),
           
           textInput('link','链接:'),
           
           textInput('md5input','输入MD5 :'),
           
           tags$div(style = "height: 1em;"),
           
           selectInput('download_position','选择下载位置:',choices=c('','C:','D:','E:','F:'),selected = NULL),
           
           tags$div(style = "height: 1em;"),
           
           textOutput('remd5'),
           
           tags$div(style = "height: 1em;"),
           
           actionButton('download_data','执行'),
           
           tags$div(style = "height: 1em;"),
           
           textOutput('dstatus')
           
    )
    
  )
  
  )
  
  server <- function(input, output) {
    
    
    
    elink <- reactive({
      
      enc2native(input$link)
      
    })
    
    
    
    durl <- reactive({
      
      uname<-input$username
      
      password<-input$psw
      
      lin <- elink()
      
      ben64<-base64encode(paste0(uname,'!',password))
      
      url<-sub('info=.*?token',paste0('info=',ben64,'&token'),lin)
      
      url <- str_replace_all(url,'\\|','\\&')
      
      enc2native(url)
      
    })
    
    
    
    filename<- reactive({
      
      lin <- elink()
      
      fname<- str_replace(str_extract(lin,'filedata=.*'),'filedata=','')
      
      fdecode <- base64decode(fname,'character')
      
      fname<-str_split(fdecode,',')
      
      fname <- unlist(fname)
      
      fname
      
    })
    
    
    
    filename2 <- reactive({
      
      lin <- elink()
      
      fname2 <- str_replace(str_replace(str_extract(lin,'filename=.*?\\.'),'filename=',''),'\\.','')
      
      enc2native(fname2)
      
    })
    
    
    
    filetype<-reactive({
      
      lin <-elink()
      
      ftype<-str_replace(str_extract(str_extract(lin,'filename=.*?info'),'\\..*?\\|'),'\\|','')
      
      ftype
      
    })
    
    
    
    vals<-reactiveValues()
    
    rmd <-reactiveValues()
    
    rmd$Data<-''
    
    vals$Data<-'E:'
    
    observeEvent(input$download_position,{
      
      if(input$download_position != ""){
        
        path1<- paste0(input$download_position,'/',str_replace(filename()[3],'/',''))
        
        if(!dir.exists(path1)){
          
          dir.create(path1)
          
        }
        
        path2<- paste0(input$download_position,'/',str_replace(filename()[3],'/',''),'/',str_replace(filename()[2],'/',''))
        
        if(!dir.exists(path2)){
          
          dir.create(path2)
          
        }
        
        vals$Data <- path2
        
      }
      
    }
    
    )
    
    
    
    inputposition <- reactive({
      
      input$diwnload_position
      
    })
    
    
    
    
    
    status<-reactiveValues(changevalue1=FALSE,changevalue2=FALSE)
    
    
    
    
    
    
    
    observeEvent(input$download_data,{
      
      download.file(paste0(durl(),'&pid=',filename()[1]),paste0(vals$Data,'/',filename2(),filetype()),method = "curl")
      
      commands<-paste0('certutil -hashfile ',paste0('"',vals$Data,'/',filename2(),filetype(),'"',' MD5'),' ','> ',paste0('"',vals$Data,'/',filename2(),'-','md5.txt','"'))
      
      shell(commands)
      
      con<-file(paste0(vals$Data,'/',filename2(),'-','md5.txt'))
      
      mdline<-readLines(con,n=-1)
      
      rmd$Data <- mdline[2]
      dmd5<-paste0("下载文件MD5：",mdline[2])
      smd5<-paste0("服务器端文件MD5：",inputmd5())
      pid<-paste0("项目编号：",filename()[3])
      fid<-paste0("文件类型: ",filename()[2])
      dname<-paste0("文件名：",filename2())
      uname<-paste0("下载ID：",inputusername())
      ti<-paste0("下载时间: ",Sys.time())
      t<-c(dmd5,smd5,pid,fid,dname,uname,ti)
      writeLines(t,con,sep="\n")
      close(con)
      
    }
    
    )
    
    
    
    inputmd5<-reactive({
      
      if(!is.null(input$md5input)){
        
        imd5 <- input$md5input
        
        imd5
        
      }
      
    })
    
    inputusername<-reactive({
      
      if(!is.null(input$username)){
        
        iuser <- input$username
        
        iuser
        
      }
      
    })
    
    
    
    
    
    output$remd5 <- renderText(
      
      if(rmd$Data !=''){
        
        if(rmd$Data == inputmd5()){
          
          paste0('下载文件MD5 ：',rmd$Data,'       验证结果 : MD5一致')
          
        }else{
          
          paste0('下载文件MD5 ：',rmd$Data,'       验证结果 : MD5不一致')
          
        }
        
      }else{
        
        'MD5待验证'
        
      }
      
      
      
    )
    
    
    
    
    
    
    
    observe({
      
      if (rmd$Data !=''){
        
        output$dstatus <- renderText('下载完成，已关闭页面')
        
        status$changevalue1 <- TRUE
        
      }else
        
      {
        
        output$dstatus <- renderText('等待下载')
        
      }
      
    })
    
    
    
    observe({
      
      if(status$changevalue1){
        
        
        
        status$changevalue2 <- TRUE
        
        
        
        Sys.sleep(5)
        
      }
      
      
      
    })
    
    
    
    observe({
      
      if(status$changevalue2){
        
        stopApp()
        
      }
      
    })
    
    
    
    
    
  }
  
  runApp(list(ui=ui,server=server),host="127.0.0.1",port=7264,launch.browser = TRUE)
  
  
  
  cat("\014")
  
  sink()
  
  sink(type="message")
  
  close(test)
  
  file.remove("test.log")
  
}
