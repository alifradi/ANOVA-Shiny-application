library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(formattable)
library(nlme)

ui <- fluidPage(
  helpText(h6("Powerd by")),
  tags$a(href='https://www.linkedin.com/in/frady-ali-ab8700132/',tags$h6("FRADY Ali")),
  tags$a(href='http://data-expert.net/',tags$img(src='alidata.png',height=50,width=150)),
  sidebarLayout(
    sidebarPanel(
      fileInput('file1', 'Upload your  CSV File'),
      radioButtons(inputId='Anova', label='Select type of Analyis Of Variance', choices=list('1-Way ANOVA','2-Way ANOVA'), selected='1-Way ANOVA'),
      htmlOutput("variables"),
      numericInput(inputId="n", label="n", value=4, min=3,step=1),
      sliderInput(inputId = 'alpha',label ='Significant level',min = 0.001,max =1,value = 0.05,step = 0.01),
      htmlOutput("facteurs"),
      htmlOutput("block"),
      htmlOutput("sub")),
    mainPanel(
      checkboxInput(inputId='displayDT', label='Display Data'),
      uiOutput("tbd"),
      uiOutput("tb")
      
    ))
)

# Server
server <- function(input, output) {
  myData1 <- reactive({
    inFile <- input$file1
    if (is.null(inFile)) return(NULL)
    data <- read.csv(inFile$datapath, header = TRUE,row.names=1,sep = ",")
    data
  })
  output$TD <- renderDataTable({
    data <- myData1()
    DT::datatable(data = data,options = list(pageLength = 5))
  })
  output$variables <- renderUI({
    req(myData1())
    df_init <- myData1() 
    for(i in c(1:ncol(df_init))){
      if((class(df_init[,i])=="integer") && length(unique(df_init[,i]))<=input$n){df_init[,i]<-factor(df_init[,i])}}
    x=sapply(df_init,class)
    x=(x=="numeric")
    df=df_init[,x]
    if (identical(df, '') || identical(df,data.frame())) return(NULL)
    selectInput(inputId = "V1", label = "Variables to use: Y", choices=names(df), selected=names(df[1]))
  })
  output$facteurs <- renderUI({
    req(myData1())
    df_init <- myData1() 
    for(i in c(1:ncol(df_init))){
      if((class(df_init[,i])=="integer") && length(unique(df_init[,i]))<=input$n){df_init[,i]<-factor(df_init[,i])}}
    x=sapply(df_init,class)
    x=(x=="factor")
    df=df_init[,x]
    if (identical(df, '') || identical(df,data.frame())) return(NULL)
    if(input$Anova=='1-Way ANOVA'){selectInput(inputId = "F1", label = "Factor to use: X", choices=names(df), selected=names(df))
     
    }
    else if(input$Anova=='2-Way ANOVA'){
      verticalLayout(
        selectInput(inputId = "F11", label = "Factors to use: X1", choices=names(df),selected =names(df)[2]),
        selectInput(inputId = "F12", label = "Factors to use: X2", choices=names(df)))
      
    }
   
  })
  output$block <- renderUI({
    req(myData1())
    df_init <- myData1() 
    for(i in c(1:ncol(df_init))){
      if((class(df_init[,i])=="integer") && length(unique(df_init[,i]))<=input$n){df_init[,i]<-factor(df_init[,i])}}
    x=sapply(df_init,class)
    x=(x=="factor")
    df=df_init[,x]
    if (identical(df, '') || identical(df,data.frame())) return(NULL)
     selectInput(inputId = "sujet", label = "Blocking variable", choices=names(df))})
  output$mod<-renderUI({
    data<- myData1() 
    df=levels(data[,input$F12])
    selectInput(inputId = "sub",label = "Select factor X2 modality:",choices =df)
  })
  #one way ANOVA
  output$main1<-renderText({t=paste("Boxplot of",input$V1,"by",input$F1)})
  output$p<-renderPlot({
    data<-myData1()
    if (is.null(data)) return()
    v=c(input$V1,input$F1)
    x=data[,v]
    Variable=x[,1]
    Factor=x[,2]
    #g=length(unique(data[,input$V1]))
    ##grid(nx=NA, ny=g) #grid over boxplot
    #par(new=TRUE)
   # boxplot(x[,1]~x[,2],main=t,col="green",xlab=input$F1,ylab=input$V1,boxwex=0.35)
    qplot( x=Factor , y=Variable , data=x , geom=c("boxplot","jitter") , fill=Factor)
  })
  output$binarModel<-renderText({
    data<-myData1()
    if (is.null(data)) return("Enter your data!")
    v=c(input$V1,input$F1)
    x=data[,v]
    X=x[,2]
    Y=x[,1]
    l=anova(lm(Y~X))
    
    if(l$`Pr(>F)`[1]<input$alpha){return(paste('p-Value= ',format(l$`Pr(>F)`[1], digits=1, nsmall=4),"| There is at least a siginficant difference between two of means"))}
    else return(paste('p-Value= ',format(l$`Pr(>F)`[1], digits=1, nsmall=4),"| There is no significant difference between means"))
    
  })
  output$pi<-renderPlot({
    data<-myData1()
    if (is.null(data)) return()
    x=data[,input$F1]
    y=data[,input$V1]
    par(mfrow=c(2,2))
    plot (lm(y~x),col.lab='#1D1DE9' ,col.smooth=" red",pch = 16,col = '#1D1DE9')
  })
  output$normali<-renderPlot({
    data<-myData1()
    if (is.null(data)) return()
    x=data[,input$F1]
    y=data[,input$V1]
    t=paste('Histogram of residuals',input$V1,'~',input$F1)
    q=lm(y~x, data=data)
    hist(residuals(q),main=t,xlab = 'residuals',ylab = 'Frequency',border = '#FEFEFE',col = '#E8A201',col.lab='#1D1DE9',col.axis='#140091')
    })
  output$kolSim1<-renderText({
    data<-myData1()
    if (is.null(data)) return("Enter your data!")
    v=c(input$V1,input$F1)
    x=data[,v]
    X=x[,2]
    Y=x[,1]
    fit=lm(Y~X, data=data)
    t=ks.test(resid(fit), y= pnorm)
    paste('p-Value= ',format(t$p.value , digits=1, nsmall=4))
  })
  output$kolSim2<-renderText({
    data<-myData1()
    if (is.null(data)) return("Enter your data!")
    v=c(input$V1,input$F1)
    x=data[,v]
    X=x[,2]
    Y=x[,1]
    fit=lm(Y~X, data=data)
    t=ks.test(resid(fit), y= pnorm)
    paste('D Statistic= ',format(t$statistic , digits=1, nsmall=4))
  })
  output$pe<-renderPlot({
    data<-myData1()
    if (is.null(data)) return()
    X_factor=data[,input$F1]
    y=data[,input$V1]
    t<- aov(y~X_factor)
    TukeyHSD(t)
    if(input$par){par(las=1)}
    else par(las=0)
    par(mar=c(0,7,3,3))
    plot(TukeyHSD(t,ordered = TRUE,conf.level =input$CL))
    })
  output$BarlettTest1<-renderText({
    data<-myData1()
    if (is.null(data)) return("Enter your data!")
    v=c(input$V1,input$F1)
    x=data[,v]
    X=x[,2]
    Y=x[,1]
    t=bartlett.test(Y~X)
    paste('p-value:',format(t$p.value , digits=1, nsmall=4))
  })
  output$BarlettTest2<-renderText({
    data<-myData1()
    if (is.null(data)) return("Enter your data!")
    v=c(input$V1,input$F1)
    x=data[,v]
    X=x[,2]
    Y=x[,1]
    t=bartlett.test(Y~X)
    paste('Bartlett s K-squared statistic: ',format(t$statistic, digits=1, nsmall=4))
  })
  output$BT<-renderUI({
    data<-myData1()
    if (is.null(data)) return()
    v=c(input$V1,input$F1)
    x=data[,v]
    X=x[,2]
    Y=x[,1]
    t=bartlett.test(Y~X)
    if(t$p.value>0.05){tags$img(src='good.png',height=60,width=90)}
    else {tags$img(src='bad.png',height=60,width=65)}
  })
  output$KS<-renderUI({
    data<-myData1()
    if (is.null(data)) return()
    v=c(input$V1,input$F1)
    x=data[,v]
    Y=x[,1]
    t= t=ks.test(Y, "pnorm", mean(Y),sd(Y))
    if(t$p.value>0.05){tags$img(src='good.png',height=60,width=90)}
    else {tags$img(src='bad.png',height=60,width=65)}
  })
  output$Tab <- DT::renderDataTable( {
    data<-myData1()
    v=c(input$V1,input$F1)
    x=data[,v]
    s=do.call(rbind, tapply(x[,1],x[,2], summary))
    s[,4] = round(x = s[,4],digits = 2)
    s=as.data.frame(s)
    return(as.datatable(formattable(s, list(area(col = Mean) ~ normalize_bar("#78F44B", 0.6)))))
  })
  output$table1 <- DT::renderDataTable( {
    data<-myData1()
    if (is.null(data)) return()
    v=c(input$V1,input$F1)
    x=data[,v]
    X=x[,2]
    Y=x[,1]
    t=list()
    t$names=levels(X)
    t$index=1:length(levels(X))
    i=which(t$names==input$refi)
    s=summary(lm(Y~C(X,base=i)))
    s=as.data.frame(s$coefficients)
    tu=t$names[t$names!=t$names[i]]
    tu=c(t$names[i],tu)
    row.names(s)=c()
    row.names(s)<-tu
    s[,1] = round(x = s[,1],digits = 2)
    s[,2] = round(x = s[,2],digits = 2)
    s[,3] = round(x = s[,3],digits = 2)
    s[,4] =round(x = s[,4],digits = 6)
    s$Significant=s[,4]<input$alpha
    return(as.datatable(formattable(s, list(Significant = formatter("span",
                                                                    style = x ~ style(color = ifelse(x, "green", "red")),
                                                                    x ~ icontext(ifelse(x, "ok", "remove"), ifelse(x, "Yes", "No"))) ))))
  })
  output$table2 <- DT::renderDataTable( {
    data<-myData1()
    if (is.null(data)) return()
    v=c(input$V1,input$F1)
    x=data[,v]
    X=x[,2]
    Y=x[,1]
    mon.aov <- aov(Y~X)
    t=TukeyHSD(mon.aov,conf.level =input$CL)
    s=as.data.frame(t$X)
    s[,1] = round(x = s[,1],digits = 4)
    s[,2] = round(x = s[,2],digits = 4)
    s[,3] = round(x = s[,3],digits = 4)
    s[,4] = round(x = s[,4],digits = 4)
    colnames(s)=c("Range of interval","Low. bound","Up. bound","Adj. p-Value")
    s$Significant_difference=s[,4]<input$alpha
    return(as.datatable(formattable(s, list(Significant_difference = formatter("span",
                                                                    style = x ~ style(color = ifelse(x, "green", "red")),
                                                                    x ~ icontext(ifelse(x, "ok", "remove"), ifelse(x, "Yes", "No"))) ))))
    
    })
  output$table3 <- DT::renderDataTable( {
    data<-myData1()
    if (is.null(data)) return()
    v=c(input$V1,input$F1)
    x=data[,v]
    X=x[,2]
    Y=x[,1]
    k=pairwise.t.test(Y,X,p.adjust="bonf")
    s= round(x =k$p.value,digits = 4)
    s[is.na(s)] <- ""
    s=as.data.frame(s)
    n=nrow(s)
    return(as.datatable(formattable(s, list(area(col = 1:n)~color_tile("#78F44B","transparent") ))))
    })
  output$table4 <- DT::renderDataTable( {
    data<-myData1()
    if (is.null(data)) return("Enter your data!")
    v=c(input$V1,input$F1)
    x=data[,v]
    X=x[,2]
    Y=x[,1]
    l=anova(lm(Y~X))
    s=as.data.frame(l)
   row.names(s)=c(input$F1,'Residuals')
   s[,2] = round(x = s[,2],digits = 2)
   s[,3] = round(x = s[,3],digits = 2)
   s[,4] = round(x = s[,4],digits = 2)
   s[,5] = round(x = s[,5],digits = 6)
   s$Significant=s[,5]<input$alpha
   s$Significant[2]=""
   s[is.na(s)] <- ""
   return(as.datatable(formattable(s, list(Significant = formatter("span",
                                                                              style = x ~ style(color = ifelse(x, "green", "red")),
                                                                              x ~ icontext(ifelse(x, "ok", "remove"), ifelse(x, "Yes", "No"))) ))))
   
   
  })
  output$table5<-renderPrint( {
    data<-myData1()
    if (is.null(data)) return()
    v=c(input$V1,input$F1,input$sujet)
    x=data[,v]
    Factor_X=x[,2]
    Variable=x[,1]
    Blocking_Variable=x[,3]
    t=summary(aov(Variable~Factor_X+Error(Blocking_Variable/Factor_X)))  
    t
  })

  # Two way ANOVA
  output$main2<-renderText({t=paste("Boxplot of",input$V1,"by",input$F11," : ",input$F12)})
  output$table <- renderTable({
    data <- myData1()
    data <- subset(data,data[,input$F12]==input$sub)
    v=c(input$V1,input$F11,input$F12)
    data[,v]
  })
  output$pi2<-renderPlot({
    data<-myData1()
    if (is.null(data)) return()
    v=c(input$V1,input$F11,input$F12)
    x=data[,v]
    y=x[,1]
    z=x[,2]
    Factor_X2_modalities=x[,3]
    interaction.plot(z,Factor_X2_modalities,y,ylab = input$V1,xlab = input$F11,leg.bg = input$F12,col = 1:length(levels(Factor_X2_modalities)))
  })
  output$pi22<-renderPlot({
    data<-myData1()
    if (is.null(data)) return()
    x=data[,input$F12]
    y=data[,input$V1]
    z=data[,input$F11]
    par(mfrow=c(2,2))
    plot (lm(y~x+z), col.lab='#1D1DE9' ,col.smooth=" red",pch = 16,col = '#1D1DE9')
  })
  output$normsub<-renderPlot({
    data<-myData1()
    data<- subset(data,data[,input$F12]==input$sub)
    if (is.null(data)) return()
    y=data[,input$V1]
    t=paste('Histogram of',input$V1,'|',input$F12,'=',input$sub)
    hist(y,main=t,xlab = input$V1,ylab = 'Frequency',border = '#FEFEFE',col = '#E8A201',col.lab='#1D1DE9',col.axis='#140091')
  })
  output$table12 <- DT::renderDataTable( {
    data<-myData1()
    if (is.null(data)) return()
    v=c(input$V1,input$F11,input$F12)
    x=data[,v]
    Variable=x[,1]
    Factor_X1=x[,2]
    Factor_X2=x[,3]
    s=do.call(rbind,summary(aov(lm(Variable~ Factor_X1*Factor_X2,data=data))) )
    s[,2] = round(x = s[,2],digits = 2)
    s[,3] = round(x = s[,3],digits = 2)
    s[,4] =round(x = s[,4],digits = 2)
    s[,5] =round(x = s[,5],digits = 6)
    s[is.na(s)] <- ""
    as.data.frame(s)
    s$Significant=s[,5]<input$alpha
    s[ nrow(s), ncol(s)]=""
   
   
    return(as.datatable(formattable(s, list(Significant = formatter("span",
                                                                    style = x ~ style(color = ifelse(x, "green", "red")),
                                                                    x ~ icontext(ifelse(x, "ok", "remove"), ifelse(x, "Yes", "No"))) ))))
    
    
  })#step1 ANOVA
  output$table13 <- DT::renderDataTable( {
    data<-myData1()
    if (is.null(data)) return()
    v=c(input$V1,input$F11,input$F12)
    t=myData1()[,v]
    m=summary(aov(t[,1]~t[,2]*t[,3],data=data))
    m=do.call(rbind,m)
    as.data.frame(m)
    data<- subset(data,data[,input$F12]==input$sub)
    x=data[,v]
    Variable=x[,1]
    Factor_X1=x[,2]
    Factor_X2=x[,3]
    s=summary(aov(lm(Variable~ Factor_X1,data=data)))
    s=do.call(rbind,s)
    g=s[1,3]/m[1,3]
    Pv=1-pf(g,df1 =s[1,1] ,df2 =m[4,1] )
    s[1,5]=Pv
    s[,2] = round(x = s[,2],digits = 2)
    s[,3] = round(x = s[,3],digits = 2)
    s[,4] =round(x = s[,4],digits = 2)
    s[,5] =round(x = s[,5],digits = 6)
    s[is.na(s)] <- ""
    as.data.frame(s)
    s$Significant=s[,5]<input$alpha
    s[ nrow(s), ncol(s)]=""
    
    
    return(as.datatable(formattable(s, list(Significant = formatter("span",
                                                                    style = x ~ style(color = ifelse(x, "green", "red")),
                                                                    x ~ icontext(ifelse(x, "ok", "remove"), ifelse(x, "Yes", "No"))) ))))
    
    
    
  })#subset of data
  output$table14 <-renderDataTable({
    data<-myData1()
    if (is.null(data)) return()
    v=c(input$V1,input$F11,input$F12)
    x=data[,v]
    Y=x[,1]
    X1=x[,2]
    X2=x[,3]
    s=summary(lm(Y~X1*X2,data=data))
    s=s$coefficients
    s[,1] = round(x = s[,1],digits = 2)
    s[,2] = round(x = s[,2],digits = 2)
    s[,3] = round(x = s[,3],digits = 2)
    s[,4] =round(x = s[,4],digits = 6)
    l=c(levels(X1),levels(X2))
    h=nchar(l[2])
    for (i in 3:length(l)) {
      if(nchar(l[i])>h){h=nchar(l[i])}
    }
    h=h+3
    h=max(h,nchar("(Intercept)"))
    w=vector()
    w[1]=nchar(row.names(s)[1])<h
    w[1]
    for (i in 2:nrow(s)) {
      w[i]=nchar(row.names(s)[i])<h
    }
    s=s[w,]
    row.names(s)[1]=paste('X1',l[1])
    s=as.data.frame(s)
    s$Significant=s[,4]<input$alpha
    return(as.datatable(formattable(s, list(Significant = formatter("span",
                                                                    style = x ~ style(color = ifelse(x, "green", "red")),
                                                                    x ~ icontext(ifelse(x, "ok", "remove"), ifelse(x, "Yes", "No"))) ))))
    
    })#estimate parameters
  output$table15<-renderPrint( {
    data<-myData1()
    if (is.null(data)) return()
    v=c(input$V1,input$F11,input$F12,input$sujet)
    x=data[,v]
    variable=x[,1]
    Factor_X1=x[,2]
    Factor_X2=x[,3]
    Variable=x[,1]
    Blocking_Variable=x[,4]
    t=summary(aov(Variable~Factor_X1*Factor_X2+Error(Blocking_Variable/Factor_X1*Factor_X2)))  
    t
  })
  output$table16<-renderPrint( {
    data<-myData1()
    if (is.null(data)) return()
    v=c(input$V1,input$F11,input$F12,input$sujet)
    x=data[,v]
    variable=x[,1]
    Factor_X1=x[,2]
    Factor_X2=x[,3]
    Variable=x[,1]
    Blocking_Variable=x[,4]
    t=summary(aov(Variable~Factor_X1*Factor_X2+Error(Blocking_Variable:Factor_X2)))  
    t
  })
  output$pa2<-renderPlot({
    data<-myData1()
    if (is.null(data)) return()
    v=c(input$V1,input$F11,input$F12)
    x=data[,v]
    Variable=x[,1]
    Factor_X1=x[,2]
    Factor_X2=x[,3]
    ggplot(x, aes(x=Factor_X1, y=Variable, fill=Factor_X2)) + 
      geom_boxplot() +
      facet_wrap(~x[,2], scale="free")
    
   # boxplot(x[,1]~x[,2]:x[,3],main=t,col="green",xlab=u,ylab=input$V1,boxwex=0.9)
  })
  output$pa3<-renderPlot({
    data<-myData1()
    if (is.null(data)) return()
    v=c(input$V1,input$F11,input$F12)
    x=data[,v]
    Variable=x[,1]
    Factor_X1=x[,2]
    Factor_X2=x[,3]
    ggplot(x, aes(x=Factor_X1, y=Variable, fill=Factor_X2)) + 
      geom_boxplot()
  })
  output$Tab2 <- renderDataTable( {
    data<-myData1()
    if (is.null(data)) return()
    v=c(input$V1,input$F11,input$F12)
    x=data[,v]
    s=summary(x)
    s=do.call(rbind, tapply(x[,1],x[,2]:x[,3], summary))
    s[,4] = round(x = s[,4],digits = 2)
    s=as.data.frame(s)
    return(as.datatable(formattable(s, list(area(col = Mean) ~ normalize_bar("#78F44B", 0.6)))))
    
  })
  output$ShapiroTest1<-renderText({
    data<-myData1()
    if (is.null(data)) return()
    v=c(input$V1,input$F11,input$F12)
    x=data[,v]
    t=shapiro.test(residuals(object = lm(x[,1]~x[,2]*x[,3],data=data)))
    paste('p-value:',round(x = t$p.value,digits = 6))
  })
  output$SH<-renderUI({
    data<-myData1()
    if (is.null(data)) return()
    v=c(input$V1,input$F11,input$F12)
    x=data[,v]
    t=shapiro.test(residuals(object = lm(x[,1]~x[,2]*x[,3],data=data)))
    if(t$p.value>0.05){tags$img(src='good.png',height=60,width=90)}
    else {tags$img(src='bad.png',height=60,width=65)}
  })
  output$Tuk<-renderDataTable({
    data<-myData1()
    if (is.null(data)) return()
    v=c(input$V1,input$F11,input$F12)
    x=data[,v]
    Variable=x[,1]
    Factor_X1=x[,2]
    Factor_X2=x[,3]
    a=aov(lm(Variable~ Factor_X1*Factor_X2,data=data))
    b=TukeyHSD(a,ordered = TRUE,conf.level =input$alpha2)
    s=as.data.frame(b$Factor_X1)
    s[,1] = round(x = s[,1],digits = 4)
    s[,2] = round(x = s[,2],digits = 4)
    s[,3] = round(x = s[,3],digits = 4)
    s[,4] = round(x = s[,4],digits = 4)
    colnames(s)=c("Range of interval","Low. bound","Up. bound","Adj. p-Value")
    s$Significant_difference=s[,4]<input$alpha
    return(as.datatable(formattable(s, list(Significant_difference = formatter("span",
                                                                               style = x ~ style(color = ifelse(x, "green", "red")),
                                                                               x ~ icontext(ifelse(x, "ok", "remove"), ifelse(x, "Yes", "No"))) ))))
    
    
  })
  output$Tuke<-renderDataTable({
    data<-myData1()
    if (is.null(data)) return()
    v=c(input$V1,input$F11,input$F12)
    x=data[,v]
    Variable=x[,1]
    Factor_X1=x[,2]
    Factor_X2=x[,3]
    a=aov(lm(Variable~ Factor_X1*Factor_X2,data=data))
    b=TukeyHSD(a,ordered = TRUE,conf.level =input$alpha2)
    s=as.data.frame(b$Factor_X2)
    s[,1] = round(x = s[,1],digits = 4)
    s[,2] = round(x = s[,2],digits = 4)
    s[,3] = round(x = s[,3],digits = 4)
    s[,4] = round(x = s[,4],digits = 4)
    colnames(s)=c("Range of interval","Low. bound","Up. bound","Adj. p-Value")
    s$Significant_difference=s[,4]<input$alpha
    return(as.datatable(formattable(s, list(Significant_difference = formatter("span",
                                                                               style = x ~ style(color = ifelse(x, "green", "red")),
                                                                               x ~ icontext(ifelse(x, "ok", "remove"), ifelse(x, "Yes", "No"))) ))))
    
    
  })
  output$Tuktab1<-renderText({input$F11})
  output$Tuktab2<-renderText({input$F12})
  output$plottuk1<-renderPlot({
    data<-myData1()
    if (is.null(data)) return()
    v=c(input$V1,input$F11,input$F12)
    x=data[,v]
    Variable=x[,1]
    Factor_X1=x[,2]
    Factor_X2=x[,3]
    a=aov(lm(Variable~ Factor_X1,data=data))
    TukeyHSD(a)
    if(input$para){par(las=1)}
    else par(las=0)
    par(mar=c(0,7,3,3))
    plot(TukeyHSD(a,ordered = TRUE,conf.level =input$alpha2))
    
  })
  output$plottuk2<-renderPlot({data<-myData1()
  if (is.null(data)) return()
  v=c(input$V1,input$F11,input$F12)
  x=data[,v]
  Variable=x[,1]
  Factor_X1=x[,2]
  Factor_X2=x[,3]
  a=aov(lm(Variable~ Factor_X2,data=data))
  TukeyHSD(a)
  if(input$para){par(las=1)}
  else par(las=0)
  par(mar=c(0,7,3,3))
  plot(TukeyHSD(a,ordered = TRUE,conf.level =input$alpha2))})
  output$freqTable<-renderDataTable({
    data<-myData1()
    if (is.null(data)) return()
    v=c(input$V1,input$F11,input$F12)
    x=data[,v]
    Variable=x[,1]
    Factor_X1=x[,2]
    Factor_X2=x[,3]
    t=table(Factor_X1,Factor_X2)
    n=nrow(t)
    m=ncol(t)
    t=array(t)
    t=matrix(t,n,m)
    colnames(t)=levels(Factor_X2)
    row.names(t)=levels(Factor_X1)
    t=as.data.frame(t)
    
    u=table(Factor_X1,Factor_X2)
    n=nrow(u)
    m=ncol(u)
    u=array(u)
    p=max(m,n)
    u=matrix(u,m,n,byrow = TRUE)
    colnames(u)=levels(Factor_X1)
    row.names(u)=levels(Factor_X2)
    u=as.data.frame(u)
    if(input$showtab){ DT::datatable(data = t,options = list(pageLength = p))}
    else return(DT::datatable(data = u,options = list(pageLength = p)))
   })
  output$Fl1<-renderText({
    data<-myData1()
    if (is.null(data)) return()
    v=c(input$V1,input$F11,input$F12)
    data=data[,v]
    t=fligner.test(data[,1]~interaction(data[,2],data[,3]))
    paste('p-value:',format(t$p.value , digits=1, nsmall=4))
   
  })
  output$Fl2<-renderText({
    data<-myData1()
    if (is.null(data)) return()
    v=c(input$V1,input$F11,input$F12)
    data=data[,v]
    t=fligner.test(data[,1]~interaction(data[,2],data[,3]))
    paste('Fligner-Killeen s K-squared statistic: ',format(t$statistic, digits=1, nsmall=4))
  })
  output$Fl<-renderUI({
    data<-myData1()
    if (is.null(data)) return()
    v=c(input$V1,input$F11,input$F12)
    data=data[,v]
    t=fligner.test(data[,1]~interaction(data[,2],data[,3]))
    if(t$p.value>0.05){tags$img(src='good.png',height=60,width=90)}
    else {tags$img(src='bad.png',height=60,width=65)}
  })
  output$shap<-renderUI({
    data<-myData1()
    if (is.null(data)) return()
    v=c(input$V1,input$F11,input$F12)
    x=data[,v]
    t=shapiro.test(residuals(object = lm(x[,1]~x[,2]*x[,3],data=data)))
    if(t$p.value>0.05){tags$img(src='good.png',height=60,width=90)}
    else {tags$img(src='bad.png',height=60,width=65)}
  })
  #Display
  output$tb <- renderUI({
    data<- myData1() 
    x=data[,input$V1]
    v=c(input$V1,input$F1)
    y=data[,v]
    X=y[,2]
    Y=y[,1]
    t=paste('Varaiblity lavel of "',input$F12,'"')
    if (is.null(myData1()))
      h3("Watch me - Tutorial",br(),tags$video(src='ali.mp4',type="video/mp4",width="720px",height="450px",controls="controls"),align="center")
    else if(is.null(myData1())==FALSE && input$Anova=='1-Way ANOVA')
    {tabsetPanel(tabPanel("Check normality",
                          plotOutput("normali"),
                          h3("Kolmogorov Smirnov test"),splitLayout(verticalLayout( textOutput("kolSim1"), textOutput("kolSim2"),
                                                                                    tags$head(tags$style("#kolSim1{color: #07006B;
                                                font-size: 18px;font-style: bold; }" ))
                                                                                    ,tags$head(tags$style("#kolSim2{color: #07006B;
                                                font-size: 18px;font-style: bold; }" ))),
                                                                    uiOutput('KS')),
                          h3("Residuals analysis"),
                          plotOutput("pi")
    ),
                 tabPanel("Check homoscedasticity",
                          h3("Summary inputs"),
                          DT::dataTableOutput("Tab"), 
                          h3("Bartlett test of homogeneity of variances"),
                          splitLayout( verticalLayout(textOutput("BarlettTest1"),textOutput("BarlettTest2")
                                                      ,tags$head(tags$style("#BarlettTest1{color: #07006B;
                                                                            font-size: 18px;font-style: bold; }" )),
                                                      tags$head(tags$style("#BarlettTest2{color: #07006B;
                                                                           font-size: 18px;font-style: bold; }" ))),
                                       uiOutput("BT")),
                          h3("Boxplot of inputs"),
                          h4(textOutput("main1"),align='center'),
                          tags$head(tags$style("#main1{color: red;font-size: 18px;font-style: bold; }" )),
                          plotOutput("p")
                          ),
                tabPanel("Test hypotheses",
                          textOutput("binarModel"),
                          tags$head(tags$style("#binarModel{color: #07006B;font-size: 18px;font-style: bold; }" )),
                          DT::dataTableOutput("table4"),
                          h3("Descriptive analysis"),
                          selectInput(inputId = "refi",label = 'Reference of comparing',choices = levels(X),selected = levels(X)[1]),
                          DT::dataTableOutput("table1") 
    ),
                 tabPanel("Test of diffreneces in means",
                          uiOutput("tests")
                 ),
                 tabPanel("ANOVA with replications",verbatimTextOutput("table5") )
    )}
    else if (is.null(myData1())==FALSE && input$Anova=='2-Way ANOVA'){
      tabsetPanel(
        tabPanel("Check normality",
                 h3("Shapiro-Wilk test"),
                 splitLayout(textOutput("ShapiroTest1"),
                             uiOutput("SH")),
                 tags$head(tags$style("#ShapiroTest1{color: #07006B;font-size: 18px;font-style: bold; }" )),
                 h3("Residuals analysis"),
                 plotOutput("pi22")
        ),
        tabPanel("Check homoscedasticity",
                 DT::dataTableOutput("Tab2"),
                 h3(" Fligner-Killeen test of homogeneity of variances"),
                 splitLayout( verticalLayout(textOutput("Fl1"),textOutput("Fl2")
                                             ,tags$head(tags$style("#Fl1{color: #07006B;
                                                                            font-size: 18px;font-style: bold; }" )),
                                             tags$head(tags$style("#Fl2{color: #07006B;
                                                                           font-size: 18px;font-style: bold; }" ))),
                              uiOutput("Fl")),
                 h3("Boxplot of inputs"),
                 h4(textOutput("main2"),align='center'),
                 tags$head(tags$style("#main2{color: red;font-size: 18px;font-style: bold; }" )),
                 
                 plotOutput("pa2"),plotOutput("pa3")
                 ),
        tabPanel("Study inputs",
                 checkboxInput(inputId = 'showtab',label = h4('Transpose table:'),value = FALSE),
                 DT::dataTableOutput("freqTable"),
                 DT::dataTableOutput("table12"),
                 h3("Interactions plot"),
                 plotOutput("pi2"),
                 h3("ANOVA Table: Estimating paramerters"),
                 DT::dataTableOutput("table14"),
                 h3("Tukey's Test"),
                 sliderInput(inputId = "alpha2",label = 'Level of confidence',value = 0.95,max = 0.99,min = 0,step = 0.01),
                 checkboxInput(inputId='para', label='Label orientation',value = TRUE),
                 uiOutput("anova2tuk")
                ),
        tabPanel("Reaction study",
                 htmlOutput("mod"),
                 h4(print("Data sample")),
                 splitLayout(tableOutput(outputId = "table"),plotOutput("normsub")),
                  h4(textOutput("T1")),
                 DT::dataTableOutput("table13"))
        ,tabPanel("Two way ANOVA with replications",verbatimTextOutput("table15"),verbatimTextOutput("table16"))
      )
      
    }
   
    })

  output$tests <- renderUI({tabsetPanel(tabPanel("Tukey",
                                               plotOutput("pe"),
                                               checkboxInput(inputId='par', label='Label orientation',value = TRUE),
                                               helpText("Check for horizontal lables"),
                                               sliderInput(inputId='CL', label='Confidence level', min=0, max=0.99,value=0.95, step=0.01),
                                               DT::dataTableOutput("table2")
                                               
                                                  ),
                                         tabPanel("Bonferroni",
                                               DT::dataTableOutput("table3")
                                                 )
   )})
  output$tbd<-renderUI({
    
    if(input$displayDT){DT::dataTableOutput("TD")}
    
  })
  output$T1<-renderText({paste(input$V1,"~",input$F11,"|",input$F12,"=",input$sub)})
  output$T2<-renderText({
    data<-myData1()
    if (is.null(data)) return()
    paste("text/css","#TD td:nth-child(",match(input$V1, colnames(data)),") {text-align:center;background-color:red;color: white;text-align:center}")
    
  })
  output$anova2tuk<-renderUI({
    tabsetPanel(tabPanel("Factor X1",
                         textOutput("Tuktab1"),
                         tags$head(tags$style("#Tuktab1{color: #07006B;font-size: 18px;font-style: bold; }" )),
                         DT::dataTableOutput("Tuk"),
                         plotOutput("plottuk1")),
                tabPanel("Factor X2",
                         textOutput("Tuktab2"),
                         tags$head(tags$style("#Tuktab2{color: #07006B;font-size: 18px;font-style: bold; }" )),
                         DT::dataTableOutput("Tuke"),
                         plotOutput("plottuk2")))
  })
  
}
# Create a Shiny app object
shinyApp(ui = ui, server = server)