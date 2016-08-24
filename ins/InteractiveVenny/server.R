# Must be executed BEFORE rgl is loaded on headless devices.
options(rgl.useNULL=TRUE)

library(shiny)
library(VennDiagram)
library(shinysky)
# library(Cairo)


#x denote dataframe,y denote samplenames
data2list<-function(x){
  #list apped
  lappend <- function(lst, obj) {
    lst[[length(lst)+1]] <- obj
    return(lst)
  }
  ## max list is five
  if(ncol(x)>5)
  {
    return(NULL)
  }else{
    LL <- list()
    for(i in 1:ncol(x)){
      tempdata<-x[,i]
       tempdata<-tempdata[!is.na(tempdata)]
       LL<-lappend(LL,tempdata)
    
    }
    return(LL)
  }
  
}


options(shiny.maxRequestSize=100*1024^2) # max file size is 100Mb
shinyServer(function(input,output,session){
   
    
    datasetInput <- reactive({ 
                                
          #example<-read.table("E:\\Program Files\\R\\R-3.1.0\\library\\shiny\\examples\\Venn\\test.txt",header=F,sep="\t")
          
      example<-read.table("data/test.txt",header=F,sep="\t")
           	inFile <- input$file1
            if (!is.null(inFile)){
                 
                data<-read.table(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote) 		
             
            }
            
           switch(input$dataset,
                    "example" = example,
                    "upload" = data
                    )	
    	})
    
    textinput<-reactive({
      L<-list()
      if (input$samplelist1!=""){
        a<-input$samplelist1
        a<-strsplit(a,"\n")
        L<-append(L,a)
      }
      if (input$samplelist2!=""){
        b<-input$samplelist2
        b<-strsplit(b,"\n")
        L<-append(L,b)
        
      }
      if (input$samplelist3!=""&&input$listnumber>=3){
        c<-input$samplelist3
        c<-strsplit(c,"\n")
        L<-append(L,c)
      }
      if (input$samplelist4!=""&&input$listnumber>=4){
        d<-input$samplelist4
        d<-strsplit(d,"\n")
        L<-append(L,d)
      }
      if (input$samplelist5!=""&&input$listnumber==5){
        e<-input$samplelist5
        e<-strsplit(e,"\n")
        L<-append(L,e)
      }
      
      return(L)
    })
    
    #input name
    textinputnamelist<-reactive({
      namelist<-c()
      if (input$Samplename1!=""){
        a<-input$Samplename1
        namelist<-c(namelist,a)
      }
      if (input$Samplename2!=""){
        b<-input$Samplename2
        namelist<-c(namelist,b)
      }
      if (input$Samplename3!=""&&input$listnumber>=3){
        c<-input$Samplename3
        namelist<-c(namelist,c)
      }
      if (input$Samplename4!=""&&input$listnumber>=4){
        d<-input$Samplename4
        namelist<-c(namelist,d)
      }
      if (input$Samplename5!=""&&input$listnumber>=5){
        e<-input$Samplename5
        namelist<-c(namelist,e)
      }
      
      return(namelist)
    })
    
    #input colorlist
    textinputcolorlist<-reactive({
      namelist<-c()
      if (input$jscolorInput1!=""){
        a<-input$jscolorInput1
        namelist<-c(namelist,paste("#",a,sep=""))
      }else{
        namelist<-c(namelist,paste("#FFFFFF"))
      }
      if (input$jscolorInput2!=""){
        b<-input$jscolorInput2
        namelist<-c(namelist,paste("#",b,sep=""))
      }else{
        namelist<-c(namelist,paste("#FFFFFF"))
      }
      if (input$listnumber>=3){
        if(input$jscolorInput3==""){
          namelist<-c(namelist,paste("#FFFFFF"))
        }else{
          c<-input$jscolorInput3
          namelist<-c(namelist,paste("#",c,sep=""))
        }
      } 
      if (input$listnumber>=4){
        if(input$jscolorInput4==""){
          namelist<-c(namelist,paste("#FFFFFF"))
        }else{
          c<-input$jscolorInput4
          namelist<-c(namelist,paste("#",c,sep=""))
        }
      }
      if (input$listnumber>=5){
        if(input$jscolorInput5==""){
          namelist<-c(namelist,paste("#FFFFFF"))
        }else{
          c<-input$jscolorInput5
          namelist<-c(namelist,paste("#",c,sep=""))
        }
      }
      
      return(namelist)
    })
    
    fillcolorInput<-reactive({
      x<-input$select2Input1
      a<-datasetInput()
      if (length(x)>ncol(a)){
        x<-x[1:ncol(a)]
      }
      x
    })
    
    
    textcolorInput<-reactive({
      x<-input$select2Input2
      x
    })
    
    
     samplenameInput<-reactive({
       x<-input$samplenametxt
       a<-datasetInput()
      x<-do.call(c,strsplit(x, ","))
      if(input$header==TRUE){
        x<-colnames(a)
      }else if (length(x)>ncol(a)){
        x<-x[1:ncol(a)]
      }
      
       x
     })
    
# output upload dataset or example dataset  
    output$summary <- renderTable({
      input$goButton3
      a=NULL
      isolate({
          a<-datasetInput()
      
          
      })
      a
    },include.rownames=FALSE)
 #output in analysis Summary   
    output$text1<-renderPrint({
      
      a<-datasetInput()
      if(!input$dataset=='Inputlist'){
        mylist<-data2list(a)
      }else{
        mylist<-textinput()
      }
      
      #samplenames <-c("Sample1","Sample2","Sample3")
      isolate({
        
        if(input$dataset=='Inputlist'){
          samplenames<-textinputnamelist()
        }else{
          totalname <- samplenameInput()
          samplenames <- c()
          x<-length(totalname)
          if(x>=length(mylist)){
            samplenames <- totalname
          }else{
            for(i in length(mylist)){
              samplenames<-c(samplenames,totalnames[i%%x+1])
            }
          }
        }       
      })
      
      #change list name to sample name that user input
      taglist<-c("List1","List2","List3","List4","List5")
      outputtext<-""
      selectname=input$select2InputDynamic
      for(i in 1:length(selectname)){
        outputtext<-paste(outputtext,samplenames[which(taglist==selectname[i])],sep=",")
      }
      l<-getoverlap(length(input$select2InputDynamic))
      cat('The total length of overlap list in "',outputtext,'\" is \"',nrow(l),'"',sep="")
    })
    
    
    
#plot picture
    output$VenPlot <- renderPlot({
    
        input$goButton
        input$goButton2
        input$goButton3
        a<-datasetInput()
        if(!input$dataset=='Inputlist'){
          mylist<-data2list(a)
        }else{
          mylist<-textinput()
        }
        
         #samplenames <-c("Sample1","Sample2","Sample3")
        isolate({

          if(input$dataset=='Inputlist'){
            samplenames<-textinputnamelist()
            
            fillcolorlist<-textinputcolorlist()
          }else{
                   totalname <- samplenameInput()
                   totalfillcolorlist<- fillcolorInput()

                    samplenames <- c()
                    fillcolorlist<-c()
                    
                   
                   #samplenames
                     x<-length(totalname)
                     if(x>=length(mylist)){
                       samplenames <- totalname
                     }else{
                       for(i in length(mylist)){
                       samplenames<-c(samplenames,totalnames[i%%x+1])
                      }
                     }
                   
                   #fillcolorlist
                   x<-length(totalfillcolorlist)
                   if(x>=length(mylist)){
                     fillcolorlist <- totalfillcolorlist
                   }else{
                     for(i in length(mylist)){
                       fillcolorlist<-c(fillcolorlist,totalfillcolorlist[i%%x+1])
                     }
                   }
          }       
                   
                   #txtcolorlist
                   totaltxtcolorlist <- textcolorInput()
                   txtcolorlist<-c()
                   
                   x<-length(totaltxtcolorlist)
                   if(x>=length(mylist)){
                     txtcolorlist <- totaltxtcolorlist[1:length(mylist)]
                   }else{
                     for(i in length(mylist)){
                       txtcolorlist<-c(txtcolorlist,totaltxtcolorlist[i%%x+1])
                     }
                   }
                   
                   #samplenames <- totalname
                   #fillcolorlist <- totalfillcolorlist
                   #txtcolorlist <- totaltxtcolorlist
                   #for(i in 1:ncol(a)){
                    #     samplenames <- c(samplenames,totalname[i])
                    #     fillcolorlist <- c(fillcolorlist,totalfillcolorlist[i])
                    #     txtcolorlist <- c(txtcolorlist,totaltxtcolorlist[i])
                   #} 
                   
        })
        
        
        
        if(length(mylist)==2){
          plot <- venn.diagram(
            x = list(
              A = mylist[[1]],
              B = mylist[[2]]
            ),
            filename = NULL,
            col = input$bordercolinput,
            category.names = samplenames,
            fill = fillcolorlist,
            alpha = 0.50,
            cat.col = txtcolorlist,
            cat.cex = 1.5,
            cex=input$fontsize,
            cat.dist=input$namedist,
            cat.fontface = "bold",
            margin = 0.05
          )
        }else if(length(mylist)==3){
          plot <- venn.diagram(
            x = list(
              A = mylist[[1]],
              B = mylist[[2]],
              C = mylist[[3]]
            ),
            filename = NULL,
            col = input$bordercolinput,
            category.names = samplenames,
            fill = fillcolorlist,
            alpha = 0.50,
            cat.col = txtcolorlist,
            cat.cex = 1.5,
            cex=input$fontsize,
            cat.dist=input$namedist,
            cat.fontface = "bold",
            margin = 0.05
          )
        }else if(length(mylist)==4){
          plot <- venn.diagram(
            x = list(
              A = mylist[[1]],
              B = mylist[[2]],
              C = mylist[[3]],
              D = mylist[[4]]
            ),
            filename = NULL,
            col = input$bordercolinput,
            category.names = samplenames,
            fill = fillcolorlist,
            alpha = 0.50,
            cat.col = txtcolorlist,
            cat.cex = 1.5,
            cex=input$fontsize,
            cat.dist=input$namedist,
            cat.fontface = "bold",
            margin = 0.05
          )
        }else if(length(mylist)==5){
          plot <- venn.diagram(
            x = list(
              A = mylist[[1]],
              B = mylist[[2]],
              C = mylist[[3]],
              D = mylist[[4]],
              E = mylist[[5]]
            ),
            filename = NULL,
            col = input$bordercolinput,
            category.names = samplenames,
            fill = fillcolorlist,
            alpha = 0.50,
            cat.col = txtcolorlist,
            cat.cex = 1.5,
            cat.dist=input$namedist,
            cex=input$fontsize,
            cat.fontface = "bold",
            margin = 0.05
          )
        }
        
        
        grid.draw(plot)
        
        
    },width=800,height=800) 
    
    #plotfunction
    plotdata<-function(){
      input$goButton
      input$goButton2
      input$goButton3
      a<-datasetInput()
      if(!input$dataset=='Inputlist'){
        mylist<-data2list(a)
      }else{
        mylist<-textinput()
      }
      
      #samplenames <-c("Sample1","Sample2","Sample3")
      isolate({
        
        if(input$dataset=='Inputlist'){
          samplenames<-textinputnamelist()
          fillcolorlist<-textinputcolorlist()
        }else{
          totalname <- samplenameInput()
          totalfillcolorlist<- fillcolorInput()
          
          samplenames <- c()
          fillcolorlist<-c()
          
          
          #samplenames
          x<-length(totalname)
          if(x>=length(mylist)){
            samplenames <- totalname
          }else{
            for(i in length(mylist)){
              samplenames<-c(samplenames,totalnames[i%%x+1])
            }
          }
          
          #fillcolorlist
          x<-length(totalfillcolorlist)
          if(x>=length(mylist)){
            fillcolorlist <- totalfillcolorlist
          }else{
            for(i in length(mylist)){
              fillcolorlist<-c(fillcolorlist,totalfillcolorlist[i%%x+1])
            }
          }
        }       
        
        #txtcolorlist
        totaltxtcolorlist <- textcolorInput()
        txtcolorlist<-c()
        
        x<-length(totaltxtcolorlist)
        if(x>=length(mylist)){
          txtcolorlist <- totaltxtcolorlist[1:length(mylist)]
        }else{
          for(i in length(mylist)){
            txtcolorlist<-c(txtcolorlist,totaltxtcolorlist[i%%x+1])
          }
        }
        
        #samplenames <- totalname
        #fillcolorlist <- totalfillcolorlist
        #txtcolorlist <- totaltxtcolorlist
        #for(i in 1:ncol(a)){
        #     samplenames <- c(samplenames,totalname[i])
        #     fillcolorlist <- c(fillcolorlist,totalfillcolorlist[i])
        #     txtcolorlist <- c(txtcolorlist,totaltxtcolorlist[i])
        #} 
        
      })
      
      
      if(length(mylist)==2){
        plot <- venn.diagram(
          x = list(
            A = mylist[[1]],
            B = mylist[[2]]
          ),
          filename = NULL,
          col = input$bordercolinput,
          category.names = samplenames,
          fill = fillcolorlist,
          alpha = 0.50,
          cat.col = txtcolorlist,
          cat.cex = 1.5,
          cex=input$fontsize,
          cat.dist=input$namedist,
          cat.fontface = "bold",
          margin = 0.05
        )
      }else if(length(mylist)==3){
        plot <- venn.diagram(
          x = list(
            A = mylist[[1]],
            B = mylist[[2]],
            C = mylist[[3]]
          ),
          filename = NULL,
          col = input$bordercolinput,
          category.names = samplenames,
          fill = fillcolorlist,
          alpha = 0.50,
          cat.col = txtcolorlist,
          cat.cex = 1.5,
          cex=input$fontsize,
          cat.dist=input$namedist,
          cat.fontface = "bold",
          margin = 0.05
        )
      }else if(length(mylist)==4){
        plot <- venn.diagram(
          x = list(
            A = mylist[[1]],
            B = mylist[[2]],
            C = mylist[[3]],
            D = mylist[[4]]
          ),
          filename = NULL,
          col = input$bordercolinput,
          category.names = samplenames,
          fill = fillcolorlist,
          alpha = 0.50,
          cat.col = txtcolorlist,
          cat.cex = 1.5,
          cex=input$fontsize,
          cat.dist=input$namedist,
          cat.fontface = "bold",
          margin = 0.05
        )
      }else if(length(mylist)==5){
        plot <- venn.diagram(
          x = list(
            A = mylist[[1]],
            B = mylist[[2]],
            C = mylist[[3]],
            D = mylist[[4]],
            E = mylist[[5]]
          ),
          filename = NULL,
          col = input$bordercolinput,
          category.names = samplenames,
          fill = fillcolorlist,
          alpha = 0.50,
          cat.col = txtcolorlist,
          cat.cex = 1.5,
          cat.dist=input$namedist,
          cex=input$fontsize,
          cat.fontface = "bold",
          margin = 0.05
        )
      }
        
      return(plot)
      
    }
    
  
#get overlap function
    getoverlap<-function(length){
        
        input$goButton
        input$goButton2
        input$goButton3
        a<-datasetInput()
        if(!input$dataset=='Inputlist'){
          mylist<-data2list(a)
        }else{
          mylist<-textinput()
        }
        
        #samplenames <-c("Sample1","Sample2","Sample3")
        isolate({
          
          if(input$dataset=='Inputlist'){
            samplenames<-textinputnamelist()
            fillcolorlist<-textinputcolorlist()
          }else{
            totalname <- samplenameInput()
            totalfillcolorlist<- fillcolorInput()
            
            samplenames <- c()
            fillcolorlist<-c()
            
            
            #samplenames
            x<-length(totalname)
            if(x>=length(mylist)){
              samplenames <- totalname
            }else{
              for(i in length(mylist)){
                samplenames<-c(samplenames,totalnames[i%%x+1])
              }
            }
            
          } 
        })
          
        taglist<-c("List1","List2","List3","List4","List5")
          selectname=input$select2InputDynamic
          x1<-mylist[[which(taglist==selectname[1])]]
          x2<-mylist[[which(taglist==selectname[2])]]
          overlap<-intersect(x1,x2)
        if(length>=3){
          x3<-mylist[[which(taglist==selectname[3])]]
          overlap<-intersect(overlap,x3)
        }
        if(length>=4){
          x4<-mylist[[which(taglist==selectname[4])]]
          overlap<-intersect(overlap,x4)
        }
        if(length==5){
          x5<-mylist[[which(taglist==selectname[5])]]
          overlap<-intersect(overlap,x5)
        }     
      
        return(data.frame(overlap))
      }


#render overlap
    output$overlapout<-renderTable({
      getoverlap(length(input$select2InputDynamic))
    },include.rownames=FALSE)

#render listnumber
    output$listnumber<-renderText({
      
      length(input$select2InputDynamic)
    })
    
#download plot option    
    output$downloadDataPNG <- downloadHandler(
      filename = function() {
        paste("output", Sys.time(), '.png', sep='')
      },
      
      content = function(file) {
        #Cairo(file=file, width = 600, height = 600,type = "png", units = "px", pointsize = 12, bg = "white", res = NA)
        png(file=file, res = 300)
        grid.draw(plotdata())
        dev.off()
      },
      contentType = 'image/png'
    )
    
    
    output$downloadDataPDF <- downloadHandler(
      filename = function() {
        paste("output", Sys.time(), '.pdf', sep='')
      },
      
      content = function(file) {
        pdf(file)
        grid.draw(plotdata())
        dev.off()
      },
      contentType = 'image/pdf'
    )
    
    output$downloadDataEPS <- downloadHandler(
      filename = function() {
        paste("output", Sys.time(), '.eps', sep='')
      },
      
      content = function(file) {
        postscript(file,width=32,height=48,paper = "special")
        grid.draw(plotdata())
        dev.off()
      },
      contentType = 'image/eps'
    )


#download data option
    output$downloadDataCSV <- downloadHandler(
      filename = function() {
        paste("output", Sys.time(), '.csv', sep='')
      },
      
      content = function(file) {
       write.csv(getoverlap(length(input$select2InputDynamic)),file,row.names = FALSE)
      }
    )
    
    output$downloadDataTXT <- downloadHandler(
      filename = function() {
        paste("output", Sys.time(), '.txt', sep='')
      },
      
      content = function(file) {
        write.table(getoverlap(length(input$select2InputDynamic)),file,row.names = FALSE)
      }
    )


output$analysisUI <- renderUI({
  a<-datasetInput()
  if(!input$dataset=='Inputlist'){
    mylist<-data2list(a)
  }else{
    mylist<-textinput()
  }
  
  
  str<-as.character(length(mylist))
  # Depending on input$input_type, we'll generate a different
  # UI component and send it to the client.
  switch(str,
         "2" = select2Input("select2InputDynamic",h4("List Selection"),
                            choices=c("List1","List2"),
                            selected=c("List1","List2")),
         "3" = select2Input("select2InputDynamic",h4("List Selection"),
                            choices=c("List1","List2","List3"),
                            selected=c("List1","List2","List3")),
         "4" =select2Input("select2InputDynamic",h4("List Selection"),
                             choices=c("List1","List2","List3","List4"),
                             selected=c("List1","List2","List3","List4")),
         "5" = select2Input("select2InputDynamic",h4("List Selection"),
                            choices=c("List1","List2","List3","List4","List5"),
                            selected=c("List1","List2","List3","List4","List5"))
  )
})

#shiny alert
    observe({
      showshinyalert(session, "shinyalert4", paste(input$select2Input1, collapse = ","), "info")
    })
    
    observe({
      showshinyalert(session, "shinyalerttext", paste("Group names  must equal to your clusters"), "danger")
    })
    
    observe({
            showshinyalert(session, "shinyalert3", sprintf("Only 2 to 5 Clusters allowed in Loading Data"))
        })
    observe({
      showshinyalert(session, "shinyalert5", paste(input$select2Input2, collapse = ","), "info")
    })
    observe({
      showshinyalert(session, "shinyalert6", sprintf("please input your ID list single per line"), "info")
    })
    observe({
      showshinyalert(session, "shinyalert7", sprintf("Please select at least two List "), "danger")
    })

})



