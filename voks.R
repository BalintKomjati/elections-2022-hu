# TODO
# bk.hu add
# eng version
# readme


#dataprep -----------

  library(visNetwork)
  library(data.table)
  library(DT)
  setwd("E:/Mega Sync/R/elections-2022-hu")
  voks <- readxl::read_xlsx('voks.xlsx', sheet = 2) %>% data.table()
  voksDT <- copy(voks) #this is for the datatable on the viz
  voksDT[,q_id:=NULL]
  voks[,3] <- NULL #remove long questions
  setnames(voks, colnames(voks)[2], 'topic')

#links ---------------
  
  #melt and self join to create from-to pairs
  vlinks <- melt(voks, id.vars = c('q_id','topic'))

  vlinks2 <- vlinks[vlinks, on = c('q_id','topic'), allow.cartesian = TRUE][
    value == i.value & variable != i.variable][
      order(variable, i.variable, q_id)]
  setnames(vlinks2, c("value", "variable",  "i.variable"),
                    c("label", "from_name", "to_name"))
  vlinks2[,from:=as.numeric(as.factor(from_name))]
  vlinks2[,to  :=as.numeric(as.factor(to_name))]
  vlinks2[,i.value := NULL]
  
  #remove duplicates (keep from->to, remove to->from)
  vlinks3 <- vlinks2[to>from][order(q_id, from, to)]
  
  #aggregate links to topic level + add records for 'all topics'
  vlinks4 <- rbindlist(list(
     vlinks3[,.(topic = 'all', width = .N), .(from,to)],
     vlinks3[,.(               width = .N), .(from,to,topic)]
     ), use.names = T)

  vlinks4[,topicn := as.numeric(as.factor(topic))]
  vlinks4[,smooth:=T]
  
  #calculate the percentage parties agree + edge width in line with that
  vlinks4 <- voks[,.(qn=.N),topic][vlinks4, on = 'topic'] 
  vlinks4[topic == 'all',qn:=40] #qn = number of questions
  vlinks4[,width := as.character(width / qn)] #width of the edges
  vlinks4[,agreement := as.numeric(width)] 
  vlinks4[,width := agreement^4*40] #over-emphasizing large values
  
  #define individual color for each topic
  colors <- data.table(
    topic=vlinks4$topic %>% unique(),
    color=grDevices::adjustcolor(c("grey",MetBrewer::met.brewer('Signac',12)),alpha.f = .8)
    )
  
  vlinks4 <- colors[vlinks4,on="topic"]

#nodes ---------------------
  
  #extract individual nodes from links dataset
  vnodes <- rbindlist(list(
    vlinks3[,.(id = from, title = from_name)] ,
    vlinks3[,.(id = to  , title =   to_name)]
  )) %>% unique()
  vnodes <- vnodes[order(id)]
  vnodes[,title:=NULL]#no need to show titles up when hovering on the node

  #add node icons and additional parameters
  imgs <- paste0('img/',1:6,'.png')
  txt<-1
  for (i in 1:6) {
    txt[i] <- RCurl::base64Encode(
      readBin(imgs[i],'raw',
              file.info(imgs[i])[1, 'size']),'txt')
  }
  vnodes[,tx:=txt]
  vnodes[,image:=paste('data:image/png;base64', tx, sep = ',')]
  vnodes[,tx:=NULL]
  vnodes[,color:= 'black']
  vnodes[,shape:="circularImage"]
  vnodes[,mass := 100]
  vnodes[,imagePadding:=0]
  
  #preset node positions 
  vnodes[,x:=c(300,500,500,300,100,100)]
  vnodes[,y:=c(100,200,400,500,400,200)]


#viz ------------------
  
  phys_params <- list(
    theta = .5, #long vs short forces
    gravitationalConstant = -1,#repulsion
    springConstant = .1,
    avoidOverlap = 0,
    damping = 0.4
  )
  
  #extract topics to loop over them
  topics <- vlinks4[,.(topic, topicn)][order(topicn)] %>% unique()
  topics[topicn == 1, topic := "Minden tema"]
  
  #create the main network
  nw <- list()
  nw[[1]] <- 
  visNetwork(nodes = vnodes,
             edges = vlinks4[topicn == 1],
             background='black',
             main = list(text = topics[1,topic],
                         style = 'color:white;font-size:20px;text-align:center')
    ) %>%
    visPhysics(solver = "forceAtlas2Based" ,forceAtlas2Based = phys_params) %>%
    visLayout(randomSeed = 1) %>% 
    visInteraction(dragView = FALSE,
                   zoomView = FALSE) %>%
    visLegend(addEdges = data.table(
      color = c('white','white','white'), 
      width = c(1,10,20),
      arrows = c('none','none','none'),
      label = c("20%","50%",'100%')
      ), zoom = FALSE)

  #create the sub networks
  
  vnodes[,x:=c(200,400,400,200,500,100)-70]
  vnodes[,y:=c(100,100,500,500,300,300)]
  
  for (i in 2:13) {
    nw[[i]] <- 
  
      visNetwork(nodes = vnodes,
                 edges = vlinks4[topicn %in% c(i)],
                 background='black',
                 main = list(text = topics[i,topic],
                             style = 'color:white;font-size:15px;text-align:center')
      ) %>%
      visPhysics(enabled = FALSE) %>%
      visInteraction(dragView = FALSE,
                     zoomView = FALSE) %>%
      visLayout(randomSeed = 1) 
  
  }

