# TODO
# kérdések hover esetén jöjjenek fel vagy táblában lent
# fejléc design 
# kicsik pártok legyen ugyan ott 
# mobile-on is jó legyen, ne ugorjon nagy méreture
# impressum (bk.hu)
# vokskabin link
# legend legyen olvasható

# hol legyen megosztva
# bk.hu-ra is
# angol változat


#dataprep -----
library(visNetwork)
library(data.table)
setwd("E:/Mega Sync/R/elections-2022-hu")
voks <- readxl::read_xlsx('voks.xlsx', sheet = 2) %>% data.table()
q <- voks$q
voks[,q:=NULL]

#links -------------------
  vlinks <- melt(voks, id.vars = c('q_id','topic'))
  
  vlinks2 <- vlinks[vlinks, on = c('q_id','topic'), allow.cartesian = TRUE][
    value == i.value & variable != i.variable][
      order(variable, i.variable, q_id)]
  setnames(vlinks2, c("value","variable","i.variable"),
           c("label", "from_name", "to_name"))
  vlinks2[,from:=as.numeric(as.factor(from_name))]
  vlinks2[,to  :=as.numeric(as.factor(to_name))]
  vlinks2[,i.value := NULL]
  
  vlinks3 <- vlinks2[to>from][order(q_id, from, to)]
  
  vlinks4 <- rbindlist(list(
     vlinks3[,.(value = .N^4/1e6, topic = 'all', width = .N), .(from,to)],
     vlinks3[,.(value = .N^4                   , width = .N), .(from,to,topic)]
     ), use.names = T)

  vlinks4[,topicn := as.numeric(as.factor(topic))]
  vlinks4[,smooth:=T]
  
  vlinks4 <- voks[,.(qn=.N),topic][vlinks4, on = 'topic']
  vlinks4[topic == 'all',qn:=40]
  vlinks4[,width := as.character(width / qn)]
  vlinks4[,value := NULL]
  vlinks4[,agreement := as.numeric(width)]
  vlinks4[,width := agreement^4*40]
  #vlinks4[,value := NULL]
  # vlinks4[,font.color :="blue"] 
  # vlinks4[,font.size  := 40] 

  colors <- data.table(
    topic=vlinks4$topic %>% unique(),
    color=grDevices::adjustcolor(c("grey",MetBrewer::met.brewer('Signac',12)),
                                 alpha.f = .8)
    )
  
  vlinks4 <- colors[vlinks4,on="topic"]
  #vlinks4[,color := network::as.color(topic, 0.5)]

#nodes ---------------------
  vnodes <- rbindlist(list(
    vlinks3[,.(id = from, title = from_name)] ,
    vlinks3[,.(id = to  , title =   to_name)]
  )) %>% unique()
  vnodes_names <- vnodes[,.(id,title)][order(id)]
  vnodes[,title:=NULL]
  vnodes <- vnodes[order(id)]
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

  #vnodes[,shape := 'circie']
  #vnodes[,value := 1]
  vnodes[,mass := 100]
  vnodes[,imagePadding:=0]
  vnodes[,x:=c(300,500,500,300,100,100)]
  vnodes[,y:=c(100,200,400,500,400,200)]
  # vnodes <- rbindlist(list(vnodes, data.table(id = 100:150,
  #                                             title = NA,
  #                                             shape = 'dot',
  #                                             value =10,
  #                                             mass = 1)))

#viz -----
  phys_params <- list(
    theta = .5, #long vs short forces
    gravitationalConstant = -1,#repulsion
    springConstant = .1,
    avoidOverlap = 0,
    damping = 0.4
  )
  
  topics <- vlinks4[,.(topic, topicn)][order(topicn)] %>% unique()
  topics[topicn == 1, topic := "Minden tema"]
  
  nw <- list()
  nw[[1]] <- 
  visNetwork(nodes = vnodes,
             edges = vlinks4[topicn == 1],
             background='black',
             main = list(text = topics[1,topic],
                         style = 'color:white;font-size:20px;text-align:center')
    ) %>%
    visPhysics(solver = "forceAtlas2Based" ,forceAtlas2Based = phys_params) %>%
    #visEdges(shadow = F, shadow = list(color = 'grey')) %>% 
    visLayout(randomSeed = 1) %>% 
    visInteraction(dragView = FALSE,
                   zoomView = FALSE) %>%
    visLegend(addEdges = data.table(
      color = c('#BEBEBECC','#BEBEBECC','#BEBEBECC'), 
      width = c(1,10,20),
      arrows = c('none','none','none'),
      label = c("20%","50%",'100%')
      ))

  ##################### minis  
  
  vnodes[,x:=c(200,100,200,400,500,400)]
  vnodes[,y:=c(100,300,500,500,300,100)]
  
  vlinks4[,hidden := F]
  vlinks4[topicn == 1,hidden:=T]
  #phys_params$springConstant <- phys_params$springConstant*.7

  
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
      #visEdges(shadow = F, shadow = list(color = 'grey')) %>% 
      visLayout(randomSeed = 1) 
  
  }

  
  # vlinks4[,length := 95]
  # vlinks4[topicn == 5, length := 200]
  # 
  # visNetwork(nodes = vnodes,
  #            edges = vlinks4[topicn %in% c(5,6)],
  #            background='black',
  #            main='Választás 2022'
  # ) %>%
  #   visPhysics(solver = "forceAtlas2Based" ,forceAtlas2Based = phys_params) %>%
  #   #visEdges(shadow = F, shadow = list(color = 'grey')) %>% 
  #   visLayout(randomSeed = 1)


  
#-------------------------------------- tmp
# visNetwork(nodes = data.table(id = c(1,2,3,4,5,6),
#                               x  =c(300,500,500,300,100,100),
#                               y  =c(100,200,400,500,400,200),
#                               physics = c(F,F,F,F,F,F)
# ),
# edges = data.table(from = c(1,2,3,4,5,6),
#                    to =   c(2,3,3,4,5,6)
#                    # length = c(5,5), #spring length
#                    # value = c(1000,1000), #width of edges (phys irrelevant)
#                    # hidden = c(F,F), #T=not show but phys relevant
#                    # physics = c(T,T) #T=phys relevant edge
# ),
# )  %>%
#   visPhysics(solver = "forceAtlas2Based"
#              ,forceAtlas2Based = list(
#                theta = .5, #long vs short forces
#                gravitationalConstant = -50,#repulsion
#                springConstant = 0.08
#              )
#   ) %>%
#   visLayout(randomSeed = 14)



# vokst <- readxl::read_xlsx('voks.xlsx', sheet = 3) %>% data.table()
# vokst_names <- colnames(vokst[,3:8])
# vokst <- transpose(vokst[,3:8])
# setnames(vokst, colnames(vokst), paste0('q_',1:40))
# 
# dist_m <- as.matrix(dist(vokst))
# dist_mi <- 1/dist_m # one over, as qgraph takes similarity matrices as input
# library(qgraph)
# qgraph(dist_mi, layout='spring', vsize=3)