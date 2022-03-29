i<-2

vlinks5 <- vlinks4[topicn %in% c(1,i)]
vlinks5[,physics := hidden]
vlinks5[,hidden:=NULL]
vlinks5[,value := NULL]
vlinks5[,agreement := as.numeric(width)]
vlinks5[,width := agreement^4*40]


visNetwork(nodes = vnodes,
           edges = vlinks5[topicn == 1],
           background='black',
           main = list(text = topics[i,topic],
                       style = 'color:white;font-size:15px;text-align:center')
) %>%
  visPhysics(enabled = FALSE) %>%
  # visInteraction(dragNodes = FALSE, 
  #                dragView = FALSE, 
  #                zoomView = FALSE) %>%
  #visEdges(scaling) %>% 
  visLayout(randomSeed = 1) 

