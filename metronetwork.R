library(rvest)

ab = read_html('http://www.delhimetrotimes.in/metro-stations.html')

ab %>% html_nodes('table') %>% .[[1]] %>% html_table()

ab1=ab %>% html_nodes('table') %>% map_df(~html_table(.)) 


insertRow2 <- function(existingDF, newrow, r) {
  existingDF <- rbind(existingDF,newrow)
  existingDF <- existingDF[order(c(1:(nrow(existingDF)-1),r-0.5)),]
  row.names(existingDF) <- 1:nrow(existingDF)
  return(existingDF)  
}

nu = c(1,"RAJIV CHOWK",3,4)

ab1=insertRow2(ab1,nu,88)

ab1=ab1 %>% mutate(X2=case_when(X1=str_detect(ab1$X1,"S.(.*)No")
                            ~"NA", # S.(followed by one or more any character followed by NA)
                            TRUE~X2))

#str_detect(ab1$X1,"S. No")

#ab1$X2=="NA"

#ab1$X2 =replace_na(ab1$X2=="NA")

#is.na(ab1$X2)

ab1=ab1 %>% mutate(X2=ifelse(X2=="NA",NA,X2)) %>% slice(-1)

ab1$X2

ab2= tibble(station1=ab1$X2) %>% mutate(station2=lag(station1)) %>%  filter(!is.na(station1),!is.na(station2))

library(tidygraph)
library(ggraph)

metro=as_tbl_graph(ab2)

metro

ggraph(metro,layout="kk")+
  geom_edge_link(colour="blue",alpha=0.3)+
  geom_node_point()+
  geom_node_text(aes(label=name),size=2,repel=T)


metro %>% mutate(degree=centrality_betweenness()) %>% arrange(desc(degree)) %>% activate(nodes) %>% as_tibble() %>% View()