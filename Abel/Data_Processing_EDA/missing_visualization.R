######## function of missing patterns

visualize_missings<- function(data,percent=FALSE){

library(tidyverse)
library(patchwork)
library(ggnewscale)

if(percent){
  missing_patterns <- data.frame(is.na(data)) %>%
    group_by_all() %>%
    count(name = "count", sort = TRUE) %>%
    ungroup()%>%
    mutate(count=count/NROW(data)*100)
}else{
  missing_patterns <- data.frame(is.na(data)) %>%
    group_by_all() %>%
    count(name = "count", sort = TRUE) %>%
    ungroup()
}
##############################
# Rows counts for each pattern
##############################

missing_patterns<-missing_patterns%>%
  rownames_to_column(var = "row")%>%
  mutate(row=as.numeric(row))

if(nrow(missing_patterns)>50){
  len<-length(missing_patterns$row)
  lab_breaks<-missing_patterns$row[seq(1,len,length.out=50)]
}else{
  lab_breaks<-missing_patterns$row  
}


alph<-missing_patterns%>%
  select(-c(count))%>%
  group_by(row)%>%
  mutate(sum = rowSums(across(everything())),
         alpha=ifelse(sum==0,1,.5))%>%
  ungroup()%>%
  select(row,alpha)


if(percent){
  p_patterns<-ggplot(missing_patterns)+
    geom_col(aes(x=reorder(row,-row),
                 y=count,
                 fill="a",
                 alpha=as.factor(row)
    ))+
    scale_x_discrete(labels = lab_breaks,breaks=lab_breaks)+
    scale_fill_manual(values="darkblue")+
    scale_alpha_manual(values = alph$alpha)+
    theme(legend.position = "None")+
    ylab("% of rows")+ xlab(NULL)+
    coord_flip()
}else{
  p_patterns<-ggplot(missing_patterns)+
    geom_col(aes(x=reorder(row,-row),
                 y=count,
                 fill="a",
                 alpha=as.factor(row)
    ))+
    scale_x_discrete(labels = lab_breaks,breaks=lab_breaks)+
    scale_fill_manual(values="darkblue")+
    scale_alpha_manual(values = alph$alpha)+
    theme(legend.position = "None")+
    ylab("Row count")+ xlab(NULL)+
    coord_flip()
}
##################
# Num rows missing plot
##################

if(percent){
  num_rows<-data.frame(is.na(data))%>%
    apply(.,2,sum)
  num_rows<-num_rows/NROW(data)*100
}else{
  num_rows<-data.frame(is.na(data))%>%
    apply(.,2,sum)
}
num_rows<-data.frame(num_rows)%>%
  arrange(desc(num_rows))

num_rows<-num_rows%>%
  rownames_to_column( var = "var")

if(percent){
  p_rows<-num_rows%>%
    ggplot()+
    geom_col(aes(x=reorder(var,desc(num_rows)),
                 y=num_rows,
                 fill="a",
                 alpha=0.5))+
    scale_fill_manual(values="darkblue")+
    theme(legend.position = "None")+
    xlab(NULL)+ ylab("% of rows missing")+
    ggtitle("Missing values pattern")
  
  if(all(num_rows$num_rows==0)){
    # adjust scale when there is no NAs
    p_rows<-p_rows+
      ylim(0,100)
    
  }
  
}else{
  p_rows<-num_rows%>%
    ggplot()+
    geom_col(aes(x=reorder(var,desc(num_rows)),
                 y=num_rows,
                 fill="a",
                 alpha=0.5))+
    scale_fill_manual(values="darkblue")+
    theme(legend.position = "None")+
    xlab(NULL)+ ylab("# of rows missing")+
    ggtitle("Missing values pattern")
  
  
}
####################
# graph of patterns
####################

patterns<-missing_patterns%>%
  select(-row)%>%
  rownames_to_column( var = "Miss_pat")%>%
  pivot_longer(!c(Miss_pat,count),names_to = "var")%>%
  mutate(Miss_pat=as.numeric(Miss_pat))


labels<-patterns%>%
  group_by(Miss_pat)%>%
  nest()%>%
  mutate(no_na=map(data,.f = function(x){all(x[,"value"]==FALSE)}))%>%
  unnest(no_na)%>%
  filter(no_na==T)%>%
  unnest(data)

lab_col=c("FALSE"="gray80","TRUE"="mediumorchid1")
lab_complete=c("FALSE"="darkgray")

# case when there is no complete values in the dataset
if(nrow(labels)==0){
  p_heat<-patterns%>%
    left_join(num_rows,by="var")%>%
    ggplot()+
    geom_tile(aes(x=reorder(var,desc(num_rows)),
                  y=reorder(Miss_pat,-Miss_pat),fill=value),color="white")+
    scale_y_discrete(labels = lab_breaks,breaks=lab_breaks)+
    scale_fill_manual(values=lab_col)+
    theme(legend.position = "None")+
    xlab("Variable")+ ylab("Missing pattern")
}else{
p_heat<-patterns%>%
  left_join(num_rows,by="var")%>%
  ggplot()+
  geom_tile(aes(x=reorder(var,desc(num_rows)),
                y=reorder(Miss_pat,-Miss_pat),fill=value),color="white")+
  scale_y_discrete(labels = lab_breaks,breaks=lab_breaks)+
  scale_fill_manual(values=lab_col)+
  new_scale_fill()+
  geom_tile(data = labels,
            aes(x=var,
                y=reorder(Miss_pat,-Miss_pat),fill=value),
            color="white")+
  scale_fill_manual(values=lab_complete)+
  annotate("text",
           x=ceiling(length(labels$var)/2),
           y=reorder(labels$Miss_pat,-labels$Miss_pat),
           label="Complete cases")+
  theme(legend.position = "None")+
  xlab("Variable")+ ylab("Missing pattern")
}

layout<-c(
  area(t = 1,l = 1,b = 1,r = 4),
  area(t = 2,l = 5,b=4,r=5),
  area(t = 2,l = 1,b = 4,r = 4)
)


res<-p_rows+p_patterns+p_heat+plot_layout(design = layout)
return(res)
}


# Ejercicios para probar la funcion 
if(F){ 
#rm(list=ls())
set.seed(5702)
mycars <- mtcars
mycars[1:25, "gear"] <- NA
mycars[10:20, 3:5] <- NA
for (i in 1:10) mycars[sample(32,1), sample(11,1)] <- NA

visualize_missings(mycars,percent = F)
visualize_missings(openintro::birds,T) #review: plots raros
visualize_missings(openintro::ucla_textbooks_f18,T)

visualize_missings(fivethirtyeight::avengers,T)
visualize_missings(fivethirtyeight::bachelorette) 
visualize_missings(fivethirtyeight::dem_candidates) 
visualize_missings(fivethirtyeight::steak_survey,F) # esta chulo
visualize_missings(fivethirtyeight::trumpworld_polls,F)

visualize_missings(ggplot2::economics,F)
visualize_missings(Lock5withR::HollywoodMovies2011,F)

# evaluate the option to rotate the labels

}
