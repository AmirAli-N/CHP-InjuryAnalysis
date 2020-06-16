library(data.table)
library(tidyr)
library(dplyr)
library(networkD3)
library(ggplot2)

setwd("G:/My Drive/WorkingDesk/CalTrans-Injury descriptions")

df=fread(file="pair_freq.csv", sep=",", header=TRUE)
unique(df$description)

df$description=gsub("complain\\b", "complaint ", df$description)
df$description=gsub("pain", "complaint", df$description)
df$description=sub("complaint", "complaint_of_pain", df$description)
df$description=gsub("(\\complaint\\b)", "", df$description)

df$description=gsub("fractured", "fracture", df$description)
df$description=gsub("visible", "", df$description)
df$description=gsub("\\s+", " ", df$description)
df$description=gsub("^\\s", "", df$description)
df$description=gsub("soreness", "sore", df$description)
df$description=gsub("fatal", "death", df$description)
df$description=gsub("dead", "death", df$description)
df$description=gsub("deceased", "death", df$description)
df$description=gsub("blood", "bleed", df$description)
df$description=gsub("bleedy", "bleed", df$description)
df$description=gsub("bleeding", "bleed", df$description)
df$description=gsub("swollen", "swell", df$description)
df$description=gsub("lacerate", "laceration", df$description)
df$description=gsub("dizzy", "dizziness", df$description)
df$description=gsub("\\b\\aceration", "laceration", df$description)

df$body_part=gsub("left ", "", df$body_part)
df$body_part=gsub("right ", "", df$body_part)
df$body_part=gsub("low ", "", df$body_part)
df$body_part=gsub("lower ", "", df$body_part)
df$body_part=gsub("upper ", "", df$body_part)

df[df==" "]=NA
df[df==""]=NA

df$body_part=gsub("facial", "face", df$body_part)
df$body_part=gsub("abdoman", "abdomen", df$body_part)
df$body_part=gsub("abdominal", "abdomen", df$body_part)
df$body_part=gsub("spinal", "spine", df$body_part)
df$body_part=gsub("pelvi", "pelvis", df$body_part)

sum_df=data.frame(matrix(NA, nrow=0, ncol=2))
colnames(sum_df)=c('descr', 'part')

for (i in 1:length(df$body_part)){
  parts=unlist(strsplit(df$body_part[i], ' '))
  descr=unlist(strsplit(df$description[i], ' '))
  
  for (description in descr){
    sum_df=rbind(sum_df, cbind('descr_type'=description, 'body_part'=parts))
  }
}

sum_df$descr_type=as.factor(sum_df$descr_type)
sum_df$body_part=as.factor(sum_df$body_part)
sum_df=drop_na(sum_df)

temp=setDT(sum_df)[order(descr_type, body_part), .(count=.N), by=.(descr_type, body_part)]

nodes=cbind(c(as.character(unique(temp$descr_type)), as.character(unique(temp$body_part))))

nodes=cbind.data.frame(1:(length(unique(temp$descr_type))+length(unique(temp$body_part))), 
                       c(as.character(unique(temp$descr_type)), as.character(unique(temp$body_part))))
colnames(nodes)=c("num", "name")

descr.name=as.character(unique(temp$descr_type))
part.name=as.character(unique(temp$body_part))
links=as.data.frame(matrix(NA, nrow = 0, ncol=3))
colnames(links)=c("source","target","value")

for (i in descr.name){
  num_link1=0
  for(j in part.name){
    num_link1=temp$count[which(temp$descr_type==i & temp$body_part==j)]
    if(length(num_link1)>0){
      links=rbind.data.frame(links, cbind.data.frame("source"=which(descr.name==i)-1,
                                                     "target"=which(part.name==j)+length(descr.name)-1,
                                                     "value"=num_link1))
    }
  }
}

p <- sankeyNetwork(Links = links, Nodes = nodes, Source = "source",
                   Target = "target", Value = "value", NodeID = "name",
                   units = "TWh", fontSize = 14, nodeWidth = 70)

p

onRender(
  p,
  '
  function(el,x){
  // select all our node text
  var node_text = d3.select(el)
  .selectAll(".node text")
  //and make them match
  //https://github.com/christophergandrud/networkD3/blob/master/inst/htmlwidgets/sankeyNetwork.js#L180-L181
  .attr("x", 0.5*x.options.nodeWidth)
  .attr("text-anchor", "middle");
  }
  '
)

fwrite(temp, "pair_frequency.csv", sep=",", append=FALSE)

# ggplot(as.data.frame(temp),
#        aes(y = count, axis1 = descr_type, axis2 = body_part)) +
#   geom_alluvium(aes(fill = descr_type), width = 1/12) +
#   geom_stratum(width = 1/12, fill = "black", color = "grey") +
#   geom_label(stat = "stratum", infer.label = TRUE) +
#   #scale_x_discrete(limits = c("Gender", "Dept"), expand = c(.05, .05)) +
#   scale_fill_brewer(type = "qual", palette = "Set1") +
#   ggtitle("UC Berkeley admissions and rejections, by sex and department")

