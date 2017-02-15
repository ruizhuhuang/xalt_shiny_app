# install.packages("arulesViz")
# install.packages("RColorBrewer")
# install.packages("data.table")
# http://web.corral.tacc.utexas.edu/XALT/

timeRange = c("2014-07", "2014-08", "2014-09", "2014-10", "2014-11", "2014-12",
              "2015-01", "2015-02", "2015-03", "2015-04", "2015-05", "2015-06")
# c("_corrupt_record","allocation","build_date","build_user","date","exec_path",
#   "field_of_science","host","job_id","linkA","link_program","module_name",
#   "num_cores","num_nodes","num_threads","run_time","start_time","user")
column_names = c("_corrupt_record","allocation","build_date","build_user","date","exec_path",
                   "field_of_science","host","job_id","linkA","link_program","module_name",
                   "num_cores","num_nodes","num_threads","run_time","start_time","user")
tmp_dir = sprintf("%s/xalt_tmp", Sys.getenv("HOME"));  
out_dir = sprintf("%s/xalt_out", Sys.getenv("HOME"));
create_dir_cmd=sprintf("rm -rf %s;mkdir %s;chmod -R a+rx %s",out_dir,out_dir,out_dir)
system(create_dir_cmd)  # must be absolute path
pmml_path = sprintf("%s/rules.pmml.xml", out_dir)
dat=list()

get_file_names=function(time1,time2){
  s=which(timeRange==time1)
  e=which(timeRange==time2)
  selected = timeRange[s:e]
  file_names=paste("xalt-",selected,".json",sep="",collapse=",")
  return (file_names)
}

create_symbolic_link_for_sel_in_a_tmp_directory=function(tmp_dir,file_names){
  create_dir_cmd=sprintf("rm -rf %s;mkdir %s;chmod a+rx %s",tmp_dir,tmp_dir,tmp_dir)
  system(create_dir_cmd)
  num_files=length(unlist(strsplit(file_names,split = ",")))
  if(num_files==1){
    ln_cmd=sprintf("ln -s /data/00791/xwj/xalt/data/%s %s",file_names,tmp_dir)
  }else{
    ln_cmd=sprintf("ln -s /data/00791/xwj/xalt/data/{%s} %s",file_names,tmp_dir)
  }
  system(ln_cmd)
}


runXaltJava = function(StartMonth, EndMonth,conf, sup,output,
                       column_dis_b ,column_dis_v,
                       association_b,association_v,
                       top_m,rules,plot_type){
  files_sel=get_file_names(StartMonth, EndMonth)
  create_symbolic_link_for_sel_in_a_tmp_directory(tmp_dir,files_sel)
  b = which (column_names==column_dis_b)-1
  v = (which (column_names==column_dis_v)-1)
  as_b= which (column_names==association_b)-1
  t= as.character(as.integer(association_v))
  as_v_b = paste(as.character(append(t,as.character(as_b))), collapse = ",")
  print(as_v_b)
  
  submit_cmd_1 = sprintf("spark-submit --class xalt.xalt --master yarn-client --num-executors 16  --executor-cores 3 /data/00791/xwj/xalt/xalt2.jar -b %s -v %s,%s -f file:%s/xalt_tmp/ -out %s/xalt_out -d ",
                       b,v,b,
                       Sys.getenv("HOME"), Sys.getenv("HOME"))

  
#  --executor-cores 1
  submit_cmd_2 = sprintf("spark-submit --class xalt.xalt --master yarn-client --num-executors 16  --executor-cores 3 /data/00791/xwj/xalt/xalt2.jar -a -b %s -v %s -conf %s -sup %s -f file:%s/xalt_tmp/ -out %s/xalt_out ",
                         as_b, as_v_b,
                         conf, sup, Sys.getenv("HOME"), Sys.getenv("HOME"))

  time0=proc.time()
  system(submit_cmd_1)
  #print(proc.time()-time0)
  time1=proc.time()-time0
  
  time0=proc.time()
  system(submit_cmd_2)
  #print(proc.time()-time0)
  time2=proc.time()-time0
  print(time1)
  print(time2)
  #system(sprintf("rm -rf %s/xalt_tmp/", Sys.getenv("HOME")))

}

pmml_data=function(pmml_path){
  library("arulesViz")
  fromdisk <- read.PMML(pmml_path) 
}

association_plot_shading= function(rules){
  fromdisk=pmml_data(pmml_path)
  #use ? igraph::layout_ to see layout options with igraph.
  par(mar = c(0, 0, 0, 0) , lwd=1, cex=1)
  plot(head(sort(fromdisk, by="lift"), rules), method="graph", measure = "confidence", shading = "lift", 
       control=list(cex=1, arrowSize=0.6, layout=igraph::nicely(),alpha=0.9))
}
association_plot_itemset= function(rules){
  fromdisk=pmml_data(pmml_path)
  #use ? igraph::layout_ to see layout options with igraph. 
  par(mar = c(0, 0, 0, 0) , lwd=1, cex=1)
  plot(head(sort(fromdisk, by="lift"), rules), method="graph" , 
       control=list(cex=1, arrowSize=0.6,type="itemsets",layout=igraph::nicely(),alpha=0.9))
}

##########################################################################################

bar_plot = function(top_n_apps,column_dis_b, column_dis_v){
  
  data_path = sprintf("%s/distribution.txt", out_dir)
  data = read.table( data_path, sep="\t", header = TRUE, row.names=1)
  m2 = data.matrix(data)
  if (column_dis_v=="field_of_science" | column_dis_b=="field_of_science" ){
    fields= read.table( "/data/00791/xwj/xalt/fields.txt", sep="\t", row.names=1)
    if (column_dis_b=="field_of_science"){
      data = read.table( data_path, sep="\t", header = FALSE, row.names=1)
      m2 = data.matrix(data)
      data = as.data.frame(t(data))
      all <- merge(x=data, y=fields,by.x="name ",by.y= 0, all=TRUE)
      library("data.table")
      dt <- data.table(all)
      dt<-dt[, 2:ncol(all), with=FALSE]
      tmp=dt[, lapply(.SD, as.character), by=V2]
      tmp=tmp[, lapply(.SD, as.numeric), by=V2]
      mfdt <- tmp[, lapply(.SD, sum), by=V2][V2!="REMOVE"] # 9 is "REMOVE"
      trans = t(mfdt[,2:ncol(mfdt),with=FALSE])
      colnames(trans)=mfdt$V2
      
      library("RColorBrewer")
      options(scipen=10)
      par(mar=c(8,8,2,0)+0.1, mgp=c(7,1,0))
      trans_col_top10= trans[, order(-colSums(trans))]
      trans_row_top10 = trans_col_top10[order(-rowSums(trans_col_top10)),][1:10,]
      barplot(trans_row_top10, cex.names=1, col=brewer.pal(nrow(trans_row_top10), "Spectral"), las=2,
              main="field_of_science by exec_path",
              ylab="Number of Executions", xlab="field_of_science")
      legend("topright", legend=rownames(trans_row_top10), fill=brewer.pal(nrow(trans_row_top10), "Spectral"), cex = 1,box.col="white", ncol=1, y.intersp=0.8)
      
    }else{
      all <- merge(data, fields, by=0, all=TRUE)
      all <-all[!is.na(all[,2]),]  #remove the NA rows, why NA rows?
      addNA(all[,ncol(all)])
      
      #merging rows by the new sciences fields. remove the row 9 which is for REMOVE
      library("data.table")
      dt <- data.table(all)
      #dt[, 130, with=FALSE][is.na(dt[, 130, with=FALSE])]
      dt<-dt[, 2:ncol(all), with=FALSE]
      mfdt <- dt[, lapply(.SD, sum), by=V2][V2!="REMOVE"] # 9 is "REMOVE"
      m2 <- data.matrix(mfdt)
      if (column_dis_b=="field_of_science"){
        m2 = t(m2)
      }
      #top <-m2[,order(-colSums(m2))][,1:10]
      
      library("RColorBrewer")
      options(scipen=10)
      par(mar=c(8,8,2,0)+0.1, mgp=c(7,1,0))
      barplot(m2[, order(-colSums(m2))][,1:top_n_apps], cex.names=1, col=brewer.pal(nrow(m2), "Spectral"), las=2,
              main="Executable Usages by Fields of Science",
              ylab="Number of Executions", xlab="Executables")
      legend("topright", legend=as.character(mfdt$V2), fill=brewer.pal(nrow(m2), "Spectral"), cex = 1,box.col="white", ncol=1, y.intersp=0.8)
    }
    

  }else{
    library("RColorBrewer")
    options(scipen=10)
    par(mar=c(9,8,2,0)+0.1, mgp=c(7,1,0))
    data_sub_apps =m2[, order(-colSums(m2))][,1:top_n_apps]
    data_sub_cores =data_sub_apps[order(-rowSums(data_sub_apps)), ][1:10,]
    barplot(data_sub_cores, cex.names=1, col=brewer.pal(nrow(data_sub_cores), "Spectral"), las=2,
            main=sprintf("%s by %s",column_dis_b,column_dis_v),
            ylab="Number of Executions", xlab=sprintf("%s",column_dis_b))
    legend("topright", legend=gsub("\\*","",rownames(data_sub_cores)), fill=brewer.pal(nrow(data_sub_cores), "Spectral"), cex = 1,box.col="white", ncol=1, y.intersp=0.8)
    
  }
}
##################################################################################################################
# rules=50
# fromdisk=pmml_data(pmml_path)
# #use ? igraph::layout_ to see layout options with igraph.
# par(mar = c(0, 0, 0, 0) , lwd=0.7, cex=0.8)
# plot(head(sort(fromdisk, by="lift"), rules), method="graph", measure = "confidence", shading = "lift", 
#      control=list(cex=1, arrowSize=0.6, layout=igraph::nicely(),alpha=0.9))
# 
# 
# fromdisk=pmml_data(pmml_path)
# #use ? igraph::layout_ to see layout options with igraph. 
# par(mar = c(0, 0, 0, 0) , lwd=0.7, cex=0.7)
# plot(head(sort(fromdisk, by="lift"), rules), method="graph" , 
#      control=list(cex=1, arrowSize=0.6,type="itemsets",layout=igraph::nicely(),alpha=0.9))
# 


