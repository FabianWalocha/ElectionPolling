preprocessing = function(dataset){
  
  ### merge left-right bias in v22
  
  exch_v22 = function(dset){
    print(dim(dset))
    if(is.na(dset["v23"])){
      dset["v22"]=as.numeric(dset["v24"])+6;
    }else{
      dset["v22"]=abs(as.numeric(dset["v23"])-6);
    }
    return(dset);
  }  
  
  
  dataset["v23"]=apply(dataset["v23"],1,function(x) abs(as.numeric(x)-6));
  dataset["v24"]=apply(dataset["v24"],1,function(x) as.numeric(x)+6);
  
  
  dataset[dataset["v4"]>=1989&dataset["v4"]<=1996&!is.na(dataset["v22"])&dataset["v22"]==1,]["v22"]=NA;
  dataset[dataset["v4"]>=1989&dataset["v4"]<=1996&!is.na(dataset["v22"])&dataset["v22"]==11,]["v22"]=NA;
  dataset = dataset %>% mutate(v22 = coalesce(v22,v23,v24));
  
  # 2004 data for v22 seems to be botched (values are 1,2,3,4,9)
  dataset[dataset["v4"]==2004,"v22"]=NA;
  
  ### merge v76 in v77
  merDim76 = function(x) {
    x = as.numeric(x);
    if(is.na(x))return(NA);
    if(x==1)return(2);
    if(x==2)return(2);
    if(x==3)return(2);
    if(x==4)return(3);
    if(x==5)return(4);
    if(x==6)return(4);
    if(x==7)return(5);
    if(x==8)return(6);
    if(x==9)return(6);
    if(x==10)return(7);
  }     
  merDim77 = function(x) {
    ifelse(is.na(x),return(NA),
           ifelse(x>=4,return(as.numeric(x)-1),
                  ifelse(x==1,return(2),return(x))));
  }
  dataset["v76"] = apply(dataset["v76"],1,merDim76);
  dataset["v77"] = apply(dataset["v77"],1,merDim77);
  dataset = dataset %>% mutate(v76 = coalesce(v76,v77));
  
  ### Name the parties encoded in the numbers in v7
  chang67 = function(x,var2) ifelse(is.na(x),return(NA),return(var2[x]));
  parties = labels(attributes(dataset$v6)$labels);
  d_1HE = dataset;
  d_1HE["v7"] = sapply(t(dataset["v7"]), chang67, var2=parties)
  
  ### Drop merged columns and too sparse columns
  drops <- c("v1","v2","v3","doi","v10","v11","v13","v14","v16","v17","v18",
             "v19","v20","v21","v23","v24","v26","v28","v29","v30","v31","v32",
             "v33","v34","v35","v36","v37","v38","v39","v40","v41","v42","v43",
             "v44","v45","v46","v47","v48","v49","v50","v51","v53","v55","v58",
             "v59","v60","v62","v63","v65","v67","v68","v69","v70","v72","v73",
             "v74","v77","v78","v79","v80","V81");
  dset = d_1HE[ , !(names(d_1HE) %in% drops)]
  
  ## select only western German federal states
  dset = dset[dset["v75"]<=10,]
  
  return(dset) 
}