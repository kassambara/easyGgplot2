#+++++++++++++++++++++++++++++++++++++++++
#Author : Alboukadel KASSAMBARA
#Date :2014-01-19
#e-mail : alboukadel.kassambara@gmail.com
#Web site : http://www.sthda.com
#+++++++++++++++++++++++++++++++++++++++++

#+++++++++++++++++++++++++++++++++++++++++
#software requirement:
# R >= 2.15.1
# ggplot2 >= 0.9.2.1
#required package : c("ggplot2", "plyr",  'grid')
#++++++++++++++++++++++++++++++++++++++++

#rquery.loadPackages(c('ggplot2', 'plyr', 'grid'))

#arrow {grid}: Describe arrows to add to a line. Used by ggplot2.lineplot



#******************************************************************************
#Plot easily a barplot with R package ggplot2
#******************************************************************************
#data: data frame. Columns are variables and rows are observations
#xName : the name of column containing x variable. Default value is NULL.
#yName : the name of column containing y variable.
#groupName : the name of column containing group variable. This variable is used to color plot according to the group
#groupColors: color of groups. groupColors should have the same length as groups
#brewerPalette: this can be also used to indicate group colors. 
#In this case the parameter groupColors should be NULL
#...: other parameters passed on to ggplot2.customize function
ggplot2.barplot<-function(data, xName=NULL, yName=NULL,groupName=NULL, 
                          groupColors=NULL, brewerPalette=NULL,...)
{
  rquery.loadPackages('ggplot2')
  
  #stat: the statistical transformation to use on the data for this layer. Default value is "identity"
  #To get a bar graph of counts, don't map a variable to y, and use stat="bin"
  stat="identity"
  
  if(is.null(xName) & is.null(yName)){
    if(!is.vector(data)) stop("yName is missing or NULL. In this case data should be a numeric or character vector")
    data=cbind(value=data, index=1:length(data))
    xName="index"
    if(is.numeric(data)) yName="value"
    else if(is.character(data)){
      yName=NULL #barplot of the count
      xName="value"
    }
      
  } 
  #x variable is null; create a new data
  #Barplot of y using the index as xName
  if(is.null(xName) & !is.null(yName)){
    y=as.numeric(unlist(data[,yName]))
    data=as.data.frame(cbind(y, index=1:length(y)))
    colnames(data)<-c(yName, "index")
    xName="index"
  }
  #barplot of the count
  else if(!is.null(xName) & is.null(yName)){
    #stat: the statistical transformation to use on the data for this layer. Default value is "identity"
    #To get a bar graph of counts, don't map a variable to y, and use stat="bin"
    stat="bin"
  }
 #data
  data=data.frame(data)
  data[,xName]=factor(data[,xName])
  if(is.null(groupName)){
    if(!is.null(yName)) p<-ggplot(data=data, aes_string(x=xName, y=yName))
    else p<-ggplot(data=data, aes_string(x=xName))#barplot of the count
  }
  else {
    data[,groupName]=factor(data[,groupName])#transform groupName to factor
    p<-ggplot(data=data, aes_string(x=xName, y=yName, fill=groupName))
  }
  p<- p+geom_bar(stat=stat,...)
  
  #group colors
  if(!is.null(groupColors)){
    p<-p+scale_fill_manual(values=groupColors)
    p<-p+scale_colour_manual(values=groupColors)
  } 
  else if(!is.null(brewerPalette)){
    p<-p+scale_fill_brewer(palette=brewerPalette)
    p<-p+scale_colour_brewer(palette=brewerPalette, guide="none")
  } 
  
  #ggplot2.customize : titles, colors, background, legend, ....
  p<-ggplot2.customize(p,...) 
  p
}

#******************************************************************************
#Plot easily a lineplot with R package ggplot2
#******************************************************************************
#data: data frame. Columns are variables and rows are observations
#xName : the name of column containing x variable. Default value is NULL.
#yName : the name of column containing y variable.
#groupName : the name of column containing group variable. This variable is used to color plot according to the group
#addPoint : if TRUE, points are added to the plot. Default value is FALSE
#pointSize, pointShape, pointFill, pointColor : modify point size, shape, fill and color
#arrow : arrow is function from package grid. This function Describe arrows to add to a line.
  #See ?grid::arrow for more details
  # usage : arrow=arrow(). Default value is NULL.
#xAxisType : Indicate the type of x-axis. Possible values =c("categorical", "continuous"). Default value is "categorical".
  #When the variable on the x-axis is numeric, it is sometimes useful to treat it as continuous,
  #and sometimes useful to treat it as categorical.
#groupColors: color of groups. groupColors should have the same length as groups
#brewerPalette: this can be also used to indicate group colors. 
#In this case the parameter groupColors should be NULL
#...: other parameters passed on to ggplot2.customize function
ggplot2.lineplot<-function(data, xName, yName,  groupName=NULL,
                           addPoint=FALSE, pointSize=1.5, pointShape=19, pointFill=NULL, pointColor="black",
                           arrow=NULL, xAxisType=c("categorical", "continuous"),                     
                           groupColors=NULL, brewerPalette=NULL,...)
                           {
  rquery.loadPackages('ggplot2')
  if(!is.null(arrow)) rquery.loadPackages('grid')
  data=data.frame(data)
  if(xAxisType[1]=="categorical") data[,xName]=factor(data[,xName])
  else if(xAxisType[1]=="continuous") data[,xName] = as.numeric(data[,xName])
  #basic graphs
  #+++++++++++++++++++++++
  if(is.null(groupName)){
    p<-ggplot(data=data, aes_string(x=xName, y=yName, group=1))
    p<-p+geom_line(arrow=arrow,...)
    if(is.null(pointFill)) pointFill="white"
    if(addPoint) p<-p+geom_point(colour=pointColor, size=pointSize,shape=pointShape, fill=pointFill) 
  }
  # graphs with groups: Set color/shape by another variable
  #+++++++++++++++++++++++
  else{    
    data[,groupName]=factor(data[,groupName])#transform groupName to factor
    p<-ggplot(data=data, aes_string(x=xName, y=yName,group=groupName, color=groupName,linetype=groupName,
                                    shape=groupName, fill=groupName )) 
    p<-p+geom_line(arrow=arrow,...) 
    if(addPoint){
      if(is.null(pointFill)) p<-p+geom_point(size=pointSize,...)
      else p<-p+geom_point(size=pointSize, fill=pointFill,...)
    } 
  } 
  #group colors
  if(!is.null(groupColors)){
    p<-p+scale_fill_manual(values=groupColors)
    p<-p+scale_colour_manual(values=groupColors)
  } 
  else if(!is.null(brewerPalette)){
    p<-p+scale_fill_brewer(palette=brewerPalette)
    p<-p+scale_colour_brewer(palette=brewerPalette, guide="none")
  }  
  #ggplot2.customize : titles, colors, background, legend, ....
  p<-ggplot2.customize(p,...) 
  p
}


#******************************************************************************
#Mean plot avec ggplot2; peut prendre tous les parametres de barplot et lineplot
#******************************************************************************
#data: data frame; 
#xName : nom de la variable en x
#yName : nom de la variable en y
#groupName : Nom de groupe pour la coloration
#plotType : type de graphique (lineplot ou barplot)
#error.bars : type de barre d'erreur (standard error, standard deviation, confidence interval)
#level : niveau de confiance; utiliser dans le cas ou error.bars = conf.int
#errorBarWidth, errorBarColor : largeur et couleur de la barre d'erreur
#printSummary : affiche une table montrant les moyennes et les diff?rents types d'erreur calculees
#pairedSample : cas ou les deux groupes sont apparies
#idSubject : colonne contenant le nom des identifiants des ?chantillons; ? indiquer lorsque pairedSample=T
#errorBarPosition : utiliser dans le cas d'un barplot pour ajuster la position des barres d'erreur par rapport au barplot
ggplot2.plotMeans<-function(data="", xName, yName,groupName=NULL, 
                            plotType=c('lineplot', 'barplot'),
                            error.bars = c("se", "sd","conf.int", "none"), level = 0.95,
                            errorBarWidth=0.2, errorBarColor=NULL,
                            printSummary=F, #print summary table
                            #in paired sample only
                            pairedSample=FALSE, idSubject=NULL, #column name containing subject id
                            #used when groupName!=null in barplot case only
                            errorBarPosition=position_dodge(0.9),
                            ...){
  rquery.loadPackages('ggplot2')
  
  data=data.frame(data)
  data[,xName]=factor(data[,xName])
  
  if(pairedSample){
    # Treat subject ID as a factor
    if(is.null(groupName)) df <- summarySEwithin(data, measurevar=yName,
                                                 withinvars=c(xName), idvar=idSubject,conf.interval=level)
    else df <- summarySEwithin(data, measurevar=yName,idvar=idSubject,
                               withinvars=c(groupName,xName),conf.interval=level)
    
  }
  else{
    # summarySE provides the standard deviation, standard error of the mean,
    # and a (default 95%) confidence interval
    if(is.null(groupName)) df <- summarySE(data, measurevar=yName, groupvars=c(xName),
                                           conf.interval=level)
    else df <- summarySE(data, measurevar=yName, groupvars=c(xName,groupName),
                         conf.interval=level)
  }
  
  #calulation of error bar
  #satandard deviation
  ymin=NULL
  ymax=NULL
  if(error.bars[1]=='sd'){
    ymin=df[,yName]-df[,'sd']
    ymax=df[,yName]+df[,'sd']  		
  }
  #standard error
  else if(error.bars[1]=='se'){
    ymin=df[,yName]-df[,'se']
    ymax=df[,yName]+df[,'se']			
  }
  #confidence interval
  else if(error.bars[1]=='conf.int'){
    ymin=df[,yName]-df[,'ci']
    ymax=df[,yName]+df[,'ci']			
  }   
  if(error.bars[1]=='none') data=df
  else  data=cbind(df, ymin=ymin, ymax=ymax)
  #lineplot
  if(plotType[1]=='lineplot'){
    
    p<-ggplot2.lineplot(data=data, xName=xName, yName=yName,
                        groupName=groupName, ...)           	
    #add error bar
    if(error.bars[1]!='none'){
      if(is.null(errorBarColor))p<-p+geom_errorbar(aes_string(ymin='ymin', ymax='ymax'),
                                                   width=errorBarWidth, ...)	
      else p<-p+geom_errorbar(aes_string(ymin='ymin', ymax='ymax'),
                              width=errorBarWidth,colour=errorBarColor,...)     	
    }
  }
  #barplot
  else if(plotType[1]=='barplot'){
    p<-ggplot2.barplot(data=data, xName=xName, yName=yName,
                       groupName=groupName, position="dodge",...)
    #add error bar
    if(error.bars[1]!='none'){
      if(is.null(errorBarColor))p<-p+geom_errorbar(aes_string(ymin='ymin', ymax='ymax'),
                                                   width=errorBarWidth,position=errorBarPosition, ...)	
      else p<-p+geom_errorbar(aes_string(ymin='ymin', ymax='ymax'),
                              width=errorBarWidth,colour=errorBarColor,
                              position=errorBarPosition,...)     	
    }
  }#end of barplot
  
  if(printSummary){
    cat("\n\n----------------------------------------------\nSummary of Data\n----------------------------------------------\n")
    print(data, digits=1)  
    cat("\n----------------------------------------------\n\n")
  }
  
  p
  
}

#******************************************************************************
#Plot easily a histogram plot with R package ggplot2
#******************************************************************************
#data: data frame. Columns are variables and rows are observations
#xName : the name of column containing x variable.
#yName : the name of column containing y variable.
#groupName : the name of column containing group variable. This variable is used to color plot according to the group

#position : Change the position adjustment to use for overlapping points on the layer. 
  #Possible values for the argument position is "identity", "stack", "dodge".

#addMeanLine: if TRUE, the mean line is added on the plot for each group. Default value is FALSE
#meanLineColor, meanLineType, meanLineSize : mean line color, type and size.

#addDensityCurve: if true, add density curve. Default value is FALSE
#densityFill: the fill color of density plot. The value is considered only if groupName=NULL. 
#if groupName is specified, density curves are colored according groupColors pr brewerPalette.
#densityAlpha: degre of transparency of overlaid colors for density curves. Default is 0.2 (20%)
#densityLineType, densityLineColor: line type and color for density curve

#scale: Indicate whether y axis values are density or frequency. Default value is frequency.

#groupColors: color of groups. groupColors should have the same length as groups
#brewerPalette: this can be also used to indicate group colors. 
#In this case the parameter groupColors should be NULL
#...: other parameters passed on to ggplot2.customize function
ggplot2.histogram<-function(data, xName=NULL, groupName=NULL, position=c("identity", "stack", "dodge"),
                            addMeanLine=FALSE, meanLineColor=NULL, meanLineType="dashed", meanLineSize=1,
                            addDensityCurve=FALSE, densityFill="#FF6666", densityAlpha=0.2, densityLineType="solid", densityLineColor="#2F2F2F",
                            scale=c("frequency", "density"),groupColors=NULL, brewerPalette=NULL,...)
{
  
  rquery.loadPackages('ggplot2')
  
  #if xName is missing or null, data should be a numeric vector
  if(is.null(xName) & !is.numeric(data))
    stop("xName is missing or NULL. In this case data should be a numeric vector")
  #data is a numeric vector
  else if(is.numeric(data)){
    data=cbind(x=data, grp=rep(1, length(data)))
    xName="x"
  }
  
  #data
  data=data.frame(data)
  if(is.null(groupName))p<-ggplot(data=data, aes_string(x=xName))
  else {
    data[,groupName]=factor(data[,groupName])#transform groupName to factor
    p<-ggplot(data=data, aes_string(x=xName, fill=groupName, colour=groupName))
  }
  #add density curve
  if(addDensityCurve) {
    if(!is.null(groupName)) {
      p<-ggplot(data=data, aes_string(x=xName, fill=groupName, colour=groupName))
      p<-p+ geom_histogram(aes(y=..density..),position=position[1], ...) #hist with density	
      p<-p+geom_density(alpha=densityAlpha, linetype=densityLineType)
    }
    else{
      p<-p+ geom_histogram(aes(y=..density..),position=position[1], ...) #hist with density	
      p<-p+geom_density(fill=densityFill, alpha=densityAlpha, linetype=densityLineType,colour=densityLineColor, ...)
    }
    
  }	
  else {
    if(scale[1]=="density") p<-p+ geom_histogram(aes(y=..density..),position=position[1], ...) #hist with density
    else p<-p+geom_histogram(aes(y=..count..), position=position[1], ...)#histogram with count
  }
  #add Mean line
  if(addMeanLine){		
    #cas d'une seule variable
    if(is.null(groupName)){
      if(is.null(meanLineColor)) meanLineColor='red'
      m=mean(data[,xName], na.rm=T)
      p<-p+geom_vline(aes_string(xintercept=m),
                      color=meanLineColor, linetype=meanLineType, size=meanLineSize,...)						
    }#end of if
    #cas de plusieurs group
    else {
      rquery.loadPackages('plyr')
      df<-data.frame(grp=factor(data[,groupName]), x=data[,xName])
      df.m=ddply(df, .(grp), summarise, x.mean=mean(x))
      names(df.m)<-c(groupName,'x.mean')	
      
      if(is.null(meanLineColor)) p<-p+geom_vline(data=df.m, 
                                                 aes_string(xintercept='x.mean', colour=groupName),
                                                 linetype=meanLineType, size=meanLineSize,...)
      
      else p<-p+geom_vline(data=df.m, aes_string(xintercept='x.mean'),
                           linetype=meanLineType, color=meanLineColor,size=meanLineSize,...)													
    }#end of else
    
  }#end of addMeanLine
  
  #group colors
  if(!is.null(groupColors)){
    p<-p+scale_fill_manual(values=groupColors)
    p<-p+scale_colour_manual(values=groupColors)
  } 
  else if(!is.null(brewerPalette)){
    p<-p+scale_fill_brewer(palette=brewerPalette)
    p<-p+scale_colour_brewer(palette=brewerPalette, guide="none")
  } 
  #ggplot2.customize : titles, colors, background, legend, ....
  p<-ggplot2.customize(p,...) 
  p
}


#******************************************************************************
#Plot easily a density plot with R package ggplot2
#******************************************************************************
#data: data frame. Columns are variables and rows are observations
#xName : the name of column containing x variable.
#yName : the name of column containing y variable.
#groupName : the name of column containing group variable. This variable is used to color plot according to the group
#addMeanLine: if TRUE, the mean line is added on the plot for each group. Default value is FALSE
#meanLineColor, meanLineType, meanLineSize : mean line color, type and size.
#densityFill: fill color of density plot. This is only considered when groupName=NULL.
#fillGroupDensity: if TRUE, density curve of each group is filled. Default value is FALSE.
#colorGroupDensityLine: if TRUE, density curve line are colored. Default value is FALSE
#groupColors: color of groups. groupColors should have the same length as groups
#brewerPalette: this can be also used to indicate group colors. 
#In this case the parameter groupColors should be NULL
#...: other parameters passed on to ggplot2.customize function
ggplot2.density<-function(data, xName=NULL, groupName=NULL,
                          addMeanLine=FALSE, meanLineColor=NULL, meanLineType="dashed", meanLineSize=1,
                          densityFill=NULL,fillGroupDensity=FALSE, colorGroupDensityLine=FALSE,
                          groupColors=NULL, brewerPalette=NULL,...)
{
  rquery.loadPackages('ggplot2')
  
  #if xName is missing or null, data should be a numeric vector
  if(is.null(xName) & !is.numeric(data))
    stop("xName is missing or NULL. In this case data should be a numeric vector")
  #data is a numeric vector
  else if(is.numeric(data)){
    data=cbind(x=data, grp=rep(1, length(data)))
    xName="x"
  }
  
  #data
  data=data.frame(data)
  if(is.null(groupName)){
    p<-ggplot(data=data, aes_string(x=xName))
    if(!is.null(densityFill) && densityFill !='') p<-p+geom_density(fill=densityFill,...)
    else p<-p+geom_density(...)
  }
  else {
    #transform groupName to factor
    data[,groupName]=factor(data[,groupName])
    #remplissage de la courbe
    if(fillGroupDensity==TRUE){
      #coloration des ligne
      if(colorGroupDensityLine)p<-ggplot(data=data, aes_string(x=xName, fill=groupName,
                                                               colour=groupName))
      else p<-ggplot(data=data, aes_string(x=xName, fill=groupName))
    }
    else p<-ggplot(data=data, aes_string(x=xName, colour=groupName))	  
    p<-p+geom_density(...) 										
  }
  #add Mean line
  if(addMeanLine){		
    #cas d'une seule variable
    if(is.null(groupName)){
      if(is.null(meanLineColor)) meanLineColor='red'
      m=mean(data[,xName], na.rm=T)
      p<-p+geom_vline(aes_string(xintercept=m),
                      color=meanLineColor, linetype=meanLineType, 
                      size=meanLineSize)
    }#end of if
    #cas de plusieurs group
    else {
      rquery.loadPackages('plyr')
      df<-data.frame(grp=factor(data[,groupName]), x=data[,xName])
      df.m=ddply(df, .(grp), summarise, x.mean=mean(x))	
      names(df.m)<-c(groupName,'x.mean')	
      if(is.null(meanLineColor)) p<-p+geom_vline(data=df.m, aes_string(xintercept='x.mean', colour=groupName),                
                                                 linetype=meanLineType, size=meanLineSize)											
      else p<-p+geom_vline(data=df.m, aes_string(xintercept='x.mean', colour=groupName),
                           linetype=meanLineType, color=meanLineColor,size=meanLineSize)  
    }#end of else	 
  }#end of addMeanLine
  #group colors
  if(!is.null(groupColors)){
    p<-p+scale_fill_manual(values=groupColors)
    p<-p+scale_colour_manual(values=groupColors)
  } 
  else if(!is.null(brewerPalette)){
    p<-p+scale_fill_brewer(palette=brewerPalette)
    p<-p+scale_colour_brewer(palette=brewerPalette, guide="none")
  } 
  #ggplot2.customize : titles, colors, background, legend, ....
  p<-ggplot2.customize(p,...) 
  p
}



#******************************************************************************
#Plot easily a boxplot plot with R package ggplot2
#******************************************************************************
#data: data.frame or a numeric vector. Columns are variables and rows are observations
#xName : the name of column containing x variable (i.e groups).
#yName : the name of column containing y variable.
#groupName : the name of column containing group variable. This variable is used to color plot according to the group
#addMean : if TRUE, the mean point is added on the plot for each group. Default value is FALSE
#meanPointShape, meanPointSize : the shape and size of mean point
#meanPointColor:border color of the mean point. Default is black
#meanPointFill: fill color of mean point. 
#This parameter is used only when meanPointShape=21 to 25. Default value is "blue"
#addDot: if TRUE, dotplot is added on the boxplot. Default value is FALSE.
#dotSize: the size of dots
#dotPosition: dot can be centered or jittered
#jitter: degree of jitter in x direction
#groupColors: color of groups. groupColors should have the same length as groups
#brewerPalette: this can be also used to indicate group colors. 
#In this case the parameter groupColors should be NULL
#...: other parameters passed on to ggplot2.customize function
ggplot2.boxplot<-function(data, xName=NULL, yName=NULL, groupName=NULL,
                          addMean=F, meanPointShape=5, meanPointSize=4, meanPointColor="black", meanPointFill="blue",
                          addDot=FALSE, dotSize=1, dotPosition=c("center", "jitter"), jitter=0.2,
                          groupColors=NULL, brewerPalette=NULL,...)
{
  rquery.loadPackages('ggplot2')
  #if yName is missing or null, data should be a numeric vector
  if(is.null(yName) & !is.numeric(data))
    stop("yName is missing or NULL. In this case data should be a numeric vector")
  #data is a numeric vector
  else if(is.numeric(data)){
    data=cbind(y=data, x=rep(1, length(data)))
    xName="x"
    yName="y"
  }
  #xName is missing or  NULL => single boxplot corresponding to the deistribution of the variable
  #bind group column to data
  if(is.null(xName)){
    data=cbind(data, x=rep(1, nrow(data)))
    xName="x"
  }
  #data
  data=data.frame(data)
  data[,xName]=factor(data[,xName])
  #plot
  if(is.null(groupName))p<-ggplot(data=data, aes_string(x=xName, y=yName))
  else{
    data[,groupName]=factor(data[,groupName])#transform groupName to factor
    p<-ggplot(data=data, aes_string(x=xName, y=yName, fill=groupName))
  }
  p<-p+geom_boxplot(...)
  #add Mean point
  if(addMean) p<-p+stat_summary(fun.y=mean, geom='point', shape=meanPointShape, 
                                size=meanPointSize, colour=meanPointColor, fill=meanPointFill)
  #add dot
  if(addDot){
    if(dotPosition[1]=="center") p<-p+geom_dotplot(binaxis='y', stackdir='center', dotsize=dotSize, ...)
    else p<-p+geom_jitter(position=position_jitter(jitter), cex=dotSize, shape=16)
  }
  #group colors
  if(!is.null(groupColors)){
    p<-p+scale_fill_manual(values=groupColors)
    p<-p+scale_colour_manual(values=groupColors)
  } 
  else if(!is.null(brewerPalette)){
    p<-p+scale_fill_brewer(palette=brewerPalette)
    p<-p+scale_colour_brewer(palette=brewerPalette, guide="none")
  } 
  #ggplot2.customize : titles, colors, background, legend, ....
  p<-ggplot2.customize(p,...) 
  p 
}

#******************************************************************************
#Plot easily a violinplot with R package ggplot2
#******************************************************************************
#data: data frame. Columns are variables and rows are observations
#xName : the name of column containing x variable (i.e groups).
#yName : the name of column containing y variable.
#groupName : the name of column containing group variable. This variable is used to color plot according to the group
#addMean : if TRUE, the mean point is added on the plot for each group. Default value is FALSE
#meanPointShape, meanPointSize : the shape and size of mean point
#meanPointColor:border color of the mean point. Default is black
#meanPointFill: fill color of mean point. 
#This parameter is used only when meanPointShape=21 to 25. Default value is "blue"
#addDot: if TRUE, dotplot is added on the boxplot. Default value is FALSE.
#dotSize: the size of dots
#dotPosition: dot can be centered or jittered
#jitter: degree of jitter in x direction
#groupColors: color of groups. groupColors should have the same length as groups
#brewerPalette: this can be also used to indicate group colors. 
#In this case the parameter groupColors should be NULL
#...: other parameters passed on to ggplot2.customize function, geom_violin and geom_dotplot
ggplot2.violinplot<-function(data, xName=NULL, yName=NULL, groupName=NULL,
                             addMean=F, meanPointShape=5, meanPointSize=4, meanPointColor="black", meanPointFill="blue",
                             addDot=FALSE, dotSize=1, dotPosition=c("center", "jitter"), jitter=0.2,
                             groupColors=NULL, brewerPalette=NULL,...)
{
  rquery.loadPackages('ggplot2')
  
  #if yName is missing or null, data should be a numeric vector
  if(is.null(yName) & !is.numeric(data))
    stop("yName is missing or NULL. In this case data should be a numeric vector")
  #data is a numeric vector
  else if(is.numeric(data)){
    data=cbind(y=data, x=rep(1, length(data)))
    xName="x"
    yName="y"
  }
  #xName is missing or  NULL => single boxplot corresponding to the deistribution of the variable
  #bind group column to data
  if(is.null(xName)){
    data=cbind(data, x=rep(1, nrow(data)))
    xName="x"
  }
  
  #data
  data=data.frame(data)
  data[,xName]=factor(data[,xName])
  #plot
  if(is.null(groupName))p<-ggplot(data=data, aes_string(x=xName, y=yName))
  else{
    p<-ggplot(data=data, aes_string(x=xName, y=yName, fill=groupName))
    data[,groupName]=factor(data[,groupName])#transform groupName to factor
  }
  p<-p+geom_violin(...)
  #add Mean point
  if(addMean) p<-p+stat_summary(fun.y=mean, geom='point', shape=meanPointShape, 
                                size=meanPointSize, colour=meanPointColor, fill=meanPointFill)
  #add dot
  if(addDot){
    if(dotPosition[1]=="center") p<-p+geom_dotplot(binaxis='y', stackdir='center', dotsize=dotSize, ...)
    else p<-p+geom_jitter(position=position_jitter(jitter), cex=dotSize, shape=16)
  }
  #group colors
  if(!is.null(groupColors)){
    p<-p+scale_fill_manual(values=groupColors)
    p<-p+scale_colour_manual(values=groupColors)
  } 
  else if(!is.null(brewerPalette)){
    p<-p+scale_fill_brewer(palette=brewerPalette)
    p<-p+scale_colour_brewer(palette=brewerPalette, guide="none")
  } 
  #ggplot2.customize : titles, colors, background, legend, ....
  p<-ggplot2.customize(p,...)
  
  p
}

#******************************************************************************
#Plot easily a dotplot with R package ggplot2
#******************************************************************************
#data: data frame. Columns are variables and rows are observations
#xName : the name of column containing x variable (i.e groups).
#yName : the name of column containing y variable.
#groupName : the name of column containing group variable. This variable is used to color plot according to the group
#position: position_dodge(0.8). The position adjustment to use for overlappling points.
#possible values: position_jitter(0.2) and position_dodge(0.8). 
#position_dodge represents the interval between dotplot of the same group
#addMean : if TRUE, the mean point is added on the plot for each group. Default value is FALSE
#meanPointShape, meanPointSize : the shape and size of mean point
#meanPointColor:border color of the mean point. Default is black
#meanPointFill: fill color of mean point. This parameter is used only when meanPointShape=21 to 25. Default value is "blue"
#addBoxplot: if TRUE, boxplot is added on the dotplot. Default value is FALSE.
#boxplotFill: Fill color of the boxplot. Default value is white.
#boxplotColor: Boxplot line color. Default value is black.
#boxplotLineWeight: Boxplot lineweight. Default value is 0.5.
#groupColors: color of groups. groupColors should have the same length as groups
#brewerPalette: this can be also used to indicate group colors. 
#In this case the parameter groupColors should be NULL
#...: other parameters passed on to ggplot2.customize function
ggplot2.dotplot<-function(data, xName=NULL, yName=NULL, groupName=NULL,
                          position=position_dodge(0.8),
                          addMean=FALSE, meanPointShape=5, meanPointSize=4,
                          meanPointColor="black", meanPointFill="blue",
                          addBoxplot=FALSE, boxplotFill="white", boxplotColor="black",	boxplotLineWeight=0.5,				
                          groupColors=NULL, brewerPalette=NULL,...)
{
  rquery.loadPackages('ggplot2')
  
  #if yName is missing or null, data should be a numeric vector
  if(is.null(yName) & !is.numeric(data))
    stop("yName is missing or NULL. In this case data should be a numeric vector")
  #data is a numeric vector
  else if(is.numeric(data)){
    data=cbind(y=data, x=rep(1, length(data)))
    xName="x"
    yName="y"
  }
  #xName is missing or  NULL => single boxplot corresponding to the deistribution of the variable
  #bind group column to data
  if(is.null(xName)){
    data=cbind(data, x=rep(1, nrow(data)))
    xName="x"
  }
  
  #data
  data=data.frame(data)
  data[,xName]=factor(data[,xName])
  if(is.null(groupName)) p<-ggplot(data=data, aes_string(x=xName, y=yName))
  else {
    data[,groupName]=factor(data[,groupName])#transform groupName to factor
    p<-ggplot(data=data, aes_string(x=xName, y=yName, fill=groupName))
  }
  
  #add boxplot
  if(addBoxplot){  	
    if(is.null(boxplotFill))	  p<-p+geom_boxplot(colour=boxplotColor,  position=position_dodge(0.8), size=boxplotLineWeight,  outlier.shape=NA,...)
    else p<-p+geom_boxplot(fill=boxplotFill, colour=boxplotColor, position=position_dodge(0.8),size=boxplotLineWeight, outlier.shape=NA, ...)
  }
  #dotplot
  p<-p+geom_dotplot(binaxis = "y", stackdir = "center", position=position,...)
  #add Mean point
  if(addMean) p<-p+stat_summary(fun.y=mean, geom='point', shape=meanPointShape, 
                                size=meanPointSize, colour=meanPointColor, fill=meanPointFill)
  #group colors
  if(!is.null(groupColors)){
    p<-p+scale_fill_manual(values=groupColors)
    p<-p+scale_colour_manual(values=groupColors)
  } 
  else if(!is.null(brewerPalette)){
    p<-p+scale_fill_brewer(palette=brewerPalette)
    p<-p+scale_colour_brewer(palette=brewerPalette, guide="none")
  } 
  
  #ggplot2.customize : titles, colors, background, legend, ....
  p<-ggplot2.customize(p,...) 
  p
}




#******************************************************************************
#Plot easily a stripchart with R package ggplot2
#******************************************************************************
#data: data frame. Columns are variables and rows are observations
#xName : the name of column containing x variable (i.e groups).
#yName : the name of column containing y variable.
#groupName : the name of column containing group variable. This variable is used to color plot according to the group
#position: position_jitter(0.2). The position adjustment to use for overlappling points.
#possible values: position_jitter(0.2) and position_dodge(0.8). 
#position_dodge represents the interval between dotplot of the same group
#addMean : if TRUE, the mean point is added on the plot for each group. Default value is FALSE
#meanPointShape, meanPointSize : the shape and size of mean point
#meanPointColor:border color of the mean point. Default is black
#meanPointFill: fill color of mean point. This parameter is used only when meanPointShape=21 to 25. Default value is "blue"
#addBoxplot: if TRUE, boxplot is added on the dotplot. Default value is FALSE.
#boxplotFill: Fill color of the boxplot. Default value is white.
#boxplotColor: Boxplot line color. Default value is black.
#boxplotLineWeight: Boxplot lineweight. Default value is 0.5.
#groupColors: color of groups. groupColors should have the same length as groups
#brewerPalette: this can be also used to indicate group colors. 
#In this case the parameter groupColors should be NULL
#...: other parameters passed on to ggplot2.customize function
ggplot2.stripchart<-function(data, xName=NULL, yName=NULL, groupName=NULL,
                             position=position_jitter(0.2),
                             addMean=FALSE, meanPointShape=5, meanPointSize=4,
                             meanPointColor="black", meanPointFill="blue",
                             addBoxplot=FALSE, boxplotFill="white", boxplotColor="black",  boxplotLineWeight=0.5,				
                             groupColors=NULL, brewerPalette=NULL,...)
{
  rquery.loadPackages('ggplot2')
  
  #if yName is missing or null, data should be a numeric vector
  if(is.null(yName) & !is.numeric(data))
    stop("yName is missing or NULL. In this case data should be a numeric vector")
  #data is a numeric vector
  else if(is.numeric(data)){
    data=cbind(y=data, x=rep(1, length(data)))
    xName="x"
    yName="y"
  }
  #xName is missing or  NULL => single boxplot corresponding to the deistribution of the variable
  #bind group column to data
  if(is.null(xName)){
    data=cbind(data, x=rep(1, nrow(data)))
    xName="x"
  }
  
  #data
  data=data.frame(data)
  data[,xName]=factor(data[,xName])
  if(is.null(groupName)) p<-ggplot(data=data, aes_string(x=xName, y=yName))
  else{
    data[,groupName]=factor(data[,groupName])#transform groupName to factor
    p<-ggplot(data=data, aes_string(x=xName, y=yName, fill=groupName, shape=groupName, colour=groupName))
  }
  #add boxplot
  if(addBoxplot){    
    if(is.null(boxplotFill))	  p<-p+geom_boxplot(colour=boxplotColor,  position=position_dodge(0.8), size=boxplotLineWeight,  outlier.shape=NA,...)
    else p<-p+geom_boxplot(fill=boxplotFill, colour=boxplotColor, position=position_dodge(0.8),size=boxplotLineWeight, outlier.shape=NA, ...)
  }
  #stripchart
  p<-p+geom_jitter(position = position,...)
  #add Mean point
  if(addMean) p<-p+stat_summary(fun.y=mean, geom='point', shape=meanPointShape, 
                                size=meanPointSize, colour=meanPointColor, fill=meanPointFill)
  #group colors
  if(!is.null(groupColors)){
    p<-p+scale_fill_manual(values=groupColors)
    p<-p+scale_colour_manual(values=groupColors)
  } 
  else if(!is.null(brewerPalette)){
    p<-p+scale_fill_brewer(palette=brewerPalette)
    p<-p+scale_colour_brewer(palette=brewerPalette, guide="none")
  } 
  #ggplot2.customize : titles, colors, background, legend, ....
  p<-ggplot2.customize(p,...)
  p
}

ggplot2.jitter<-function(...)
{
  ggplot2.stripchart(...)
}




#******************************************************************************
#Plot easily a scatterplot with R package ggplot2
#******************************************************************************
#data: data frame. Columns are variables and rows are observations
#xName : the name of column containing x variable (i.e groups).
#yName : the name of column containing y variable.
#groupName : the name of column containing group variable. This variable is used to color plot according to the group
#addRegLine: if TRUE, regression line is added. Default value is FALSE
#regLineColor: color of regression line. Default value is blue
#regLineSize: weight of regression line. Default value is 0.5
#smoothingMethod: smoothing method (function) to use, eg. lm, glm, gam, loess, rlm
#-For datasets with n < 1000 default is loess.
#-For datasets with 1000 or more observations defaults to gam.
#-lm for linear smooths, glm for generalised linear smooths, loess for local smooths,
#gam fits a generalized additive model.

#addConfidenceInterval:Display confidence interval around smooth? (FALSE by default)
#confidenceLevel: level controling confidence region. Default is 95%
#confidenceIntervalFill: fill color of confidence intervall

#setColorByGroupName: if TRUE, points are colored according the groups. Default value is TRUE.
#setShapeByGroupName: if TRUE, point shapes are different according to the group. Default value is FALSE.
#groupColors: color of groups. groupColors should have the same length as groups
#brewerPalette: this can be also used to indicate group colors. 
#In this case the parameter groupColors should be NULL
#...: other parameters passed on to ggplot2.customize function
ggplot2.scatterplot<-function(data, xName, yName, groupName=NULL,
                              addRegLine=FALSE,regLineColor="blue",regLineSize=0.5,
                              smoothingMethod=c("lm", "glm", "gam", "loess", "rlm"),
                              addConfidenceInterval=FALSE, confidenceLevel= 0.95,confidenceIntervalFill="#C7C7C7",
                              setColorByGroupName=TRUE, setShapeByGroupName=FALSE,
                              groupColors=NULL, brewerPalette=NULL,...)
  
{
  
  rquery.loadPackages('ggplot2')
  p<-ggplot(data=data, aes_string(x=xName, y=yName))
  
  #Set color/shape by another variable
  #++++++++++
  if(!is.null(groupName)){
    data[,groupName]=factor(data[,groupName])#transform groupName to factor
    #set shape and color by groups
    if(setColorByGroupName & setShapeByGroupName)
      p<-ggplot(data=data, aes_string(x=xName, y=yName, color=groupName, shape=groupName))	
    #set only color by group
    else if(setColorByGroupName==TRUE & setShapeByGroupName==FALSE)
      p<-ggplot(data=data, aes_string(x=xName, y=yName, color=groupName))	
    #set only shape by group
    else if(setColorByGroupName==FALSE & setShapeByGroupName==TRUE)
      p<-ggplot(data=data, aes_string(x=xName, y=yName, shape=groupName))  							
  }  
  #plot
  p<-p+geom_point(...)
  #Add regression line
  if(addRegLine){
    if(is.null(groupName))
      p<-p+geom_smooth(method=smoothingMethod[1], se=addConfidenceInterval, level=confidenceLevel,
                       color=regLineColor, size=regLineSize,fill=confidenceIntervalFill,...) 
    else p<-p+geom_smooth(method=smoothingMethod[1], se=addConfidenceInterval, level=confidenceLevel,
                          size=regLineSize,fill=confidenceIntervalFill,...)       
  }#end addRegressionLine
  else if (addConfidenceInterval) p<-p+geom_smooth(se=addConfidenceInterval, level=confidenceLevel,
                                                   fill=confidenceIntervalFill,...) 
  
  #group colors
  if(!is.null(groupColors)){
    p<-p+scale_fill_manual(values=groupColors)
    p<-p+scale_colour_manual(values=groupColors)
  } 
  else if(!is.null(brewerPalette)){
    p<-p+scale_fill_brewer(palette=brewerPalette)
    p<-p+scale_colour_brewer(palette=brewerPalette, guide="none")
  } 
  
  #ggplot2.customize : titles, colors, background, legend, ....
  p<-ggplot2.customize(p,...)
  p
  
}



#******************************************************************************
#multiplot function
#******************************************************************************
#This is the definition of multiplot. 
#It can take any number of plot objects as arguments, 
#or if it can take a list of plot objects passed to #plotlist.
ggplot2.multiplot <- function(..., plotlist=NULL, cols=2) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # Make the panel
  plotCols = cols                          # Number of columns of plots
  plotRows = ceiling(numPlots/plotCols) # Number of rows needed, calculated from # of cols
  
  # Set up the page
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(plotRows, plotCols)))
  vplayout <- function(x, y)
    viewport(layout.pos.row = x, layout.pos.col = y)
  
  # Make each plot, in the correct location
  for (i in 1:numPlots) {
    curRow = ceiling(i/plotCols)
    curCol = (i-1) %% plotCols + 1
    print(plots[[i]], vp = vplayout(curRow, curCol ))
  }
  
}



#******************************************************************************
#Helper function
#******************************************************************************
#+++++++++++++++++++++++++++++++++++++++++++++
#ggplot2.customize: Customize plot (axis, title, background, color, legend, ....) generated by R package ggplot2
##+++++++++++++++++++++++++++++++++++++++++++++
## Example of use
#plot<-ggplt2.boxplot(data, xName, yName)
#ggplot2.customize(plot, ytitle="y axis", xtitle="x axis", title="my title", ylim=c(10,20))

# Customize plot title
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#mainTitle: Title of the plot
#mainTitleFont: a vector of length 3 indicating respectively the size, the style and the color of main title
#-Possible values for font style are "plain", "italic", "bold", "bold.italic"
#-color can be specified as an hexadecimal code (e.g: "#FFCC00") or by the name (e.g : "red", "green")
#-Default value is: mainTitleFont=c(14, "bold", "black")

# Customize x and y axis for ggplot2 : function ggplot2.setAxis()
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#xShowTitle, yShowTitle : if TRUE, x axis and y axis titles will be shown. Default values are TRUE
#xtitle, ytitle: x and y axis labels
#xtitleFont, ytitleFont : a vector of length 3 indicating respectively the size, the style and the color of x and y axis titles.
#-Possible values for font style:"plain", "italic", "bold", "bold.italic"
#-Color can be specified as an hexadecimal code (e.g: "#FFCC00") or by the name (e.g : "red", "green")
#Default values are c(14,"bold", "black").
#xlim, ylim: limit for the x and y axis. Default values are NULL.
#xScale, yScale: x and y axis scales. Possible values are c("none", "log2", "log10").
#-Example: yScale="log2". Default values are "none".
#xShowTickLabel, yShowTickLabel :  if TRUE, x and y axis tick mark labels will be shown. Default values are TRUE.
#xTickLabelFont, yTickLabelFont: a vector of length 3 indicating respectively the size, the style and the color of x and y axis tick label fonts.
#-Default values are c(12, "bold", "black")
#xtickLabelRotation, ytickLabelRotation : Rotation angle of x and y axis tick labels. Default values are 0.
#hideAxisTicks: if TRUE, x and y axis ticks are removed. Default value is FALSE.
#axisLine: a vector of length 3 indicating respectively the size, the line type and the color of axis lines
#-Default value is c(0.5, "solid", "#E5E5E5").

#Customize plot background
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#removePanelBorder : if TRUE, the top-right border of the plot are removed. Default value is FALSE.
#removePanelGrid: if TRUE, panel grids are removed. Default value is FALSE.
#backgroundColor : background color of plot panel. Default value is "gray"
#gridColor: color of plot panel grids. Default value is "white"

# Customize plot legend
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#showLegend : if TRUE, plot legend will be shown. Default value is TRUE.
#legendPosition: Position of legend box. Default value is "right". 
#-Possible values for legend position:"right", "left","top", "bottom"
#-legendPosition can be also a numeric vector c(x, y) indicating the coordinate of legend box
#-x and y values must be between 0 and 1. c(0,0) corresponds to "bottom left" 
#and  c(1,1) corresponds to "top right" position
#legendBackground: A vector of length 4 indicating boxFill, boxLineSize, boxLineType, boxLineColor
#Default value is c("#FFFFFF", 0.5, "blank", "black" )
#legendTextFont: a vector of length 3 indicating respectively the size, the style and the color of legend text
#Default value is c(10, "plain", "black").
#-Possible values for font style:"plain", "italic", "bold", "bold.italic"
#legendTitleFont: a vector of length 3 indicating respectively the size, the style and the color of legend title
#Default value is c(10, "plain", "black")
#legendItemOrder : character vector indicating the order of items in the legends. Example:  c("2", "1", "0.5")

# Others
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#orientation: change the orientation of the plot. Possible values are c("standard", "horizontal", "yAxisReversed").
#Default value is "standard"

#Facets display subsets of the dataset in different panels.
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#faceting : if TRUE, the data are split up by one or two variables and graphs are displayed in different panels.
#Default value is FALSE. You have to indicate the facetingVarNames
#facetingVarNames: the names of the columns containing variables to use for faceting.
#Default value is NULL.
#facetingDirection: possibles values are "vertical" and "horizontal". Default value is vertical
#facetingScales: By default, all the panels have the same scale (`facetingScales="fixed"`).
  #They can be made independent, by setting scales to `free`, `free_x`, or `free_y`.
#facetingFont : Modify facet label apperance. 
  #A vector of length 3 indicating respectively the size, the style and the color of faceting text
  #Default value is facetingFont=c(size=12, font="plain", color="black")
#facetingRect : Modify the apperance of the rectangle around facet label.
  #facetingRect=list(background=NULL, lineType=NULL, lineColor=NULL, lineSize=NULL)
#facetingTextAngles=c(NULL, NULL) : angle of the x and y texts (in [0, 360])

ggplot2.customize<-function(plot,...)
{
  args=list(...) 
  
  #Facets display subsets of the dataset in different panels.
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  if(!is.null(args$faceting)) faceting=args$faceting else faceting=FALSE
  if(!is.null(args$facetingVarNames)) facetingVarNames=args$facetingVarNames else facetingVarNames=NULL
  if(!is.null(args$facetingDirection)) facetingDirection=args$facetingDirection else facetingDirection="vertical"
  if(!is.null(args$facetingScales)) facetingScales=args$facetingScales else facetingScales="fixed"
  if(!is.null(args$facetingFont)) facetingFont=args$facetingFont else facetingFont=c(size=12, font="plain", color="black")
  if(!is.null(args$facetingRect)) facetingRect=args$facetingRect else facetingRect=list(background=NULL, lineType=NULL, lineColor=NULL, lineSize=NULL)
  if(!is.null(args$facetingTextAngles)) facetingTextAngles=args$facetingTextAngles else facetingTextAngles=c(NULL, NULL)
  if(faceting && !is.null(facetingVarNames)){
    #Faceting is done accoording to one variable
    if(length(facetingVarNames)==1){
      if(facetingDirection=="vertical")
        plot<-plot+ facet_grid(as.formula(paste(facetingVarNames," ~ .", sep='')), scales=facetingScales)
      else if(facetingDirection=="horizontal")
        plot<-plot+ facet_grid(as.formula(paste(". ~ ", facetingVarNames,sep='')), scales=facetingScales) 
    }#end of if
    #Faceting is done accoording to 2 variables
    #first variable is vertical and second variable is horizontal
    else if(length(facetingVarNames)==2)
      plot<-plot+ facet_grid(as.formula(paste(facetingVarNames[1], "~ ", facetingVarNames[2],sep='')), scales=facetingScales) 
    
  }
  
  
  # Background color
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  #Plot panel background color
  if(!is.null(args$backgroundColor)){
    backgroundColor<-args$backgroundColor
    if(backgroundColor%in%c("gray", "grey")) plot<-plot+theme_gray()#gray theme
    else if(backgroundColor=="white") plot<-plot+theme_bw()#black and white theme 
    #for the other color
    else{
      plot<-plot+theme(
        panel.background=element_rect(fill=backgroundColor, size=0.5,
                                      linetype='solid',colour=backgroundColor) )       
    }
  }
  
  #Facet label aperance
  #+++++++++++++++++++++++++++
  #Attention : should be here after backgroundColor modification => don't change the place
  #If you put the text before backgroundColor modification, the facet backgrount remain gray, even if 
  #you indicate another color
  if(faceting && !is.null(facetingVarNames)){
    plot<-plot+theme(
      strip.text.x = element_text(size=facetingFont[1], face=facetingFont[2], color=facetingFont[3], angle=facetingTextAngles[1]),
      strip.text.y = element_text(size=facetingFont[1], face=facetingFont[2], color=facetingFont[3], angle=facetingTextAngles[2]),
      strip.background = element_rect(fill=facetingRect$background, colour=facetingRect$lineColor, 
                                      linetype=facetingRect$lineType, size=facetingRect$lineSize)
    )
  }
  
  #Change axis default value accoording to call arguments
  #+++++++++++++++++++++++++++++++++++
  #x and y axis titles and scales
  if(!is.null(args$xShowTitle)) xShowTitle<-args$xShowTitle else xShowTitle=TRUE
  if(!is.null(args$yShowTitle)) yShowTitle<-args$yShowTitle else yShowTitle=TRUE
  if(!is.null(args$xtitle)) xtitle<-args$xtitle else xtitle=NULL
  if(!is.null(args$ytitle)) ytitle<-args$ytitle else ytitle=NULL
  if(!is.null(args$xtitleFont)) xtitleFont<-args$xtitleFont else xtitleFont=c(14,"bold", "black")
  if(!is.null(args$ytitleFont)) ytitleFont<-args$ytitleFont else ytitleFont=c(14,"bold", "black")
  if(!is.null(args$ylim)) ylim<-args$ylim else ylim=NULL
  if(!is.null(args$xlim)) xlim<-args$xlim else xlim=NULL
  if(!is.null(args$xScale)) xScale<-args$xScale else xScale=c("none", "log2", "log10")
  if(!is.null(args$yScale)) yScale<-args$yScale else yScale=c("none", "log2", "log10")
  #x and y axis tick mark labels
  if(!is.null(args$xShowTickLabel)) xShowTickLabel<-args$xShowTickLabel else xShowTickLabel=TRUE
  if(!is.null(args$yShowTickLabel)) yShowTickLabel<-args$yShowTickLabel else yShowTickLabel=TRUE
  if(!is.null(args$xTickLabelFont)) xTickLabelFont<-args$xTickLabelFont else xTickLabelFont=c(12, "bold", "black")
  if(!is.null(args$yTickLabelFont)) yTickLabelFont<-args$yTickLabelFont else yTickLabelFont=c(12, "bold", "black")
  if(!is.null(args$xtickLabelRotation)) xtickLabelRotation<-args$xtickLabelRotation else xtickLabelRotation=0
  if(!is.null(args$ytickLabelRotation)) ytickLabelRotation<-args$ytickLabelRotation else ytickLabelRotation=0
  if(!is.null(args$hideAxisTicks)) hideAxisTicks<-args$hideAxisTicks else hideAxisTicks=FALSE
  #axis line
  if(!is.null(args$axisLine)) axisLine<-args$axisLine else axisLine=c(0.5, "solid", "#E5E5E5")
  
  
  plot<-ggplot2.setAxis(plot, xShowTitle=xShowTitle, yShowTitle=yShowTitle,
                        xtitle=xtitle, ytitle=ytitle,
                        xtitleFont=xtitleFont, ytitleFont=ytitleFont,
                        xlim=xlim, ylim=ylim,xScale=xScale, yScale=yScale,
                        xShowTickLabel=xShowTickLabel, yShowTickLabel=yShowTickLabel,
                        xTickLabelFont=xTickLabelFont, yTickLabelFont=yTickLabelFont,
                        xtickLabelRotation=xtickLabelRotation, ytickLabelRotation=ytickLabelRotation,
                        hideAxisTicks=hideAxisTicks, axisLine=axisLine)
  
  # Customize main title
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  if(!is.null(args$mainTitle))plot<-plot+ggtitle(args$mainTitle)
  if(!is.null(args$mainTitleFont)) mainTitleFont=args$mainTitleFont
  else mainTitleFont=c(14, "bold", "black")
  plot<-plot+theme(plot.title=element_text(size=as.numeric(mainTitleFont[1]), 
                                           lineheight=1.0,face=mainTitleFont[2], colour=mainTitleFont[3], ))
  
  # plot Panel color and grid
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  #Plot panel grid color
  if(!is.null(args$gridColor)){
    gridColor=args$gridColor
    plot<-plot+theme(panel.grid.major=element_line(size=0.5, linetype='solid',colour=gridColor))+
      theme(panel.grid.minor=element_line(size=0.25, linetype='solid', colour=gridColor))
  }
  #remove top right border of the plot
  if(!is.null(args$removePanelBorder)){
    removePanelBorder=args$removePanelBorder
    if(removePanelBorder) plot<-plot+theme(panel.border=element_blank())
  }
  #Remove panel grid
  if(!is.null(args$removePanelGrid)){
    removePanelGrid=args$removePanelGrid
    if(removePanelGrid) plot<-plot+theme(panel.grid.minor=element_blank(),
                                         panel.grid.major=element_blank(),
                                         axis.line=element_line(size=as.numeric(axisLine[1]),
                                                                linetype=axisLine[2], colour=axisLine[3]))
  }
  
  # Legend
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  if(!is.null(args$showLegend)) showLegend=args$showLegend else showLegend=TRUE
  #possible values for legend position: legendPosition=c("right", "left","top", "bottom")
  #legendPosition can be also a numeric vector c(x, y) indicating the coordinate of legendBox
  #x and y values must be between 0 and 1. c(0,0) corresponds to "bottom left" 
  #and  c(1,1) corresponds to "top right" position
  if(!is.null(args$legendPosition)) legendPosition=args$legendPosition else legendPosition ="right"
  if(showLegend==TRUE) plot<-plot+theme(legend.position=legendPosition)
  else plot<-plot+theme(legend.position='none')
  
  if(showLegend==TRUE){
    #+++++++legend background and text font style+++++
    #Legend background. legendBackground: c(boxFill, boxLineSize, boxLineType, boxLineColor)
    if(!is.null(args$legendBackground)) legendBackground=args$legendBackground 
    else legendBackground=c("#FFFFFF", 0.5, "blank", "black")
    #Legend text font. legendTextFont=c(size, style, color)
    if(!is.null(args$legendTextFont)) legendTextFont=args$legendTextFont
    else legendTextFont=c(10, "plain", "black")
    #legendTitleFont=c(size, style, color)
    if(!is.null(args$legendTitleFont)) legendTitleFont=args$legendTitleFont
    else legendTitleFont=c(10, "bold", "black")
    plot<-plot+
      theme(legend.title=element_text(size=as.numeric(legendTitleFont[1]), 
                                      face=legendTitleFont[2], colour=legendTitleFont[3]))+
      theme(legend.text=element_text(size=as.numeric(legendTextFont[1]), 
                                     face=legendTextFont[2], colour=legendTextFont[3]))+
      theme(legend.background=element_rect(fill=legendBackground[1], size=as.numeric(legendBackground[2]),
                                           linetype=legendBackground[3], colour=legendBackground[4]))
    #legendTitle: title of legend
    if(!is.null(args$legendTitle)) plot<-plot+labs(fill=args$legendTitle, colour=args$legendTitle, shape=args$legendTitle) 
    #legendItemOrder : character vector indicating the order of items in the legends.
    if(!is.null(args$legendItemOrder)) plot<-plot+scale_x_discrete(limits=as.character(args$legendItemOrder))
  }
  
  # Orientation of the plot
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  if(!is.null(args$orientation)) orientation=args$orientation else orientation="standard"
  if(orientation=="horizontal") plot<-plot+coord_flip() 
  if(orientation=="yAxisReversed") plot<-plot+scale_y_reverse() 
  
  plot
}


#++++++++++++++++++++++++++++++++++++++++++++++
# Customize x and y axis for ggplot2
#++++++++++++++++++++++++++++++++++++++++++++++
#xShowTitle, yShowTitle : if TRUE, x axis and y axis titles will be shown. Default values are TRUE
#xtitle, ytitle: x and y axis labels
#xtitleFont, ytitleFont : a vector of length 3 indicating respectively the size, the style and the color of x and y axis titles.
#-Possible values for font style:"plain", "italic", "bold", "bold.italic"
#-Color can be specified as an hexadecimal code (e.g: "#FFCC00") or by the name (e.g : "red", "green")
#Default values are c(14,"bold", "black").
#xlim, ylim: limit for the x and y axis. Default values are NULL.
#xScale, yScale: x and y axis scales. Possible values are c("none", "log2", "log10").
#-Example: yScale="log2". Default values are "none".
#xShowTickLabel, yShowTickLabel :  if TRUE, x and y axis tick mark labels will be shown. Default values are TRUE.
#xTickLabelFont, yTickLabelFont: a vector of length 3 indicating respectively the size, the style and the color of x and y axis tick label fonts.
#-Default values are c(12, "bold", "black")
#xtickLabelRotation, ytickLabelRotation : Rotation angle of x and y axis tick labels. Default values are 0.
#hideAxisTicks: if TRUE, x and y axis ticks are removed. Default value is FALSE.
#axisLine: a vector of length 3 indicating respectively the size, the line type and the color of axis lines
#-Default value is c(0.5, "solid", "#E5E5E5").

#usage: plot<-ggplot2.setAxis(plot, xtitle="x title", ytitle="y title")
ggplot2.setAxis<-function(plot,
                          #x and y axis titles and scales
                          xShowTitle=TRUE, yShowTitle=TRUE, 
                          xtitle=NULL, ytitle=NULL,
                          xtitleFont=c(14,"bold", "black"), ytitleFont=c(14,"bold", "black"),
                          xlim=NULL, ylim=NULL,
                          xScale=c("none", "log2", "log10"), yScale=c("none", "log2", "log10"),
                          #x and y axis tick mark labels
                          xShowTickLabel=TRUE, yShowTickLabel=TRUE,
                          xTickLabelFont=c(12, "bold", "black"), yTickLabelFont=c(12, "bold", "black"),
                          xtickLabelRotation=0, ytickLabelRotation=0,
                          hideAxisTicks=FALSE, axisLine=c(0.5, "solid", "#E5E5E5"))
{
  
  #axis title
  if(!is.null(xtitle)) plot<-plot+xlab(xtitle)
  if(!is.null(ytitle)) plot<-plot+ylab(ytitle)
  
  if(!xShowTitle)plot<-plot+theme(axis.title.x=element_blank())
  else plot<-plot+theme(axis.title.x=element_text(size=as.numeric(xtitleFont[1]),
                                                  face=xtitleFont[2], colour=xtitleFont[3], hjust=0.4))
  
  if(!yShowTitle)plot<-plot+theme(axis.title.y=element_blank())
  else plot<-plot+theme(axis.title.y=element_text(size=as.numeric(ytitleFont[1]),
                                                  face=ytitleFont[2], colour=ytitleFont[3], vjust=0.4, angle=90))
 
  
  #xlim, ylim: limit for the x and y axis
  if(!is.null(ylim)) plot<-plot+ylim(ylim[1], ylim[2])
  if(!is.null(xlim)) plot<-plot+xlim(xlim[1], xlim[2])
  
  #log scale
  if(xScale[1]=="log2") plot<-plot+scale_x_continuous(trans='log2')
  else if(xScale[1]=="log10") plot<-plot+scale_x_log10()
  
  if(yScale[1]=="log2") plot<-plot+scale_y_continuous(trans='log2')
  else if(yScale[1]=="log10") plot<-plot+scale_y_log10()
  
  #x and y axis tick mark labels
  if(!xShowTickLabel) plot<-plot+theme(axis.text.x=element_blank())
  else plot<-plot+theme(axis.text.x=element_text(size=as.numeric(xTickLabelFont[1]),
                                                 face=xTickLabelFont[2], colour=xTickLabelFont[3], angle=xtickLabelRotation))
  if(!yShowTickLabel) plot<-plot+theme(axis.text.y=element_blank())
  else plot<-plot+theme(axis.text.y=element_text(size=as.numeric(yTickLabelFont[1]),
                                                 face=yTickLabelFont[2], colour=yTickLabelFont[3], angle=ytickLabelRotation))
  if(hideAxisTicks) plot<-plot+theme(axis.ticks=element_blank())
  
  #Axis line
  plot<-plot+theme(axis.line=element_line(size=as.numeric(axisLine[1]),
                                          linetype=axisLine[2], colour=axisLine[3]))
  
  plot
}



# Examples:
#rquery.loadPackages(c("base", "jpeg"))
# Purpose: Package installation and loading
rquery.loadPackages <- function(pkgs) {
  # install packages not already loaded:
  pkgs_miss <- pkgs[which(!pkgs %in% installed.packages()[, 1])]
  if (length(pkgs_miss) > 0) {
    cat('==================================================\n')
    cat('some required packages shown below are missed:\n')
    cat(pkgs_miss)
    cat('\nWait few minutes for installation. Internet connexion are required.\n')
    cat('==================================================\n')
    
    source("http://bioconductor.org/biocLite.R")
    all_repos=c('http://cran.r-project.org', biocinstallRepos())
    install.packages(pkgs_miss, repos=all_repos)
  }
  
  # load packages not already loaded:
  attached <- search()
  attached_pkgs <- attached[grepl("package", attached)]
  need_to_attach <- pkgs[which(!pkgs %in% gsub("package:", "", attached_pkgs))]
  
  if (length(need_to_attach) > 0) {
    for (i in 1:length(need_to_attach)) 
      suppressPackageStartupMessages(require(need_to_attach[i], 
                                             character.only = TRUE, quietly=T, warn.conflicts=F))
  }
  
}


## Summarizes data.
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  rquery.loadPackages('plyr')
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This is does the summary; it's not easy to understand...
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun= function(xx, col, na.rm) {
                   c( N    = length2(xx[,col], na.rm=na.rm),
                      mean = mean   (xx[,col], na.rm=na.rm),
                      sd   = sd     (xx[,col], na.rm=na.rm)
                   )
                 },
                 measurevar,
                 na.rm
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean"=measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

## Norms the data within specified groups in a data frame; it normalizes each
## subject (identified by idvar) so that they have the same mean, within each group
## specified by betweenvars.
##   data: a data frame.
##   idvar: the name of a column that identifies each subject (or matched subjects)
##   measurevar: the name of a column that contains the variable to be summariezed
##   betweenvars: a vector containing names of columns that are between-subjects variables
##   na.rm: a boolean that indicates whether to ignore NA's
normDataWithin <- function(data=NULL, idvar, measurevar, betweenvars=NULL,
                           na.rm=FALSE, .drop=TRUE) {
  rquery.loadPackages('plyr')
  
  # Measure var on left, idvar + between vars on right of formula.
  data.subjMean <- ddply(data, c(idvar, betweenvars), .drop=.drop,
                         .fun = function(xx, col, na.rm) {
                           c(subjMean = mean(xx[,col], na.rm=na.rm))
                         },
                         measurevar,
                         na.rm
  )
  
  # Put the subject means with original data
  data <- merge(data, data.subjMean)
  
  # Get the normalized data in a new column
  measureNormedVar <- paste(measurevar, "Normed", sep="")
  data[,measureNormedVar] <- data[,measurevar] - data[,"subjMean"] +
    mean(data[,measurevar], na.rm=na.rm)
  
  # Remove this subject mean column
  data$subjMean <- NULL
  
  return(data)
}

## Summarizes data, handling within-subjects variables by removing inter-subject variability.
## It will still work if there are no within-S variables.
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
## If there are within-subject variables, calculate adjusted values using method from Morey (2008).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   betweenvars: a vector containing names of columns that are between-subjects variables
##   withinvars: a vector containing names of columns that are within-subjects variables
##   idvar: the name of a column that identifies each subject (or matched subjects)
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySEwithin <- function(data=NULL, measurevar, betweenvars=NULL, withinvars=NULL,
                            idvar=NULL, na.rm=FALSE, conf.interval=.95, .drop=TRUE) {
  
  # Ensure that the betweenvars and withinvars are factors
  factorvars <- sapply(data[, c(betweenvars, withinvars), drop=FALSE], FUN=is.factor)
  if (!all(factorvars)) {
    nonfactorvars <- names(factorvars)[!factorvars]
    message("Automatically converting the following non-factors to factors: ",
            paste(nonfactorvars, collapse = ", "))
    data[nonfactorvars] <- lapply(data[nonfactorvars], factor)
  }
  
  # Norm each subject's data    
  data <- normDataWithin(data, idvar, measurevar, betweenvars, na.rm, .drop=.drop)
  
  # This is the name of the new column
  measureNormedVar <- paste(measurevar, "Normed", sep="")
  
  # Replace the original data column with the normed one
  data[,measurevar] <- data[,measureNormedVar]
  
  # Collapse the normed data - now we can treat between and within vars the same
  datac <- summarySE(data, measurevar, groupvars=c(betweenvars, withinvars), na.rm=na.rm,
                     conf.interval=conf.interval, .drop=.drop)
  
  # Apply correction from Morey (2008) to the standard error and confidence interval
  #  Get the product of the number of conditions of within-S variables
  nWithinGroups    <- prod(sapply(datac[,withinvars, drop=FALSE], FUN=nlevels))
  correctionFactor <- sqrt( nWithinGroups / (nWithinGroups-1) )
  
  # Apply the correction factor
  datac$sd <- datac$sd * correctionFactor
  datac$se <- datac$se * correctionFactor
  datac$ci <- datac$ci * correctionFactor
  
  return(datac)
}


#generate a plot of point shapes which R knows about.
#++++++++++++++++++++++++++++++++++++++++++++
generateRPointShapes<-function(){
  oldPar<-par()
  par(font=2, mar=c(0.5,0,0,0))
  y=rev(c(rep(1,6),rep(2,5), rep(3,5), rep(4,5), rep(5,5)))
  x=c(rep(1:5,5),6)
  plot(x, y, pch=0:25, cex=1.5, ylim=c(1,5.5), xlim=c(1,6.5), 
       axes=FALSE, xlab="", ylab="", bg="blue")
  text(x, y, labels=0:25, pos=3)
  par(mar=oldPar$mar,font=oldPar$font )
}

#generate a plot of line types which R knows about.
#++++++++++++++++++++++++++++++++++++++++++++
generateRLineTypes<-function(){
  oldPar<-par()
  par(font=2, mar=c(0,0,0,0))
  plot(1, pch="", ylim=c(0,6), xlim=c(0,0.7),  axes=FALSE,xlab="", ylab="")
  for(i in 0:6) lines(c(0.3,0.7), c(i,i), lty=i, lwd=3)
  text(rep(0.1,6), 0:6, labels=c("0.'blank'", "1.'solid'", "2.'dashed'", "3.'dotted'",
                                 "4.'dotdash'", "5.'longdash'", "6.'twodash'"))
  par(mar=oldPar$mar,font=oldPar$font )
}

#Generate a plot of color names which R knows about.
#++++++++++++++++++++++++++++++++++++++++++++
#cl : a vector of colors to plots
#bg: background of the plot
#rot: text rotation angle

#usage=showCols(bg="gray33")
showCols <- function(cl=colors(), bg = "grey", cex = 0.75, rot = 30) {
  m <- ceiling(sqrt(n <-length(cl)))
  length(cl) <- m*m; cm <- matrix(cl, m)
  ##
  require("grid")
  grid.newpage(); vp <- viewport(w = .92, h = .92)
  grid.rect(gp=gpar(fill=bg))
  grid.text(cm, x = col(cm)/m, y = rev(row(cm))/m, rot = rot,
            vp=vp, gp=gpar(cex = cex, col = cm))
}

# #arrow : arrow is function from package grid. This function Describe arrows to add to a line.
# #See ?grid::arrow for more details
# arrow<-function(angle = 30, length = unit(0.25, "inches"),ends = "last", type = "open")
# {
#   rquery.loadPackages('grid')
#   arrow(angle , length,ends, type)
# }