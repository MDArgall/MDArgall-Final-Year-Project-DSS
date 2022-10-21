library(shiny)
#REMEMBER TO ADJUST FUNCTIONS, SINCE VARIABLES ARE NOT GLOBAL!!!!!!
#define gantt in thingy

####Functions####
sep_num<-function(y)
{
  strsplit(y, split = ";")
}

sep_num<-function(y)
{
  x <- unlist(regmatches(y, gregexpr('\\(?[0-9,.]+', y)))
  sep_num<-as.numeric(gsub('\\(', '-', gsub(',', '', x)))
  #sep_num<-as.numeric(gsub('\\(', '-', gsub(',', '', unlist(regmatches(x, gregexpr('\\(?[0-9,.]+', x))))))
}

overlap<-function(task,sol,V)
{
  C<-matrix(data = 0,ncol = ncol(V),nrow = nrow(V))
  for (i in 1:nrow(V))
  {
    for (j in 1:ncol(V))
    {
      if((V[i,j]==1)|(sol$D[i,j]==1))
      {
        C[i,j]=1
      }
    }
  }
  t_assigned<-which(sol$tasks$artist==sol$tasks$artist[task])#which tasks are also performed by this artist
  t_assigned=t_assigned[-which(t_assigned==task)]
  if(length(which(C[t_assigned,task]==1))>0)#if t_assigned is dependent on task
  {
    t_assigned=t_assigned[-which(C[t_assigned,task]==1)]
  }
  if(length(which(C[task,t_assigned]==1))>0)
  {
    t_assigned=t_assigned[-which(C[task,t_assigned]==1)]#if task is dependent on t_assigned
  }
  t_overlap<-t_assigned[which(((sol$tasks$start[t_assigned]<=sol$tasks$start[task])#start1 <= start2
                               &(sol$tasks$start[t_assigned]+sol$tasks$length[t_assigned]<=sol$tasks$start[task]+sol$tasks$length[task])#end1<=end2
                               &(sol$tasks$start[t_assigned]+sol$tasks$length[t_assigned]>sol$tasks$start[task]))#end1>start2
                              |
                                ((sol$tasks$start[t_assigned]>=sol$tasks$start[task])#start1 >= start2
                                 &(sol$tasks$start[t_assigned]+sol$tasks$length[t_assigned]>=sol$tasks$start[task]+sol$tasks$length[task])#end1>=end2
                                 &(sol$tasks$start[t_assigned]<sol$tasks$start[task]+sol$tasks$length[task]))#start1<end2
                              |
                                ((sol$tasks$start[t_assigned]>=sol$tasks$start[task])#start1>=start2
                                 &(sol$tasks$start[t_assigned]+sol$tasks$length[t_assigned]<=sol$tasks$start[task]+sol$tasks$length[task]))#end1<=end2
                              |
                                ((sol$tasks$start[t_assigned]<=sol$tasks$start[task])#start1<=start2
                                 &(sol$tasks$start[t_assigned]+sol$tasks$length[t_assigned]>=sol$tasks$start[task]+sol$tasks$length[task]))#end1>=end2
  )]
  overlap<-t_overlap
}

evaluate<-function(sol, sVSa, sVSt, projectlist, V, D, art_det)
{
  C<-matrix(data = 0,ncol = ncol(V),nrow = nrow(V))
  for (i in 1:nrow(V))
  {
    for (j in 1:ncol(V))
    {
      if((V[i,j]==1)|(sol$D[i,j]==1))
      {
        C[i,j]=1
      }
    }
  }
  
  #calculate lengths
  for (t in 3:nrow(sol$tasks)-1)#exclude start and finish tasks
  {
    counter<-0
    sum<-0
    avg<-0
    for (i in 1:nrow(sVSt))#run through all skills
    {
      if (sVSt[i,t]>0)
      {
        counter = counter +1
        avg = avg + (sVSt[i,t]^2)/sVSa[i,sol$tasks$artist[t]]
        sum = sum + sVSt[i,t]
      }
    }
    if(counter==0)#if the task requires no skills, the length will be zero
    {
      sol$tasks$length[t]=0
    }else
    {
      sol$tasks$length[t]=projectlist[[t]]$length*avg/sum#adapted length = original time * (complexity/skill * complexity/sum(complexity of all skills required)) = original time * (complexity^2/skill)/sum(complexity of all skills required)
    }
  }
  #Calculate early start
  sol$tasks$earlystart[1]<-0 #set early start of first task
  sol$tasks$start[1]<-0 #set start of first task
  
  S<-1
  n<-ncol(C)
  L<-rep(0, n)
  parent<-rep(0, n)
  
  while (length(S)<n)
  {
    veV<-integer(0)
    notS<-which(!c(1:n)%in%S)
    for (i in notS)
    {
      if(length(setdiff(which(C[i,]==1),S))==0)
      {
        veV<-append(veV,i)
      }
    }
    for (v in veV)#the task which is not in S, but has neighbors in S
    {
      max <- 0#reset max
      for(j in which(C[v,]==1))#find which neighbor has the maximum distance to v
      {
        if (max==0)
        {
          max=j
        }else{if(L[j]+sol$tasks$length[j]>L[max]+sol$tasks$length[max])
        {
          max=j
        }
        }
      }
      L[v]=L[max]+sol$tasks$length[max] #assign max distance to point v
      sol$tasks$parent[v]=max
      S=append(S,v)
      sol$tasks$earlystart[v]=L[v]#early start date
      if(sol$tasks$earlystart[v]<0){
        sol$tasks$earlystart[v]=0
      }
      if(sol$tasks$whichstart[v]==0)
      {
        sol$tasks$start[v]=sol$tasks$earlystart[v]
      }
    }
  }
  #calcualte late start
  sol$tasks$latestart[nrow(sol$tasks)]<-sol$tasks$start[nrow(sol$tasks)]
  S<-nrow(sol$tasks)
  while (length(S)<n)
  {
    veV<-integer(0)
    notS<-which(!c(1:n)%in%S)
    for (i in notS)
    {
      if(length(setdiff(which(C[,i]==1),S))==0)
      {
        veV<-append(veV,i)
      }
    }
    for (v in veV)#the task which is not in S, but has neighbors in S
    {
      min <- 0#reset min
      for(j in which(C[,v]==1))#find which successor has the earliest start
      {
        if (min==0)
        {
          min=j
        }else{if(sol$tasks$start[j]<sol$tasks$start[min])
        {
          min=j
        }
        }
      }
      S=append(S,v)
      sol$tasks$latestart[v]=sol$tasks$start[min]-sol$tasks$length[v]#late start date
      if(sol$tasks$latestart[v]<0){
        sol$tasks$latestart[v]=0
      }
      if(sol$tasks$whichstart[v]==1)
      {
        sol$tasks$start[v]=sol$tasks$latestart[v]
      }
    }
  }
  ####Overworked####
  overworked<-rep(0,nrow(art_det))
  for (task in 1:nrow(sol$tasks))
  {
    t_overlap<-overlap(task,sol,V)
    if(length(t_overlap)>0)
    {
      overworked[task]=sum(t_overlap)
    }else{
      overworked[task]=0
    }
    
  }
  if(sum(overworked)>0)
  {
    sol$feasible=0
  }else{
    sol$feasible=1
  }
  
  ####Total Length###
  sol$tot_length<-sol$tasks$start[nrow(sol$tasks)]
  
  ####Artist Times###
  emplystart<-rep(0,nrow(art_det))
  emplyend<-rep(0,nrow(art_det))
  time_employed<-rep(0,nrow(art_det))
  total_salary<-rep(0,nrow(art_det))
  for (i in 1:nrow(art_det))
  {
    #intialise start and end to start and end of first task
    ass_task<-which(sol$tasks$artist==i)
    if(length(ass_task>0))#accounts for cases where employees are not assigned
    {
      emplystart[i]=min(sol$tasks$start[ass_task])
      emplyend[i]=max(sol$tasks$start[ass_task]+sol$tasks$length[ass_task])#Start + length
    }
    time_employed[i]=emplyend[i]-emplystart[i] #ROUND UP TO MIN TIME EMPOYED?
    total_salary[i]=time_employed[i]*art_det$Salary.per.day[i]
  }
  sol$tot_cost=sum(total_salary)
  evaluate<-sol
}

fartists<-function(task,sVSa,sVSt)
{
  feasibleartist<-integer(0)
  for(i in 1:ncol(sVSa))
  {
    feasible<-1
    for (j in which(sVSt[,task]>0))
    {
      if(sVSa[j,i]>0)
      {
      }else{
        feasible=0
      }
    }
    if(feasible==1)
    {
      feasibleartist=append(feasibleartist,i)
    }
  }
  fartists=feasibleartist
}


####UI####
# Define UI for data upload app
ui <- fluidPage(
  navbarPage(
    "Assignment Schedules DSS",
    tabPanel("Schedule Generator", 
     # App title 
    titlePanel("Uploading Files"),
    mainPanel(
      # Input: Select a file 
      fileInput("task_det", "Input Task Details",
        multiple = TRUE,
        accept = c("text/csv",
        "text/comma-separated-values,text/plain",
        ".csv")),
      fileInput("art_det", "Input Artist Details",
        multiple = TRUE,
        accept = c("text/csv",
        "text/comma-separated-values,text/plain",
        ".csv")),
      fileInput("sVSt", "Input Task Complexity Ratings",
        multiple = TRUE,
        accept = c("text/csv",
        "text/comma-separated-values,text/plain",
        ".csv")),
      fileInput("sVSa", "Input Artist Skill Ratings",
        multiple = TRUE,
        accept = c("text/csv",
        "text/comma-separated-values,text/plain",
        ".csv")),
      actionButton(inputId = "submit", label = "Optimize", class = "btn btn-primary" ),
      h4(textOutput("Complete")),
    )         
  ),
    tabPanel("Results",
      plotOutput("Archive"),
      downloadButton("downloadData", "Download"),
      sliderInput("solNum", "Schedule Number", min = 1, max = 1, value = 1, step = 1),
      textOutput("ObjectiveFunctionValues"),
      plotOutput("GanttPlot"),
      fluidRow(
        column(width = 4,
          tableOutput("Artists")
          ),
        column(width = 3, offset = 2,
          tableOutput("Tasks")
          )
      ),
    ),
    tabPanel("Settings",
      fluidRow(
        column(3,
          h4("CSV File Settings"),
          # Horizontal line 
          tags$hr(),
          # Input: Checkbox if file has header 
          checkboxInput("header", "Header", TRUE),
          # Input: Select separator 
          radioButtons("sep", "Separator",
            choices = c(Comma = ",",
            Semicolon = ";",
            Tab = "\t"),
            selected = ","),
          # Input: Select quotes 
          radioButtons("quote", "Quote",
            choices = c(None = "",
            "Double Quote" = '"',
            "Single Quote" = "'"),
            selected = '"'),
          # Horizontal line 
          tags$hr(),
          # Input: Select number of rows to display 
          radioButtons("disp", "Display",
            choices = c(Head = "head",
            All = "all"),
            selected = "head")
          ),
          h4("Hyperparameter Settings"),
            tags$hr(),
            column(width = 3, offset = 0.5,
            # Horizontal line
            numericInput(
              "epoch_max",
              "Termination Criterion",
              3,
              min = 0,
              max = 100,
              step = 1,
              width = NULL
              ),
            numericInput(
              "Temp",
              "Initial Temperature",
              1.2,
              min = 0,
              max = 100,
              step = 0.1,
              width = NULL
              ),
            numericInput(
              "cool",
              "Acceptances to Cool",
              5,
              min = 0,
              max = 100,
              step = 1,
              width = NULL
              ),
            numericInput(
              "reheat",
              "Rejects to Reheat",
              40,
              min = 0,
              max = 1000,
              step = 1,
              width = NULL
              ),
            numericInput(
              "infeasible_max",
              "Maximum Infeasible Solutions",
              20,
              min = 0,
              max = 1000,
              step = 1,
              width = NULL
            )
          ),
          column(width = 3, offset = 1,
            numericInput(
            "alpha",
            "Alpha",
            1.5,
            min = 0,
            max = 100,
            step = 0.05,
            width = NULL
            ),
          numericInput(
            "beta",
            "Beta",
            0.95,
            min = 0,
            max = 1,
            step = 0.01,
            width = NULL
            ),
          numericInput(
            "p",
            "Pertubation Probability P",
            0.5,
            min = 0,
            max = 1,
            step = 0.01,
            width = NULL
            ),
          numericInput(
            "h",
            "Pertubation Probability H",
            0.95,
            min = 0,
            max = 1,
            step = 0.01,
            width = NULL
            )
          )
        )
      )
    )
  )
####Server####
archive_r<-reactiveValues(archive=NULL,c_archive=NULL)
gantt_r<-reactiveValues(g=NULL,c=NULL)
server <- function(input, output, session) {
  observeEvent (input$submit, {
    ####Input Data####
    req(input$task_det)
    req(input$art_det)
    req(input$sVSt)
    req(input$sVSa)
    data <- read.csv(input$task_det$datapath,
                     header = input$header,
                     sep = input$sep,
                     quote = input$quote)
    art_det <- read.csv(input$art_det$datapath,
                        header = input$header,
                        sep = input$sep,
                        quote = input$quote)
    sVSt <- read.csv(input$sVSt$datapath,
                     header = input$header,
                     sep = input$sep,
                     quote = input$quote)
    sVSa <- read.csv(input$sVSa$datapath,
                     header = input$header,
                     sep = input$sep,
                     quote = input$quote)
    epoch_max<-input$epoch_max
    Temp<-input$Temp
    cool<-input$cool
    reheat<-input$reheat
    alpha<-input$alpha
    beta<-input$beta
    p<-input$p
    h<-input$h
    infeasible_max<-input$infeasible_max
    ####Format Data####
    #Replace NA Values
    for(i in 1:nrow(sVSa))
    {
      for(j in 1:ncol(sVSa))
      {
        if(is.na(sVSa[i,j]))
        {
          sVSa[i,j]=0
        }
      }
    }
    for(i in 1:nrow(sVSt))
    {
      for(j in 1:ncol(sVSt))
      {
        if(is.na(sVSt[i,j]))
        {
          sVSt[i,j]=0
        }
      }
    }
    data=data[lengths(data)>0]#Only use entries with data
    #Add Row Names
    rownames(data)<-c(data[[1]])
    data<-data[-1]
    
    #Sort dependencies
    projecttemp<-list(name=data[1,1], length = as.numeric(data[2,1]), assoc=sep_num(data[3,1]),dep=sep_num(data[4,1]),pred=sep_num(data[5,1]))
    projectlist<-list(projecttemp)
    for (i in 2:length(data))
    {
      projecttemp<-list(name=data[1,i], length = as.numeric(data[2,i]), assoc=sep_num(data[3,i]),dep=sep_num(data[4,i]),pred=sep_num(data[5,i]))
      projectlist<- append(projectlist, list(projecttemp))
    }
    
    #Create dependency matrix
    V<-matrix(data = 0,ncol = length(projectlist),nrow = length(projectlist))
    D<-matrix(data = 0,ncol = length(projectlist),nrow = length(projectlist))
    for (i in 1:length(projectlist))
    {
      if (length(projectlist[[i]]$pred)>0)
      {
        for (j in 1:length(projectlist[[i]]$pred))
        {
          V[i,projectlist[[i]]$pred[j]]=1
        }
      }
    }
    #Check that everything is dependent on something, and is a predecessor to something
    for (i in 1:length(projectlist))
    {
      if(sum(V[i,])==0 && i>1){
        cat("Error, task",i,"is not dependent on anything.\n")
      }  
      if(sum(V[,i])==0 && i<length(projectlist)){
        cat("Error, task",i,"does not preceed anything.\n")
      }
    }
    
    #Import Artist Skills
    if(length(which(sVSa[,1]==""))>0)
    {
      sVSa=sVSa[-which(sVSa[,1]==""),]
    }
    rownames(sVSa)<-c(sVSa[[1]])
    sVSa<-sVSa[-1]
    
    
    if(length(which(sVSt[,1]==""))>0)
    {
      sVSt=sVSt[-which(sVSt[,1]==""),]
    }
    rownames(sVSt)<-c(sVSt[[1]])
    sVSt<-sVSt[-1]
    
    #Create Project vs Artist Assignment matrix
    aVSt<-matrix(0:0,nrow = ncol(sVSa), ncol = ncol(sVSt))
    rownames(aVSt)<-colnames(sVSa)
    colnames(aVSt)<-colnames(sVSt)
    
    #Create current solution
    tasks<-data.frame(task=c(1:ncol(sVSt)),length=c(1:ncol(sVSt)),start=c(1:ncol(sVSt)), earlystart=c(1:ncol(sVSt)), latestart=c(1:ncol(sVSt)), whichstart=0, artist = 0, parent = 0)#whichstart: 0 = early, 1 = late
    solution<-list(tot_length = 10, tot_cost = 10, tasks = tasks, t=0, feasible=1)
    
    
    current_sol<-solution
    for (i in 1:ncol(sVSt))
    {
      current_sol$tasks[i,1]=projectlist[[i]]$name
      current_sol$tasks[i,2]=projectlist[[i]]$length
    }
    ####Initialise Model####
    start_time <- Sys.time()
  
    t<-1
    A<-0
    epoch<-0
    c<-1
    d<-0
    a<-0
    heat<-0
  
    t_record<-integer(0)
    cost_record<-integer(0)
    length_record<-integer(0)
    archive_record<-integer(0)
    c_record<-integer(0)
    temp_record<-integer(0)
    feasible_record<-integer(0)
    feasible_sol<-integer(0)
    infeasible_count<-0
    
    ####Generate Initial Solution####
    current_sol$feasible=0
    current_sol$D=D
    current_sol$tasks$artist[1]=0#now artsist is assigned to "start" task
    current_sol$tasks$artist[nrow(current_sol$tasks)]=0
    withProgress(message = 'Generating inital schedule:', min = 0, max = nrow(current_sol$D), value = 0, {
      ####Generate Initial Solution####
      for (i in 3:nrow(current_sol$tasks)-1)
      {
        f_art<-fartists(i,sVSa,sVSt)
        current_sol$tasks$artist[i]=sample(f_art,1)
      }
      #evaluate initial solution
      current_sol$t<-0
      current_sol<-evaluate(current_sol, sVSa, sVSt, projectlist, V, D, art_det)
      while(current_sol$feasible==0)
      {
        for(j in 1:nrow(current_sol$D)){
          t_overlap<-overlap(j,current_sol,V)
          for (i in t_overlap){
            current_sol$D[i,j]=1
          }
          setProgress(i, detail = paste("On task ",i,"out of ",nrow(current_sol$D)))
        }
        current_sol<-evaluate(current_sol, sVSa, sVSt, projectlist, V, D, art_det)
      }
      
      archive<-list(current_sol)#enter inital solution into archive
      neighbour_sol<-current_sol#initialise neighbour solution
      feasible_sol<-current_sol
      D_feas<-D
      Sys.time()-start_time
    })
    ###Run Model####
    withProgress(message = 'Generating new schedules:', min = 0, max =epoch_max, value = 0, {
      while(epoch<=epoch_max){
        ####For certain number of iterations####
        ####Generate  Neighboring Solution####
        ####Choose between new assignment or new timeslot
        r<-runif(1,0,1)
        rtask<-ceiling(runif(1)*(nrow(neighbour_sol$tasks)-2))+1#Randomly choose task, excluding start and finish tasks
        if(r<=h){####New assignment
          for (i in 1:nrow(neighbour_sol$D)){
            neighbour_sol$D[rtask,i]=0#remove artificial dependencies of rtask
            neighbour_sol$D[i,rtask]=0#remove artificial dependencies on rtask
            
          }
          f_art<-fartists(rtask,sVSa,sVSt)
          r_art<-sample(f_art,1) #Choose random artist from feasible selection
          aVSt[which(aVSt[,rtask]==1),rtask]=0#Unassign current artist
          aVSt[r_art,rtask]=1#Assign new artist
          neighbour_sol$tasks$artist[rtask]=r_art
          neighbour_sol<-evaluate(neighbour_sol, sVSa, sVSt, projectlist, V, D, art_det)
          while(neighbour_sol$feasible==0)
          {
            for(j in 1:nrow(neighbour_sol$D)){
              t_overlap<-overlap(j,neighbour_sol,V)
              for (i in t_overlap){
                if(neighbour_sol$tasks$start[i]>=neighbour_sol$tasks$start[j])
                {
                  neighbour_sol$D[i,j]=1
                }else{
                  neighbour_sol$D[j,i]=1
                }
              }
            }
            neighbour_sol<-evaluate(neighbour_sol, sVSa, sVSt, projectlist, V, D, art_det)
          }
          
        }else{
          if(r<=p+h){####Change start rules
            for (i in 1:nrow(neighbour_sol$D)){
              neighbour_sol$D[rtask,i]=0#remove artificial dependencies of rtask
              neighbour_sol$D[i,rtask]=0#remove artificial dependencies on rtask
            }
            if(neighbour_sol$tasks$whichstart[rtask]==0)
            {
              neighbour_sol$tasks$whichstart[rtask]=1
            }else{
              neighbour_sol$tasks$whichstart[rtask]=0
            }
            neighbour_sol<-evaluate(neighbour_sol, sVSa, sVSt, projectlist, V, D, art_det)
            while(neighbour_sol$feasible==0)
            {
              for(j in 1:nrow(neighbour_sol$D)){
                t_overlap<-overlap(j,neighbour_sol,V)
                for (i in t_overlap){
                  if(neighbour_sol$tasks$start[i]>=neighbour_sol$tasks$start[j])
                  {
                    neighbour_sol$D[i,j]=1
                  }else{
                    neighbour_sol$D[j,i]=1
                  }
                }
              }
              neighbour_sol<-evaluate(neighbour_sol, sVSa, sVSt, projectlist, V, D, art_det)
            }
          }
          else{#introduce artificial dependency
            for (i in 1:nrow(neighbour_sol$D)){
              neighbour_sol$D[rtask,i]=0#remove artificial dependencies of rtask
              neighbour_sol$D[i,rtask]=0#remove artificial dependencies on rtask
            }
            neighbour_sol<-evaluate(neighbour_sol, sVSa, sVSt, projectlist, V, D, art_det)
            t_overlap<-overlap(rtask,neighbour_sol,V)
            for (i in t_overlap){
              neighbour_sol$D[i,rtask]=1
            }
            neighbour_sol<-evaluate(neighbour_sol, sVSa, sVSt, projectlist, V, D, art_det)
            while(neighbour_sol$feasible==0)
            {
              for(j in 1:nrow(neighbour_sol$D)){
                t_overlap<-overlap(j,neighbour_sol,V)
                for (i in t_overlap){
                  if(neighbour_sol$tasks$start[i]>=neighbour_sol$tasks$start[j])
                  {
                    neighbour_sol$D[i,j]=1
                  }else{
                    neighbour_sol$D[j,i]=1
                  }
                }
              }
              neighbour_sol<-evaluate(neighbour_sol, sVSa, sVSt, projectlist, V, D, art_det)
            }
          }
        }
        
        neighbour_sol$t=t
        
        ####Evaluate Neighboring Solution####
        neighbour_sol=evaluate(neighbour_sol, sVSa, sVSt, projectlist, V, D, art_det)
        
        ####Accept or Reject####
        #Compare to each archive: detect if dominant or non-dominant
        ####Create A tilda
        alreadyin<-0 #checks if current solution is in archive, neighbouring solution wont be in archive
        for (i in 1:length(archive))
        {
          if(archive[[i]]$t==current_sol$t)
          {
            alreadyin=1
          }
        }
        if(alreadyin==1){
          aTilda<-append(archive,list(neighbour_sol))
        }else{
          aTilda<-append(archive,append(list(current_sol),list(neighbour_sol)))
        }
        
        ####Find degree of dominatedness
        dom_neigh<-rep(0,length(aTilda))
        dom_curr<-rep(0,length(aTilda))
        #-1: dominated by archive
        #0: not dominated and doesn't dominate
        #1: dominates archive
        ####Find degree of dominatedness of current solution
        for(i in 1:length(aTilda))
        {
          #Checks if current solution is dominated
          if((aTilda[[i]]$tot_cost<current_sol$tot_cost && aTilda[[i]]$tot_length<=current_sol$tot_length)||(aTilda[[i]]$tot_cost<=current_sol$tot_cost && aTilda[[i]]$tot_length<current_sol$tot_length))
          {
            dom_curr[i]=1#solution is dominated
          }
          #Checks if neighbor solution is dominated
          if((aTilda[[i]]$tot_cost<neighbour_sol$tot_cost && aTilda[[i]]$tot_length<=neighbour_sol$tot_length)||(aTilda[[i]]$tot_cost<=neighbour_sol$tot_cost && aTilda[[i]]$tot_length<neighbour_sol$tot_length))
          {
            dom_neigh[i]=1
          }
        }
        
        ####Determine whether energy meets requirements
        energy<-(length(which(dom_neigh==1))-length(which(dom_curr==1)))/length(aTilda)
        r<-runif(1,0,1)
        if(r<min(1,exp(-energy/Temp)))
        {
          c=c+1
          current_sol=neighbour_sol#replace current solution with neighbouring solution
          #Update dom_neigh to be relevant to archive, not just aTilda
          dom_neigh2<-rep(0,length(archive))
          for(i in 1:length(archive))
          {
            #Checks if neighbour solution is dominated
            if((archive[[i]]$tot_cost<neighbour_sol$tot_cost && archive[[i]]$tot_length<=neighbour_sol$tot_length)|(archive[[i]]$tot_cost<=neighbour_sol$tot_cost && archive[[i]]$tot_length<neighbour_sol$tot_length))
            {
              dom_neigh2[i]=1
            }else{if((archive[[i]]$tot_cost>neighbour_sol$tot_cost && archive[[i]]$tot_length>=neighbour_sol$tot_length)|(archive[[i]]$tot_cost>=neighbour_sol$tot_cost && archive[[i]]$tot_length>neighbour_sol$tot_length))
            {
              dom_neigh2[i]=-1
            } 
            }
          } 
          same<-0
          for(i in length(archive))
          {
            if(neighbour_sol$tot_length==archive[[i]]$tot_length|neighbour_sol$tot_cost==archive[[i]]$tot_cost)
            {
              same = 1 #only accept one iteration of a specfic combination of values 
            }
          }
          if(length(which(dom_neigh2==1))==0&neighbour_sol$feasible==1&same==0)
          {
            if(length(which(dom_neigh2==-1))>0)
            {
              archive=archive[-which(dom_neigh2==-1)]#remove dominated solutions
            }
            archive=append(archive,list(neighbour_sol))#add current solution
            epoch = 0
          }
          ####Rete
          if(neighbour_sol$feasible==0)
          {
            infeasible_count=infeasible_count+1
          }else{
            infeasible_count=0
            feasible_sol=neighbour_sol
            D_feas=D
          }
          if(infeasible_count>=infeasible_max)#if maximum number of infeasible solutions has been reach, reset the current and neighboring solutions to the last feasible solutions
          {
            infeasible_count=0#set infeasible count to 0, since solutions have been reset to a feasible solution
            neighbour_sol=feasible_sol
            current_sol=feasible_sol
            D=D_feas
          }
        }else{
          neighbour_sol=current_sol
          d=d+1
        }
        t=t+1#increment t
        if (d>=reheat)
        {
          Temp = Temp*alpha
          c=0
          d=0
          epoch=epoch+1
          print(epoch)
        }
        if(c>=cool)
        {
          Temp = Temp*beta
          c=0
          d=0
        }
        setProgress(epoch, detail = paste("On epoch ",epoch,"out of ",epoch_max))
      }#end of while
    })#end of progress bar
    archive_r$archive<-archive
    updateSliderInput(inputId = "solNum", max = length(archive))
    ####Plot Pareto front
    arch_cost_length<-matrix(0,nrow=2,ncol=length(archive))
    for(i in 1:length(archive))
    {
      arch_cost_length[1,i]=archive[[i]]$tot_cost
      arch_cost_length[2,i]=archive[[i]]$tot_length
    }
    arch_cost_length<-arch_cost_length[,order(arch_cost_length[1,])]
    if(length(archive)>1)
    {
      output$Archive <- renderPlot({
        plot(arch_cost_length[1,],arch_cost_length[2,],type = "o", xlab = "Total Cost", ylab = "Makespan")
      })
    }else{
      output$Archive <- renderPlot({
        plot(arch_cost_length,arch_cost_length,type = "o", xlab = "Total Cost", ylab = "Makespan",col="white")
        points(arch_cost_length,type = "o")
      })
    }
    output$Complete<-renderText(paste("The process was completed after ",end_time - start_time," minutes. Please see the results tab."))
    ####Artist table
    art_tbl<-matrix(NA,nrow = nrow(art_det),ncol = 2)
    colnames(art_tbl)<-c("Artist Number", "Artist Name")
    for (i in 1:nrow(art_tbl))
    {
      art_tbl[i,1]<-i
      art_tbl[i,2]<-unname(art_det[i,1])
    }
    output$Artists<-renderTable({
      art_tbl
    })
    ####Task table
    task_tbl<-matrix(NA,nrow = ncol(data),ncol = 2)
    colnames(task_tbl)<-c("Task Number", "Task Name")
    for (i in 1:nrow(task_tbl))
    {
      task_tbl[i,1]<-i
      task_tbl[i,2]<-unname(data[1,i])
    }
    output$Tasks<-renderTable({
      task_tbl
    })
    ####Plot Gantt
    Gantt<-integer(0)
    for (j in 1:length(archive))
    {
      gantt<-matrix(data = NA, nrow = ncol(data),ncol = 5)
      
      for(i in 1:nrow(gantt))
      {
        if(length(art_det[archive[[j]]$tasks$artist[i],1])==1)
        {
          gantt[i,5]<-as.numeric(art_det[archive[[j]]$tasks$artist[i],1])
          gantt[i,1]<-as.numeric(archive[[j]]$tasks$artist[i])
        }
        else
        {
          gantt[i,1]<-0
        }
        gantt[i,2]<-as.numeric(data[1,i])
        gantt[i,3]<-as.numeric(archive[[j]]$tasks$start[i])
        gantt[i,4]<-as.numeric(archive[[j]]$tasks$start[i]+archive[[j]]$tasks$length[i])
      }
      Gantt=append(Gantt,list(gantt))
      gantt_r$g<-Gantt
    }
    ####Condenser####
    c_archive<-archive
    for (a in 1:length(c_archive))#for each archived solution
    {
      latestart<-integer(0)
      earlystart<-integer(0)
      avg_start<-c(rep(0,nrow(art_det)))
      for (i in 1:nrow(art_det))#determine average start time for each artist
      {
        avg_start[i]=sum(c_archive[[a]]$tasks$start[which(c_archive[[a]]$tasks$artist==i)])/length(c_archive[[a]]$tasks$start[which(c_archive[[a]]$tasks$artist==i)])
      }
      for(t in 1:nrow(c_archive[[a]]$tasks))
      {
        if(c_archive[[a]]$tasks$artist[t]>0)
        {
          if(c_archive[[a]]$tasks$start[t]<=avg_start[c_archive[[a]]$tasks$artist[t]])
          {
            latestart=append(latestart,t)#set to late start if a task starts before the average
          }
          else
          {
            earlystart=append(earlystart,t)#set to early start if a task starts after the average
          }
        }
      }
      for (i in latestart)
      {
        art=c_archive[[a]]$tasks$artist[i]#determine which artist the task is assigned to 
        art_task<-which(c_archive[[a]]$tasks$artist==art)#the tasks that are assigned to the same artist
        #the task which immediately follows task i
        following_task<-art_task[which(c_archive[[a]]$tasks$start[art_task]==min(c_archive[[a]]$tasks$start[art_task][which(c_archive[[a]]$tasks$start[art_task]>c_archive[[a]]$tasks$start[i])]))]
        D[following_task,i]=1#set task i to precede following task
        c_archive[[a]]$tasks$whichstart[i] = 1 #set task i to late start
      }
      for (i in earlystart)
      {
        art=c_archive[[a]]$tasks$artist[i]#determine which artist the task is assigned to 
        art_task<-which(c_archive[[a]]$tasks$artist==art)#the tasks that are assigned to the same artist
        #the task which immediately precedes task i
        preceding_task<-art_task[which(c_archive[[a]]$tasks$start[art_task]==max(c_archive[[a]]$tasks$start[art_task][which(c_archive[[a]]$tasks$start[art_task]<c_archive[[a]]$tasks$start[i])]))]
        D[i,preceding_task]=1#set preceding task to precede task i
        c_archive[[a]]$tasks$whichstart[i] = 0 #set task i to early start
      }
      c_archive[[a]]=evaluate(c_archive[[a]], sVSa, sVSt, projectlist, V, D, art_det)
    }
    archive_r$c_archive<-c_archive
    Gantt_c<-integer(0)
    for (j in 1:length(c_archive))
    {
      gantt<-matrix(data = NA, nrow = ncol(data),ncol = 5)
      
      for(i in 1:nrow(gantt))
      {
        if(length(art_det[c_archive[[j]]$tasks$artist[i],1])==1)
        {
          gantt[i,5]<-as.numeric(art_det[c_archive[[j]]$tasks$artist[i],1])
          gantt[i,1]<-as.numeric(c_archive[[j]]$tasks$artist[i])
        }
        else
        {
          gantt[i,1]<-0
        }
        gantt[i,2]<-as.numeric(data[1,i])
        gantt[i,3]<-as.numeric(c_archive[[j]]$tasks$start[i])
        gantt[i,4]<-as.numeric(c_archive[[j]]$tasks$start[i]+c_archive[[j]]$tasks$length[i])
      }
      Gantt_c=append(Gantt_c,list(gantt))
      gantt_r$c<-Gantt_c
    }
    end_time <- Sys.time()
  })#end of observe
  objfun<-reactive({
    paste("Makespan:",round(archive_r$archive[[as.numeric(input$solNum)]]$tot_length,2),"days","Total Cost: R",round(archive_r$archive[[as.numeric(input$solNum)]]$tot_cost,2))
  })
  output$ObjectiveFunctionValues<-renderText({
    objfun()
  })
  output$GanttPlot<-renderPlot({
    plot(gantt_r$g[[as.numeric(input$solNum)]][,3],gantt_r$g[[as.numeric(input$solNum)]][,1], col="white", xlab = "Time (days)", ylab = "Agent")
    
    for(i in 1:nrow(gantt_r$g[[as.numeric(input$solNum)]]))
    {
      lines(c(gantt_r$g[[as.numeric(input$solNum)]][i,3],gantt_r$g[[as.numeric(input$solNum)]][i,4]),c(gantt_r$g[[as.numeric(input$solNum)]][i,1],gantt_r$g[[as.numeric(input$solNum)]][i,1]),lwd=10,col=rgb(0,i/nrow(gantt_r$g[[as.numeric(input$solNum)]])*(1-i%%2)+(1-i/nrow(gantt_r$g[[as.numeric(input$solNum)]]))*i%%2, i/nrow(gantt_r$g[[as.numeric(input$solNum)]])*(i%%2)+(1-i/nrow(gantt_r$g[[as.numeric(input$solNum)]]))*(1-i%%2)))
      points(gantt_r$g[[as.numeric(input$solNum)]][i,3],gantt_r$g[[as.numeric(input$solNum)]][i,1],pch = 18,cex=1.2)
      points(gantt_r$g[[as.numeric(input$solNum)]][i,4],gantt_r$g[[as.numeric(input$solNum)]][i,1],pch = 18,cex=1.2)
      text((gantt_r$g[[as.numeric(input$solNum)]][i,3]+gantt_r$g[[as.numeric(input$solNum)]][i,4])/2,gantt_r$g[[as.numeric(input$solNum)]][i,1],i,cex = 0.5 )
    }
  })
  objfun_c<-reactive({
    paste("Makespan:",round(archive_r$c_archive[[as.numeric(input$solNum)]]$tot_length,2),"days","Total Cost: R",round(archive_r$c_archive[[as.numeric(input$solNum)]]$tot_cost,2))
  })
  output$ObjectiveFunctionValues_c<-renderText({
    objfun_c()
  })
  output$GanttPlot_c<-renderPlot({
    plot(gantt_r$c[[as.numeric(input$solNum)]][,3],gantt_r$c[[as.numeric(input$solNum)]][,1], col="white", xlab = "Time (days)", ylab = "Agent")
    
    for(i in 1:nrow(gantt_r$c[[as.numeric(input$solNum)]]))
    {
      lines(c(gantt_r$c[[as.numeric(input$solNum)]][i,3],gantt_r$c[[as.numeric(input$solNum)]][i,4]),c(gantt_r$c[[as.numeric(input$solNum)]][i,1],gantt_r$c[[as.numeric(input$solNum)]][i,1]),lwd=10,col=rgb(0,i/nrow(gantt_r$c[[as.numeric(input$solNum)]])*(1-i%%2)+(1-i/nrow(gantt_r$c[[as.numeric(input$solNum)]]))*i%%2, i/nrow(gantt_r$c[[as.numeric(input$solNum)]])*(i%%2)+(1-i/nrow(gantt_r$c[[as.numeric(input$solNum)]]))*(1-i%%2)))
      points(gantt_r$c[[as.numeric(input$solNum)]][i,3],gantt_r$c[[as.numeric(input$solNum)]][i,1],pch = 18,cex=1.2)
      points(gantt_r$c[[as.numeric(input$solNum)]][i,4],gantt_r$c[[as.numeric(input$solNum)]][i,1],pch = 18,cex=1.2)
      text((gantt_r$c[[as.numeric(input$solNum)]][i,3]+gantt_r$c[[as.numeric(input$solNum)]][i,4])/2,gantt_r$c[[as.numeric(input$solNum)]][i,1],i,cex = 0.5 )
    }
  })
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(archive_r$archive, file, row.names = FALSE)
    }
  )
}
shinyApp(ui, server)