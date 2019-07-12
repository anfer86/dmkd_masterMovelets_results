# --------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------
get.f1.model.byClass2 <- function ( model = NA, pathdir = NA){
  
  if (is.na(model)) return();
  
  filename <- paste0("model_", model, "_prediction.csv")
  
  dt1 <- fread(paste(pathdir,filename, sep = "/") )
  
  colnames(dt1) <- c("true","predicted")
  
  dt1$true <- factor(dt1$true)
  dt1$predicted <- factor(dt1$predicted, levels = levels(dt1$true))
  
  cm <- confusionMatrix( data = dt1$predicted, reference = dt1$true, mode = "prec_recall" )
  
  dt2 <- data.table( 
    class = tstrsplit(rownames(cm$byClass)," ")[[2]],
    classifier = model,
    f1_score = data.table(cm$byClass)$F1,
    precision = data.table(cm$byClass)$Precision,
    recall = data.table(cm$byClass)$Recall
  )
  
  dt2[is.na(dt2)] <- 0
  
  dt2
}

get.f1.model.byClass <- function ( model = NA, pathdir = NA){
  
  if (is.na(model)) return();
  
  filename <- paste0("model_", model, "_report.csv")
  
  dt1 <- fread(paste(pathdir,filename, sep = "/") )
  dt1[,.(class,classifier,f1_score, precision, recall)]
}

# --------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------

get.accuracy.measures <- function ( measure = NA, pathdir = NA){
  
  if (is.na(measure)) return();
  
  filename <- "similarityAnalysis.txt"
  
  dt <- getResultsCVByMeasure(pathdir, paste0(measure,"CV"))
  
  dt1 <- dt[,.(acc1.mean = mean(acc1)),.(measure, params)]
  
  best <- dt1[dt1[ ,.(maxi=.I[which.max(acc1.mean)]), .(measure) ]$maxi]
  
  dt <- fread(paste(pathdir,measure,filename, sep = "/") )
  colnames(dt) <- c("params","acc1", "acc5")
  
  dt[params == best$params]
  
  data.table(method=measure,
             acc1 = round(dt[params == best$params]$acc1 * 100,1),
             acc5 = round(dt[params == best$params]$acc5 * 100,1))
  
  #data.table(method = measure,
  #           acc1.median = round(median(dt$acc1) * 100,1),
  #           acc1.IQR    = round(IQR(dt$acc1) * 100,1),
  #           acc5.median = round(median(dt$acc5) * 100,1),
  #           acc5.IQR    = round(IQR(dt$acc5) * 100,1))
}

# --------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------

getResultsCVByMeasure <- function (pathdir,measure){
  
  filepath <- paste0(pathdir,"/",measure,"/","similarityAnalysis.txt")
  if (file.exists(filepath)){
    dt <- fread(filepath)
    colnames(dt) <- c("repetition","fold","params","acc1","acc5")
    dt[,measure := measure]
  }
  dt
}

# --------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------

get.f1.byClass <- function ( measure = NA, pathdir = NA){
  
  if (is.na(measure)) return();
  
  filename <- "similarityAnalysis.txt"
  
  dt <- getResultsCVByMeasure(pathdir, paste0(measure,"CV"))
  
  dt1 <- dt[,.(acc1.mean = mean(acc1)),.(measure, params)]
  
  best <- dt1[dt1[ ,.(maxi=.I[which.max(acc1.mean)]), .(measure) ]$maxi]
  
  params.filename <- paste0(best$params,".txt")
  dt1 <- fread(paste(pathdir,measure,params.filename, sep = "/") )
  colnames(dt1) <- c("true","predicted")
  
  dt1$true <- factor(dt1$true)
  dt1$predicted <- factor(dt1$predicted, levels = levels(dt1$true))
  
  cm <- confusionMatrix( data = dt1$predicted, reference = dt1$true, mode = "prec_recall" )
  
  dt2 <- data.table( 
    class = tstrsplit(rownames(cm$byClass)," ")[[2]],
    classifier = measure,
    f1_score = data.table(cm$byClass)$F1,
    precision = data.table(cm$byClass)$Precision,
    recall = data.table(cm$byClass)$Recall
  )
  
  dt2[is.na(dt2)] <- 0
  
  dt2  
  
}


# --------------------------------------------------------------------------------
get.best.models.by.class <- function (dt){
  
  dt.wide <- reshape(dt[,.(class,classifier,f1_score)], idvar = c("class"), timevar = "classifier", direction = "wide")
  colnames(dt.wide) <- gsub(pattern = "f1_score.", "", colnames(dt.wide) )
  
  dt.test <- dt.wide[,-c("class")]
  dt.test[is.na(dt.test)] <- 0
  ranked.results <- data.table(t(apply(1-dt.test, 1, rank)))
  
  dt3 <- rbindlist(lapply(1:nrow(ranked.results), function(ix){
    
    x <- ranked.results[ix,]
    x1 <- dt.test[ix,]
    
    best.classifiers <- (x == 0) # Para retornar falso
    
    if (min(x) < max(x)){
      best.classifiers <- (x == min(x))
    }
    
    data.table(best.classifiers)
  }))
  
  
  final.result <- table(melt(dt3))
  dt.final.result <- data.table(final.result)[value == T]
  dt.final.result$variable <- gsub(pattern = "f1_score.","",x = dt.final.result$variable)
  dt.final.result
}
