library(data.table)
library(ggplot2)
library(caret)
library(scmamp)


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


# --------------------------------------------------------------------------------------

nfolds = 5

# --------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------
# ----------------- SIMILARITY MEASURES ------------------------------------------------
# --------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------
pathdir <- "../results/foursquare/"

measures <- c("mddtw","lcss","edr","msm")
  
#    -----------------------------------------------------------------------------------
#    ----------------- ACCURACY --------------------------------------------------------
#    -----------------------------------------------------------------------------------
results.sim.acc <- rbindlist(
  lapply( 1:nfolds,
    function(irun){
      pathdir_irun <- paste0(pathdir, "run", irun, "/Similarity/foursquare/")
      data.frame (
        rbindlist(lapply(measures, function(x){ get.accuracy.measures(x,pathdir_irun)} )),
        fold = irun
        )
      }
    )
  )

results.sim.acc.agg <- results.sim.acc[, .(
                              acc1.mean = mean(acc1), acc1.sd = sd(acc1),
                              acc5.mean = mean(acc5), acc5.sd = sd(acc5) ),
                              by=method]

#    -----------------------------------------------------------------------------------
#    ----------------- BY CLASS --------------------------------------------------------
#    -----------------------------------------------------------------------------------

results.sim.byClass <- rbindlist(
  lapply( 1:nfolds,
          function(irun){
            pathdir_irun <- paste0(pathdir, "run", irun, "/Similarity/foursquare/")
            data.frame (
              rbindlist(lapply(measures, function(x){ get.f1.byClass(x,pathdir_irun)} )),
              fold = irun
            )
        }
    )
)

results.sim.byClass.agg <- results.sim.byClass[, .(
                              f1_scores.mean = mean(f1_score), 
                              precision.mean = mean(precision),
                              recall.mean = mean(recall) ),
                              by=.(class,classifier)]


# --------------------------------------------------------------------------------------
# ----------------- MOVELETS -----------------------------------------------------------
# --------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------

models = c("approachRF300")

results.movelets.acc <- rbindlist(
  lapply( 1:nfolds,
          function(irun){
            pathdir_irun <- paste0(pathdir, "run", irun, "/Movelets/foursquare_movelets/p_false__q_LSP__ms_1__Ms_10/model/")
            
            rbindlist(lapply(models, function(x){
              
              pathdir_irun_model <- paste0( pathdir_irun, 'model_', x, '_history.csv' )
              content <- tail( fread(pathdir_irun_model, header = T) )
              
              data.frame(
                method = paste0('Movelets_',x),
                acc1   = round( as.numeric(content[,3]) * 100, 1),
                acc5   = round( as.numeric(content[,4]) * 100, 1),
                fold   = irun
              )
            })
            )
          }
  )
)

results.movelets.acc.agg <- results.movelets.acc[, .(
  acc1.mean = mean(acc1), acc1.sd = sd(acc1),
  acc5.mean = mean(acc5), acc5.sd = sd(acc5) ),
  by=method]


#    -----------------------------------------------------------------------------------
#    ----------------- BY CLASS --------------------------------------------------------
#    -----------------------------------------------------------------------------------

results.movelets.byClass <- rbindlist(
  lapply( 1:nfolds,
          function(irun){
            pathdir_irun <- paste0(pathdir, "run", irun, "/Movelets/foursquare_movelets/p_false__q_LSP__ms_1__Ms_10/model/")
            data.frame (
              rbindlist(lapply(models, function(x){ get.f1.model.byClass(x,pathdir_irun)} )),
              fold = irun
            )
          }
  )
)

results.movelets.byClass.agg <- results.movelets.byClass[, .(
  f1_scores.mean = mean(f1_score), 
  precision.mean = mean(precision),
  recall.mean = mean(recall) ),
  by=.(class,classifier)]


# --------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------
# ----------------- MASTER MOVELETS ----------------------------------------------------
# --------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------

models = c("approachRF300")

results.masterMovelets.acc <- rbindlist(
  lapply( 1:nfolds,
          function(irun){
            pathdir_irun <- paste0(pathdir, "run", irun, "/MasterMovelets/foursquare_ED/mnf_-1__q_LSP__ms_1__Ms_10/model/")
            
            rbindlist(lapply(models, function(x){
              
              pathdir_irun_model <- paste0( pathdir_irun, 'model_', x, '_history.csv' )
              content <- tail( fread(pathdir_irun_model, header = T) )
              
              data.frame(
                method = paste0('MasterMovelets_',x),
                acc1   = round( as.numeric(content[,3]) * 100, 1),
                acc5   = round( as.numeric(content[,4]) * 100, 1),
                fold   = irun
              )
              })
            )
        }
    )
)

results.masterMovelets.acc.agg <- results.masterMovelets.acc[, .(
            acc1.mean = mean(acc1), acc1.sd = sd(acc1),
            acc5.mean = mean(acc5), acc5.sd = sd(acc5) ),
            by=method]


#    -----------------------------------------------------------------------------------
#    ----------------- BY CLASS --------------------------------------------------------
#    -----------------------------------------------------------------------------------

results.masterMovelets.byClass <- rbindlist(
  lapply( 1:nfolds,
          function(irun){
            pathdir_irun <- paste0(pathdir, "run", irun, "/MasterMovelets/foursquare_ED/mnf_-1__q_LSP__ms_1__Ms_10/model/")
            data.frame (
              rbindlist(lapply(models, function(x){ get.f1.model.byClass(x,pathdir_irun)} )),
              fold = irun
            )
          }
  )
)

results.masterMovelets.byClass.agg <- results.masterMovelets.byClass[, .(
                              f1_scores.mean = mean(f1_score), 
                              precision.mean = mean(precision),
                              recall.mean = mean(recall) ),
                              by=.(class,classifier)]


# --------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------
# ----------------- BITULER ------------------------------------------------------------
# --------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------

models = c("bidirecctionalRNN2")

results.bituler.acc <- rbindlist(
  lapply( 1:nfolds,
          function(irun){
            pathdir_irun <- paste0(pathdir, "run", irun, "/Bituler/model/")
            
            rbindlist(lapply(models, function(x){
              
              pathdir_irun_model <- paste0( pathdir_irun, 'model_', x, '_history.csv' )
              content <- tail( fread(pathdir_irun_model, header = T), 1 )
              
              data.frame(
                method = paste0('Bituler_',x),
                acc1   = round( as.numeric(content[,3]) * 100, 1),
                acc5   = round( as.numeric(content[,4]) * 100, 1),
                fold   = irun
              )
            })
            )
          }
  )
)

results.bituler.acc.agg <- results.bituler.acc[, .(
  acc1.mean = mean(acc1), acc1.sd = sd(acc1),
  acc5.mean = mean(acc5), acc5.sd = sd(acc5) ),
  by=method]


#    -----------------------------------------------------------------------------------
#    ----------------- BY CLASS --------------------------------------------------------
#    -----------------------------------------------------------------------------------

results.bituler.byClass <- rbindlist(
  lapply( 1:nfolds,
          function(irun){
            pathdir_irun <- paste0(pathdir, "run", irun, "/Bituler/model/")
            data.frame (
              rbindlist(lapply(models, function(x){ get.f1.model.byClass(x,pathdir_irun)} )),
              fold = irun
            )
          }
  )
)

results.bituler.byClass.agg <- results.bituler.byClass[, .(
  f1_scores.mean = mean(f1_score), 
  precision.mean = mean(precision),
  recall.mean = mean(recall) ),
  by=.(class,classifier)]


# ------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------

dt <- rbind( results.sim.acc, results.bituler.acc, results.movelets.acc, results.masterMovelets.acc )[,.(method, fold, acc1)]
summary( aov( acc1 ~ method, data=dt) )

dt.wide <- reshape(dt, idvar = c("fold"), timevar = "method", direction = "wide")
dt.wide$fold <- NULL
colnames(dt.wide) <- gsub( "acc1.", "", colnames(dt.wide) )

tukey.test <- tukeyPost(dt.wide, control = ncol(dt.wide) )
print('Tukey Post-Test')
print(tukey.test)

# Error bars represent standard error of the mean
#dt <- rbind( results.masterMovelets.acc.agg, results.sim.acc.agg )
#ggplot(dt, aes(x=method, y=acc1.mean)) + 
#  ylim(0,100) + 
#  theme_minimal() +
#  geom_bar(position=position_dodge(), stat="identity") +
#  geom_errorbar(aes(ymin=acc1.mean-sd(acc1.sd), ymax=acc1.mean+sd(acc1.sd)),
#                width=.2,                    # Width of the error bars
#                position=position_dodge(.9))

dt.agg <- rbind( results.sim.acc.agg, results.bituler.acc.agg, results.movelets.acc.agg, results.masterMovelets.acc.agg )

dt.agg.str <- dt.agg[,.(method, 
          acc1 = paste0(formatC(round(acc1.mean,1), mode = 'character', format = "fg"), 
                        ' (',
                        formatC(round(acc1.sd,1)  , mode = 'character', format = "fg"),
                        ')'),
          acc5 = paste0(formatC(round(acc5.mean,1), mode = 'character', format = "fg"), 
                        ' (',
                        formatC(round(acc5.sd,1)  , mode = 'character', format = "fg"),
                        ')')
          
          ),]

print(dt.agg.str)
fwrite(dt.agg.str, paste0(pathdir,'summary.csv') )

library(xtable)
dt.agg.latex = xtable(transpose(dt.agg.str))
print(dt.agg.latex)




stop()

dt2.models1 <- rbindlist(lapply(models, function(x){ get.f1.model.byClass2(x,pathdir)} ))
dt2.models1$classifier = gsub(pattern = "approach2", "MasterMovelets\nNN", dt2.models1$classifier)
dt2.models1$classifier = gsub(pattern = "approachRF300", "MasterMovelets\nRF", dt2.models1$classifier)




dt2.sims <- rbindlist(lapply(measures, function(x){ get.f1.byClass(x,pathdir)} ))
acc.sims <- rbindlist(lapply(measures, function(x){ get.accuracy.measures(x,pathdir)} ))

# -------------------Models Master Movelets ---------------------------#
pathdir <- "../results/foursquare/KBS2018/E4/foursquare1/MasterMovelets/foursquare1_ED/model/"

models = c("approach2","approachRF300")

dt2.models1 <- rbindlist(lapply(models, function(x){ get.f1.model.byClass2(x,pathdir)} ))
dt2.models1$classifier = gsub(pattern = "approach2", "MasterMovelets\nNN", dt2.models1$classifier)
dt2.models1$classifier = gsub(pattern = "approachRF300", "MasterMovelets\nRF", dt2.models1$classifier)

# -------------------Models Movelets ---------------------------#
pathdir <- "D:/Users/andres/kbs2018/results/KBS2018/E4/foursquare1/Movelets/foursquare1/model/"

models = c("approach2","approachRF300")

dt2.models2 <- rbindlist(lapply(models, function(x){ get.f1.model.byClass2(x,pathdir)} ))
dt2.models2$classifier = gsub(pattern = "approach2", "Movelets\nNN", dt2.models2$classifier)
dt2.models2$classifier = gsub(pattern = "approachRF300", "Movelets\nRF", dt2.models2$classifier)

# -------------------Models TUL ---------------------------#
pathdir <- "D:/Users/andres/workspace/TUL/RNN_Foursquare/data/model/"

models = c("bidirecctionalRNN2")

dt2.models.TUL <- rbindlist(lapply(models, function(x){ get.f1.model.byClass(x,pathdir)} ))

dt2 <- rbind(dt2.sims,dt2.models1,dt2.models2,dt2.models.TUL)[,.(class,classifier,f1_score)]
dt2$f1_score <- round(dt2$f1_score,2)

dt2$classifier <- gsub(pattern = "edr", "EDR", dt2$classifier)
dt2$classifier <- gsub(pattern = "lcss", "LCSS", dt2$classifier)
dt2$classifier <- gsub(pattern = "msm", "MSM", dt2$classifier)
dt2$classifier <- gsub(pattern = "mddtw", "MD-DTW", dt2$classifier)
dt2$classifier <- gsub(pattern = "dtwa", "DTWa", dt2$classifier)
dt2$classifier <- gsub(pattern = "Approach2_Step5", "BiTULER", dt2$classifier)

selected.models <- c("MD-DTW","LCSS","EDR","MSM","BiTULER", "Movelets\nRF","MasterMovelets\nNN", "MasterMovelets\nRF")

dt2 <- dt2[classifier %in% selected.models,]

# ----------------------------------------------------------------------
# RANKING ANALYSIS

dt2.wide <- reshape(dt2[,.(class,classifier,f1_score)], idvar = c("class"), timevar = "classifier", direction = "wide")
colnames(dt2.wide) <- gsub(pattern = "f1_score.", "", colnames(dt2.wide) )

dt.test <- dt2.wide[,-c("class")]
dt.test[is.na(dt.test)] <- 0
ranked.results <- data.table(t(apply(1-dt.test, 1, rank)))

dt3 <- rbindlist(lapply(1:nrow(ranked.results), function(ix){
  
  x <- ranked.results[ix,]
  x1 <- dt.test[ix,]
  
  best.classifiers <- (x == 0) # Para retornar falso
  
  if (min(x) > max(x)){
    best.classifiers <- (x == min(x))
  }
  
  data.table(best.classifiers)
}))


final.result <- table(melt(dt3))
print(final.result)
dt.final.result <- data.table(final.result)[value == T]

dt.final.result$variable <- gsub(pattern = "f1_score.","",x = dt.final.result$variable)

ggplot( data = dt.final.result, aes( x = variable, y= N)) + geom_col() +
  theme_bw() + ylab("Number of best F-measure classes") + xlab("Classification methods")  +
  scale_x_discrete(limits = selected.models) + 
  geom_text(aes(label = N, y = N + 2),position = position_dodge(0.9),vjust = 0, size = 3.5) +
  theme( axis.text.x = element_text(angle = 90, hjust = 1) )
ggsave("kbsBestClassesFoursquare.pdf", width = 5, height = 4)

cat("\n")
cat("####################\n")
cat("Statistical Analysis\n")
library(scmamp)

dt.test2 <- data.frame(dt.test)[,which(colnames(dt.test) %in% selected.models)]

f.test <- friedmanAlignedRanksTest(dt.test2)
print(f.test)

f.posttest <- friedmanAlignedRanksPost(dt.test2)
print(f.posttest)

pdf("kbsFoursquare.pdf", width=13.5, height=4.5, paper='special') 
plotCD(dt.test2, alpha = 0.05, cex = 1.2)
dev.off()

print(t(acc.sims))
#print (t(acc.sims[, .(acc1 = paste0(acc1.median,"(",acc1.IQR,")"), acc5 = paste0(acc5.median,"(",acc5.IQR,")")  ),method ]))

print(friedmanAlignedRanksPost(dt.test2, control = "MasterMovelets.NN"), 2)

print(friedmanAlignedRanksPost(dt.test2, control = "MasterMovelets.RF"), 2)



h <- rbindlist(lapply(1:nrow(dt.test), function(ix){
  
  x <- dt.test[ix,]
  
  best.classifiers <- (x == 0) # Para retornar falso
  
  if (max(x) > 0 ){
    best.classifiers <- (x == max(x))
  }
  
  data.table(best.classifiers)
}))
final.result <- table(melt(dt3))
