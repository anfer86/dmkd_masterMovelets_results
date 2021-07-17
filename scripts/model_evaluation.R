library(data.table)
library(ggplot2)
library(caret)
library(scmamp)

source('helper.r')

# --------------------------------------------------------------------------------------
# DEFINE THE DATASET NAME
# OPTIONS: foursquare gowalla or brightkite
dataset <- "brightkite"

pathdir <- paste0("../results/", dataset ,"/")

# --------------------------------------------------------------------------------------

nfolds = 5

# --------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------
# ----------------- SIMILARITY MEASURES ------------------------------------------------
# --------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------

measures <- c("mddtw","lcss","edr","msm")
  
#    -----------------------------------------------------------------------------------
#    ----------------- ACCURACY --------------------------------------------------------
#    -----------------------------------------------------------------------------------
results.sim.acc <- rbindlist(
  lapply( 1:nfolds,
    function(irun){
      pathdir_irun <- paste0(pathdir, "run", irun, "/Similarity/", dataset, "/")
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
            pathdir_irun <- paste0(pathdir, "run", irun, "/Similarity/", dataset, "/")
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
            pathdir_irun <- paste0(pathdir, "run", irun, "/Movelets/", dataset, "_movelets/p_false__q_LSP__ms_1__Ms_10/model/")
            
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
            pathdir_irun <- paste0(pathdir, "run", irun, "/Movelets/", dataset, "_movelets/p_false__q_LSP__ms_1__Ms_10/model/")
            data.frame (
              rbindlist(lapply(models, function(x){ 
                get.f1.model.byClass(x,pathdir_irun)[,classifier := paste0('Movelets_',x) ,]
                })),
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

models = c("approach2","approachRF300")

results.masterMovelets.acc <- rbindlist(
  lapply( 1:nfolds,
          function(irun){
            pathdir_irun <- paste0(pathdir, "run", irun, "/MasterMovelets/", dataset, "_ED/mnf_-1__q_LSP__ms_1__Ms_10/model/")
            
            rbindlist(lapply(models, function(x){
              
              pathdir_irun_model <- paste0( pathdir_irun, 'model_', x, '_history.csv' )
              content <- tail( fread(pathdir_irun_model, header = T), 1 )
              
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
            pathdir_irun <- paste0(pathdir, "run", irun, "/MasterMovelets/", dataset, "_ED/mnf_-1__q_LSP__ms_1__Ms_10/model/")
            data.frame (
              rbindlist(lapply(models, function(x){ 
                get.f1.model.byClass(x,pathdir_irun)[,classifier := paste0('MasterMovelets_',x) ,]
                })),
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
              rbindlist(lapply(models, function(x){ 
                get.f1.model.byClass(x,pathdir_irun)[,classifier := paste0('Bituler_',x) ,]
                })),
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



# -------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------
# ---------------- PLOTS --------------------------------------------------------------
# -------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------
dt2 <- results.byClass.agg <- rbind(
  results.sim.byClass.agg,
  results.bituler.byClass.agg,
  results.movelets.byClass.agg,
  results.masterMovelets.byClass.agg)[,.(class,classifier,f1_score=f1_scores.mean)]

dt2$f1_score <- round(dt2$f1_score,4)

dt2$classifier <- gsub(pattern = "edr", "EDR", dt2$classifier)
dt2$classifier <- gsub(pattern = "lcss", "LCSS", dt2$classifier)
dt2$classifier <- gsub(pattern = "msm", "MSM", dt2$classifier)
dt2$classifier <- gsub(pattern = "mddtw", "MD-DTW", dt2$classifier)
dt2$classifier <- gsub(pattern = "dtwa", "DTWa", dt2$classifier)
dt2$classifier <- gsub(pattern = "Bituler_bidirecctionalRNN2", "BiTULER", dt2$classifier)
dt2$classifier <- gsub(pattern = "Movelets_approachRF300", "Movelets\nRF", dt2$classifier)
dt2$classifier <- gsub(pattern = "MasterMovelets_approach2", "MasterMovelets\nNN", dt2$classifier)
dt2$classifier <- gsub(pattern = "MasterMovelets_approachRF300", "MasterMovelets\nRF", dt2$classifier)

selected.models <- c("MD-DTW","LCSS","EDR","MSM","BiTULER", "Movelets\nRF","MasterMovelets\nNN", "MasterMovelets\nRF")
selected.models <- c("MD-DTW","LCSS","EDR","MSM","BiTULER", "Movelets\nRF","MasterMovelets\nNN")

dt2 <- dt2[classifier %in% selected.models,]

ggplot( data = dt2, aes( x = classifier, y= f1_score)) + geom_boxplot() +
  theme_bw() + ylab("Number of best F-measure classes") + xlab("Classification methods")  +
  scale_x_discrete(limits = selected.models) + 
  #geom_text(aes(label = , y = N + 2),position = position_dodge(0.9),vjust = 0, size = 3.5) +
  theme( axis.text.x = element_text(angle = 90, hjust = 1) )

# ----------------------------------------------------------------------

dt.final.result <- get.best.models.by.class(dt2)

ggplot( data = dt.final.result, aes( x = variable, y= N)) + geom_col() +
  theme_bw() + ylab("Number of best F-measure classes") + xlab("Classification methods")  +
  scale_x_discrete(limits = selected.models) + 
  geom_text(aes(label = N, y = N + 2),position = position_dodge(0.9),vjust = 0, size = 3.5) +
  theme( axis.text.x = element_text(angle = 90, hjust = 1) )

ggsave( paste0("best_models_byclass_", dataset, ".pdf"), width = 5, height = 4)

# ----------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------
