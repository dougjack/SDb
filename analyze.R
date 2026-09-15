# Analyze South Delta barriers project simulations
#
# Doug Jackson
# doug@QEDAconsulting.com
library(tidyverse)
library(ggplot2)
library(lubridate)

####################################################################################################
# Constants
####################################################################################################
workingDir <- "C:/Users/dougj/Documents/QEDA/DWR/SouthDeltaBarriers/programs/SDb"

dataDir <- "C:/Users/dougj/Documents/QEDA/DWR/SouthDeltaBarriers/fromXiao/studies_13sep26"

# Number of resamples
N <- 10000

figWidth <- 7
figHeight <- 7
####################################################################################################
# Functions
####################################################################################################
readSalmon <- function(files) {
    datList <- list()
    for(f in files) {
        thisDat <- read.csv(f)
        
        datList[[length(datList)+1]] <- thisDat
    }
    dat <- bind_rows(datList)
    
    return(dat)
}

analyzeSalmon <- function(dF, loc) {
    
    dF <- dF |> mutate(Date=dmy(Date), ptm_start_date=dmy(ptm_start_date), 
                                             first_release_date=dmy(first_release_date), last_release_date=dmy(last_release_date),
                                             scenario=ifelse(scenario=="D-GO-BSL-10yr-b_salmon", "baseline", "preferred"))
    
    dF <- dF |> select(first_release_date, scenario:overall)
    
    vars <- names(dF)[which(!(names(dF) %in% c("first_release_date", "scenario")))]
    
    for(var in vars) {
        
        thisDF <- dF[, c("first_release_date", "scenario", var)]
        names(thisDF) <- c("first_release_date", "scenario", "var")
        
        thisDFwide <- thisDF |> pivot_wider(id_cols=first_release_date, names_from=scenario, values_from=var) |> 
            mutate(year=as.factor(year(first_release_date)), month=as.factor(month(first_release_date)), julianDay=yday(first_release_date),
                   diff=preferred-baseline)
        
        varType <- ifelse(grepl("frac", var), "routing", "survival")
        
        thisOutputDir <- file.path(outputDir, loc, varType)
        dir.create(thisOutputDir, recursive=T, showWarnings=F)
        
        p <- ggplot(thisDF) + geom_line(aes(x=first_release_date, y=var)) +
            facet_wrap(~scenario, ncol=1) +
            labs(title=paste0(var, ", ", loc), x="", y=varType) +
            theme_light()
        ggsave(file.path(thisOutputDir, paste0("timeSeries_", loc, "_", var, ".png")), width=figWidth, height=figHeight)
        
        p <- ggplot(thisDFwide) + geom_point(aes(x=julianDay, y=diff, color=month), size=0.25) +
            facet_wrap(~year, ncol=1, scales="free_y") +
            labs(title=paste0("Difference in ", var, ", ", loc), x="Julian day", y=paste(varType, "difference (preferred - baseline)")) +
            guides(color=guide_legend(override.aes=list(size=2))) +
            theme_light()
        ggsave(file.path(thisOutputDir, paste0("diff_", loc, "_", var, ".png")), width=figWidth, height=figHeight)
        
        p <- ggplot(thisDFwide) + geom_point(aes(x=baseline, y=preferred, group=month, color=month), alpha=0.25) +
            geom_abline(intercept=0, slope=1, color="red") +
            facet_wrap(~month, ncol=1) +
            labs(title=paste(var, "comparison,", loc), x="survival, baseline scenario", y=paste0(varType, ", preferred alternative")) +
            theme_light()
        ggsave(file.path(thisOutputDir, paste0("compare_", loc, "_", var, ".png")), width=figWidth, height=figHeight)
        
        ####################################################################################################
        # Boxplots with significance
        # From Wikipedia: "The Wilcoxon test is a good alternative to the t-test when the normal distribution of the 
        # differences between paired individuals cannot be assumed. Instead, it assumes a weaker hypothesis that the 
        # distribution of this difference is symmetric around a central value and it aims to test whether this center 
        # value differs significantly from zero."
        sigs <- thisDFwide |> group_by(month) |> filter(n_distinct(diff)>1) |> 
            summarize(pValResampleMean=resampleDiff(diff),
                      pValResampleMed=resampleMedDiff(diff),
                      pValPairedT=t.test(preferred, baseline, paired=T, alternative="two.sided")$p.value,
                      pValShapiro=shapiro.test(diff)$p.value,
                      pWilcox = wilcox.test(baseline, preferred, paired=T)$p.value)
        
        sigMonths <- sigs |> filter(pWilcox<0.05)
        sigMonths <- sigMonths$month
        
        p <- ggplot(thisDFwide) + geom_boxplot(aes(x=month, y=diff)) + 
            annotate("text", x=sigMonths, y=max(thisDFwide$diff)*1.05, label="*", size=8, color="blue") +
            labs(title=paste(var, "comparison,", loc), x="month", y=paste(varType, "difference (preferred - baseline)")) +
            theme_light()
        ggsave(file.path(thisOutputDir, paste0("boxPlot_", loc, "_", var, ".png")), width=figWidth, height=figHeight)
        cat("--------------------------------------------------------------------\n")
        cat(paste0(var, ", ", loc, "\n"))
        print(sigs)
        
    }
    
    return(list(dF=dF))
}

resampleDiff <- function(diffs) {
    
    origDiffs <- diffs
    numDiffs <- length(diffs)
    
    meanDiffs <- as.numeric(N)
    for(j in 1:N) {
        thisDiffs <- origDiffs * sample(c(-1, 1), numDiffs, replace=T)
        meanDiffs[j] <- mean(thisDiffs, na.rm=T)
    }
    nVal <- sum(abs(meanDiffs)>abs(mean(origDiffs, na.rm=T)))/N
    
    return(nVal)
}

resampleMedDiff <- function(diffs) {
    
    origDiffs <- diffs
    numDiffs <- length(diffs)
    
    medDiffs <- as.numeric(N)
    for(j in 1:N) {
        thisDiffs <- origDiffs * sample(c(-1, 1), numDiffs, replace=T)
        medDiffs[j] <- median(thisDiffs, na.rm=T)
    }
    nVal <- sum(abs(medDiffs)>abs(median(origDiffs, na.rm=T)))/N
    
    return(nVal)
}
####################################################################################################
# Run
####################################################################################################
setwd(workingDir)

outputDir <- file.path(workingDir, "output_analyze")
dir.create(outputDir, showWarnings=F)

northDeltaFiles <- list.files(dataDir, pattern="salmon_nd*", full.names=T)
southDeltaFiles <- list.files(dataDir, pattern="salmon_sd*", full.names=T)
npFiles <- list.files(dataDir, pattern="np_*", full.names=T)
spFiles <- list.files(dataDir, pattern="sp_*", full.names=T)

northDelta <- readSalmon(northDeltaFiles)
out <- analyzeSalmon(northDelta, "northDelta")

southDelta <- readSalmon(southDeltaFiles)
out <- analyzeSalmon(southDelta, "southDelta")
