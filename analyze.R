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
    
    p <- ggplot(dF) + geom_line(aes(x=first_release_date, y=overall)) +
        facet_wrap(~scenario, ncol=1) +
        labs(title=paste("Through-Delta survival,", loc), x="", y="survival") +
        theme_light()
    ggsave(file.path(outputDir, paste0("timeSeriesSurv_", loc, ".png")), width=figWidth, height=figHeight)
    
    dFsurv <- dF |> select(first_release_date, scenario, overall) |> 
        pivot_wider(id_cols=first_release_date, names_from=scenario, values_from=overall) |> 
        mutate(month=as.factor(month(first_release_date)), diff=preferred-baseline)
    
    p <- ggplot(dFsurv) + geom_point(aes(x=first_release_date, y=diff, color=month), size=0.25) +
        ylim(-1, 1) +
        labs(title=paste("Difference in through-Delta survival,", loc), x="", y="survival difference (preferred - baseline)") +
        guides(color=guide_legend(override.aes=list(size=2))) +
        theme_light()
    ggsave(file.path(outputDir, paste0("diffSurv_", loc, ".png")), width=figWidth, height=figHeight)
    
    p <- ggplot(dFsurv) + geom_point(aes(x=baseline, y=preferred, group=month, color=month), alpha=0.25) +
        geom_abline(intercept=0, slope=1, color="red") +
        facet_wrap(~month, ncol=1) +
        labs(title=paste("Through-Delta survival comparison,", loc), x="survival, baseline scenario", y="survival, preferred alternative") +
        theme_light()
    ggsave(file.path(outputDir, paste0("compareSurv_", loc, ".png")), width=figWidth, height=figHeight)
    
    ####################################################################################################
    # Boxplots with significance
    # From Wikipedia: "The Wilcoxon test is a good alternative to the t-test when the normal distribution of the 
    # differences between paired individuals cannot be assumed. Instead, it assumes a weaker hypothesis that the 
    # distribution of this difference is symmetric around a central value and it aims to test whether this center 
    # value differs significantly from zero."
    sigs <- dFsurv |> group_by(month) |> summarize(pValResampleMean=resampleDiff(diff),
                                                   pValResampleMed=resampleMedDiff(diff),
                                                   pValPairedT=t.test(preferred, baseline, paired=T, alternative="two.sided")$p.value,
                                                   pValShapiro=shapiro.test(diff)$p.value,
                                                   pWilcox = wilcox.test(baseline, preferred, paired=T)$p.value)
    
    sigMonths <- sigs |> filter(pWilcox<0.05)
    sigMonths <- sigMonths$month
    
    p <- ggplot(dFsurv) + geom_boxplot(aes(x=month, y=diff)) + 
        annotate("text", x=sigMonths, y=max(dFsurv$diff)*1.05, label="*", size=8, color="blue") +
        labs(title=paste("Through-Delta survival comparison,", loc), x="month", y="survival difference (preferred - baseline)") +
        theme_light()
    ggsave(file.path(outputDir, paste0("boxPlotSurv_", loc, ".png")), width=figWidth, height=figHeight)

    cat("--------------------------------------------------------------------\n")
    cat(paste0(loc, "\n"))
    print(sigs)
    
    return(list(dF=dF, dFsurv=dFsurv))
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
