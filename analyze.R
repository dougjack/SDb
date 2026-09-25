# Analyze South Delta barriers project simulations
#
# Doug Jackson
# doug@QEDAconsulting.com
library(tidyverse)
library(ggplot2)
library(lubridate)
library(readxl)
library(slider)
####################################################################################################
# Constants
####################################################################################################
workingDir <- "C:/Users/dougj/Documents/QEDA/DWR/SouthDeltaBarriers/programs/SDb"

dataDir <- "C:/Users/dougj/Documents/QEDA/DWR/SouthDeltaBarriers/fromXiao/studies_22sep26"

insertionLocsFile = "C:/Users/dougj/Documents/QEDA/DWR/SouthDeltaBarriers/programs/SDb/insertionLocs.csv"

# South Delta flows
baselineFlowFile <- "C:/Users/dougj/Documents/QEDA/DWR/SouthDeltaBarriers/fromXiao/studies_22sep26/hydro/D-GO-BSL-10yr-b/D-GO-BSL-10yr-b_flow.xlsx"
preferredFlowFile <- "C:/Users/dougj/Documents/QEDA/DWR/SouthDeltaBarriers/fromXiao/studies_22sep26/hydro/D-GO-9b/D-GO-9b_flow.xlsx"

# Number of resamples
N <- 10000

# Time step used in flows file
tidefileIncrement_min <- 15

rollingMeanDays <- 30

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
            mutate(year=as.factor(year(first_release_date)), month=as.factor(month(first_release_date, label=T)), julianDay=yday(first_release_date),
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
            labs(title=paste(var, "comparison,", loc), x=paste0(varType, ", baseline scenario"), y=paste0(varType, ", preferred alternative")) +
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
        
        p <- ggplot(thisDFwide) + geom_boxplot(aes(x=month, y=diff, fill=month)) + 
            scale_fill_brewer(palette="Paired") +
            annotate("text", x=sigMonths, y=max(thisDFwide$diff)*1.05, label="*", size=8, color="blue") +
            labs(title=paste(var, "comparison,", loc), x="", y=paste(varType, "difference (preferred - baseline)")) +
            theme_light()
        ggsave(file.path(thisOutputDir, paste0("boxPlot_", loc, "_", var, ".png")), width=figWidth, height=figHeight)
        cat("--------------------------------------------------------------------\n")
        cat(paste0(var, ", ", loc, "\n"))
        print(sigs)
        
        # Boxplots by year
        p <- ggplot(thisDFwide) + geom_boxplot(aes(x=month, y=diff, fill=month)) + 
            facet_wrap(~year, ncol=3) +
            scale_fill_brewer(palette="Paired") +
            labs(title=paste(var, "comparison,", loc), x="", y=paste(varType, "difference (preferred - baseline)")) +
            theme_light()
        ggsave(file.path(thisOutputDir, paste0("boxPlot_byYear_", loc, "_", var, ".png")), width=10, height=9)
        
    }
    
    return(list(dF=dF))
}

readParticles <- function(files) {
    datList <- list()
    for(f in files) {
        thisScen <- gsub(".dat", "", basename(f))
        if(thisScen=="np_90day_D-GO-sdg9b") {
            thisDat <- read.table(f, skip=1, sep="\t", header=T)
        } else {
            thisDat <- read.csv(f, skip=1, header=T)
        }
        
        thisDat$scenario <- thisScen
        thisDat$startDate <- dmy(thisDat$SimPeriod)
        
        datList[[length(datList)+1]] <- thisDat
    }
    dat <- bind_rows(datList)
}

analyzeParticles <- function(dF, type) {
    
    yLab <- "90-day flux"
    
    dF <- dF |> mutate(scenario=ifelse(scenario %in% c("np_90day_D-GO-BSL-10yr-b", "sp_90day_D-GO-BSL-10yr-b"), "baseline", "preferred")) |> 
        select(startDate, SimLoc:scenario)
    
    vars <- names(dF)[which(!(names(dF) %in% c("startDate", "SimLoc", "scenario")))]
    
    locs <- unique(dF$SimLoc)
    
    typeStr <- ifelse(type=="neutrallyBuoyant", "neutrally buoyant", "surface-oriented")
    
    for(var in vars) {
        
        thisOutputDir <- file.path(outputDir, type, var)
        dir.create(thisOutputDir, recursive=T, showWarnings=F)
        
        for(releaseLoc in locs) {
            thisDF <- dF |> filter(SimLoc==releaseLoc)
            thisDF <- thisDF[, c("startDate", "scenario", var)]
            names(thisDF) <- c("startDate", "scenario", "var")
            
            if(sum(!is.na(thisDF$var))==0 | sum(thisDF$var!=0)==0) {next}
            
            thisInsertionLoc <- insertionLocs |> filter(loc==releaseLoc)
            thisInsertionNode <- thisInsertionLoc$node
            thisInsertionDesc <- thisInsertionLoc$name
            
            thisDFwide <- thisDF |> pivot_wider(id_cols=startDate, names_from=scenario, values_from=var) |> 
                mutate(year=as.factor(year(startDate)), month=as.factor(month(startDate)), julianDay=yday(startDate),
                       diff=preferred-baseline)
            
            p <- ggplot(thisDF) + geom_point(aes(x=startDate, y=var)) +
                facet_wrap(~scenario, ncol=1) +
                labs(title=paste0(var, ", release node ", thisInsertionNode, ", ", typeStr), subtitle=thisInsertionDesc,
                     x="", y=yLab) +
                theme_light()
            ggsave(file.path(thisOutputDir, paste0("timeSeries_", releaseLoc, "_", var, ".png")), width=figWidth, height=figHeight)
            
            p <- ggplot(thisDFwide) + geom_point(aes(x=year, y=diff, color=month), size=2) +
                facet_wrap(~month, ncol=1, scales="free_y") +
                labs(title=paste0("Difference in ", var, ", release node ", thisInsertionNode, ", ", typeStr), subtitle=thisInsertionDesc,
                     x="Julian day", y=paste("90-day flux difference (preferred - baseline)")) +
                guides(color=guide_legend(override.aes=list(size=2))) +
                theme_light()
            ggsave(file.path(thisOutputDir, paste0("diff_", releaseLoc, "_", var, ".png")), width=figWidth, height=figHeight)
            
            p <- ggplot(thisDFwide) + geom_point(aes(x=baseline, y=preferred, group=month, color=month), size=2, alpha=1) +
                geom_abline(intercept=0, slope=1, color="red") +
                facet_wrap(~month, ncol=1) +
                labs(title=paste(var, "comparison, release node", thisInsertionNode, ", ", typeStr), subtitle=thisInsertionDesc,
                     x="90-day flux, baseline scenario", y=paste0("90-day flux, preferred alternative")) +
                theme_light()
            ggsave(file.path(thisOutputDir, paste0("compare_", releaseLoc, "_", var, ".png")), width=figWidth, height=figHeight)

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
                labs(title=paste(var, "comparison, release node", thisInsertionNode, ", ", typeStr), subtitle=thisInsertionDesc,
                     x="month", y=paste("90-day flux difference (preferred - baseline)")) +
                theme_light()
            ggsave(file.path(thisOutputDir, paste0("boxPlot_", releaseLoc, "_", var, ".png")), width=figWidth, height=figHeight)
            cat("--------------------------------------------------------------------\n")
            cat(paste0(var, ", ", releaseLoc, "\n"))
            print(sigs)
        }
        
        # Plot all release locations together
        thisDF <- dF[, c("startDate", "SimLoc", "scenario", var)]
        names(thisDF) <- c("startDate", "SimLoc", "scenario", "var")
        thisDF <- left_join(thisDF, insertionLocs, by=c("SimLoc"="loc"))
        
        if(sum(!is.na(thisDF$var))==0 | sum(thisDF$var!=0)==0) {next}
        
        thisDFwide <- thisDF |> pivot_wider(id_cols=c("startDate", "SimLoc", "node", "name"), names_from=scenario, values_from=var) |> 
            mutate(month=as.factor(month(startDate, label=T)), diff=preferred-baseline, node=as.factor(node))
        
        sigs <- thisDFwide |> group_by(node) |> filter(n_distinct(diff)>1) |> 
            summarize(pValResampleMean=resampleDiff(diff),
                      pValResampleMed=resampleMedDiff(diff),
                      pValPairedT=t.test(preferred, baseline, paired=T, alternative="two.sided")$p.value,
                      pValShapiro=shapiro.test(diff)$p.value,
                      pWilcox = wilcox.test(baseline, preferred, paired=T)$p.value)
        
        sigNodes <- sigs |> filter(pWilcox<0.05)
        sigNodes <- sigNodes$node
        
        p <- ggplot(thisDFwide) + geom_boxplot(aes(x=node, y=diff)) +
            annotate("text", x=sigNodes, y=max(thisDFwide$diff)*1.05, label="*", size=8, color="blue") +
            labs(title=paste(var, "comparison,", typeStr), x="release node", y="90-day flux difference (preferred - baseline)") +
            theme_light()
        ggsave(file.path(thisOutputDir, paste0("boxPlot_", var, ".png")), width=figWidth, height=figHeight)
        
        sigs <- thisDFwide |> group_by(node, month) |> filter(n_distinct(diff)>1) |> 
            summarize(pValResampleMean=resampleDiff(diff),
                      pValResampleMed=resampleMedDiff(diff),
                      pValPairedT=t.test(preferred, baseline, paired=T, alternative="two.sided")$p.value,
                      pValShapiro=shapiro.test(diff)$p.value,
                      pWilcox = wilcox.test(baseline, preferred, paired=T)$p.value, 
                      .groups="drop")
        
        sigs <- sigs |> filter(pWilcox<0.05)
        
        p <- ggplot(thisDFwide) + geom_boxplot(aes(x=node, y=diff)) +
            facet_wrap(~month, ncol=1) +
            geom_text(data=sigs, aes(x=node, y=max(thisDFwide$diff)*0.9, label="*"), size=8, color="blue") +
            labs(title=paste(var, "comparison,", typeStr), x="release node", y="90-day flux difference (preferred - baseline)") +
            theme_light()
        ggsave(file.path(thisOutputDir, paste0("boxPlot_byMonth_", var, ".png")), width=6, height=8)
        
        p <- ggplot(thisDFwide) + geom_point(aes(x=baseline, y=preferred, color=node, group=node)) + 
            geom_abline(slope=1, intercept=0, color="red") + 
            #xlim(0, 100) + ylim(0, 100) +
            facet_wrap(~node, ncol=1) +
            labs(title=paste(var, "comparison, ", typeStr), subtitle=thisInsertionDesc,
                 x="90-day flux, baseline scenario", y=paste0("90-day flux, preferred alternative")) +
            theme_light()
        ggsave(file.path(thisOutputDir, paste0("compare_", var, ".png")), width=figWidth, height=figHeight)
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

####################################################################################################
# salmon
northDeltaFiles <- list.files(dataDir, pattern="salmon_nd*", full.names=T)
southDeltaFiles <- list.files(dataDir, pattern="salmon_sd*", full.names=T)

northDelta <- readSalmon(northDeltaFiles)
out <- analyzeSalmon(northDelta, "northDelta")

southDelta <- readSalmon(southDeltaFiles)
out <- analyzeSalmon(southDelta, "southDelta")

# Calculate averages by location to verify that the correct data were copied to Excel spreadsheets
northDeltaLong <- northDelta |> pivot_longer(cols=c(-Date, -ptm_start_date, -first_release_date, -last_release_date, -scenario), names_to="var", values_to="val")
northDeltaAvg <- northDeltaLong |> select(Date, scenario, var, val) |> group_by(scenario, var) |> summarize(meanVal=mean(val), .groups="drop")

southDeltaLong <- southDelta |> pivot_longer(cols=c(-Date, -ptm_start_date, -first_release_date, -last_release_date, -scenario), names_to="var", values_to="val")
southDeltaAvg <- southDeltaLong |> select(Date, scenario, var, val) |> group_by(scenario, var) |> summarize(meanVal=mean(val), .groups="drop")

####################################################################################################
# neutrally buoyant and surface-oriented particles
insertionLocs <- read.csv(insertionLocsFile)

npFiles <- list.files(dataDir, pattern="np_*", full.names=T)
spFiles <- list.files(dataDir, pattern="sp_*", full.names=T)

np <- readParticles(npFiles)
out <- analyzeParticles(np, "neutrallyBuoyant")

sp <- readParticles(spFiles)
out <- analyzeParticles(sp, "surfaceOriented")

# Calculate averages by location to verify that the correct data were copied to Excel spreadsheets
npLong <- np |> pivot_longer(cols=c(-SimPeriod, -SimLoc, -scenario, -startDate), names_to="loc", values_to="flux")
npAvg <- npLong |> select(scenario, loc, flux) |> group_by(scenario, loc) |> summarize(meanFlux=mean(flux), .groups="drop")

spLong <- sp |> pivot_longer(cols=c(-SimPeriod, -SimLoc, -scenario, -startDate), names_to="loc", values_to="flux")
spAvg <- spLong |> select(scenario, loc, flux) |> group_by(scenario, loc) |> summarize(meanFlux=mean(flux), .groups="drop")

####################################################################################################
# Analyze South Delta flow
baselineFlowHeaders <- read_xlsx(baselineFlowFile, skip=1, n_max=1, col_names=F) |> as.character()
baselineFlow <- read_xlsx(baselineFlowFile, skip=7, col_names=baselineFlowHeaders)
names(baselineFlow)[2] <- "datetime"
baselineFlow <- baselineFlow |> mutate(scenario="baseline",
                                       outflow_OH1=pmax(0, OH1), outflow_SJL=pmax(0, SJL), fracOutflow_SJL=outflow_SJL/(outflow_SJL + outflow_OH1))
baselineFlowFrac <- baselineFlow |> select(datetime, fracOutflow_SJL)

preferredFlowHeaders <- read_xlsx(preferredFlowFile, skip=1, n_max=1, col_names=F) |> as.character()
preferredFlow <- read_xlsx(preferredFlowFile, skip=7, col_names=preferredFlowHeaders)
names(preferredFlow)[2] <- "datetime"
preferredFlow <- preferredFlow |> mutate(scenario="preferred", 
                                         outflow_OH1=pmax(0, OH1), outflow_SJL=pmax(0, SJL), fracOutflow_SJL=outflow_SJL/(outflow_SJL + outflow_OH1))
preferredFlowFrac <- preferredFlow |> select(datetime, fracOutflow_SJL)

flowFrac <- full_join(baselineFlowFrac, preferredFlowFrac, by="datetime", suffix=c("_baseline", "_preferred"))

flowFrac <- flowFrac |> mutate(year=year(datetime), 
                               diff=fracOutflow_SJL_preferred - fracOutflow_SJL_baseline, month=as.factor(month(datetime, label=T)))

p <- ggplot(flowFrac) + geom_point(aes(x=fracOutflow_SJL_baseline, y=fracOutflow_SJL_preferred), alpha=0.25) +
    geom_abline(intercept=0, slope=1, color="red") +
    #facet_wrap(~month, ncol=1) +
    facet_grid(month~year) +
    theme_light()
ggsave(file.path(outputDir, "compareFlowFrac.png"), width=15, height=15)

p <- ggplot(flowFrac) + geom_boxplot(aes(x=month, y=diff)) + 
    #annotate("text", x=sigMonths, y=max(thisDFwide$diff)*1.05, label="*", size=8, color="blue") +
    #labs(title=paste(var, "comparison, release node", thisInsertionNode, ", ", typeStr), subtitle=thisInsertionDesc,
    #     x="month", y=paste("90-day flux difference (preferred - baseline)")) +
    theme_light()
ggsave(file.path(outputDir, "boxPlotFlowFrac.png"), width=figWidth, height=figHeight)

# Calculate x-day rolling means of flow fractions
samplesBefore <- 0
samplesAfter <- round((60/tidefileIncrement_min*24*rollingMeanDays))

flowFrac <- flowFrac |> mutate(meanFracOutflow_SJL_baseline=slide_dbl(fracOutflow_SJL_baseline, mean, na.rm=T, .before=samplesBefore, .after=samplesAfter),
                               meanFracOutflow_SJL_preferred=slide_dbl(fracOutflow_SJL_preferred, mean, na.rm=T, .before=samplesBefore, .after=samplesAfter))

# Compare frac_SJL_junction routing to flows
dF <- southDelta |> mutate(Date=dmy(Date), ptm_start_date=dmy(ptm_start_date), 
                   first_release_date=dmy(first_release_date), last_release_date=dmy(last_release_date),
                   scenario=ifelse(scenario=="D-GO-BSL-10yr-b_salmon", "baseline", "preferred"))

dF <- dF |> select(first_release_date, scenario:overall)

var <- "frac_SJR_junction"
thisDF <- dF[, c("first_release_date", "scenario", var)]
names(thisDF) <- c("first_release_date", "scenario", "var")

thisDFwide <- thisDF |> pivot_wider(id_cols=first_release_date, names_from=scenario, values_from=var) |> 
    mutate(year=as.factor(year(first_release_date)), month=as.factor(month(first_release_date, label=T)), julianDay=yday(first_release_date),
           diff=preferred-baseline)

fracSJR <- left_join(thisDFwide, flowFrac, by=c("first_release_date"="datetime")) |> 
    mutate(frac_SJR_baseline=baseline, frac_SJR_preferred=preferred, year=year.x, month=month.x) |> 
    select(first_release_date, year, month, frac_SJR_baseline, frac_SJR_preferred, meanFracOutflow_SJL_baseline, meanFracOutflow_SJL_preferred)

fracSJR_baseline <- fracSJR |> select(first_release_date, year, month, frac_SJR_baseline, meanFracOutflow_SJL_baseline) |> mutate(scenario="baseline")
names(fracSJR_baseline) <- c("first_release_date", "year", "month", "frac_SJR", "meanFracOutflow_SJL", "scenario")
fracSJR_preferred <- fracSJR |> select(first_release_date, year, month, frac_SJR_preferred, meanFracOutflow_SJL_preferred) |> mutate(scenario="preferred")
names(fracSJR_preferred) <- c("first_release_date", "year", "month", "frac_SJR", "meanFracOutflow_SJL", "scenario")

fracSJRlong <- bind_rows(fracSJR_baseline, fracSJR_preferred)

p <- ggplot(fracSJRlong, aes(x=meanFracOutflow_SJL, y=frac_SJR)) + geom_point(aes(color=month, group=month), alpha=0.75) +
    geom_smooth(linewidth=0.75, alpha=0.1, method="loess", formula="y~x", color="black", fill="black") +
    scale_color_brewer(palette="Paired") +
    labs(x="30-day rolling mean of SJL outflow fraction", y="SJR junction routing fraction") +
    theme_light()
ggsave(file.path(outputDir, "fracSJR_vs_fracSJL.png"), width=figWidth, height=figHeight)

p <- ggplot(fracSJRlong, aes(x=meanFracOutflow_SJL, y=frac_SJR)) + geom_point(aes(color=month, group=month), alpha=0.75) +
    facet_wrap(~scenario, ncol=1) +
    geom_smooth(linewidth=0.75, alpha=0.1, method="loess", formula="y~x", color="black", fill="black") +
    #guides(color=guide_legend(override.aes=list(alpha=1, size=3))) +
    scale_color_brewer(palette="Paired") +
    labs(x="30-day rolling mean of SJL outflow fraction", y="SJR junction routing fraction") +
    theme_light()
ggsave(file.path(outputDir, "fracSJR_vs_fracSJL_byScenario.png"), width=figWidth, height=figHeight)

p <- ggplot(fracSJRlong, aes(x=meanFracOutflow_SJL, y=frac_SJR, color=scenario, fill=scenario)) + geom_point(alpha=0.25) +
    facet_wrap(~month, ncol=5) +
    geom_smooth(linewidth=0.75, alpha=0.1, method="loess", formula="y~x") +
    scale_color_brewer(palette="Dark2") +
    labs(x="30-day rolling mean of SJL outflow fraction", y="SJR junction routing fraction") +
    theme_light()
ggsave(file.path(outputDir, "fracSJR_vs_fracSJL_byMonth.png"), width=12, height=6)

meanFracSJRbyYear <- fracSJRlong |> group_by(year, month, scenario) |> 
    summarize(meanFracSJR=mean(frac_SJR, na.rm=T), meanFracOutflow_SJL=mean(meanFracOutflow_SJL, na.rm=T), 
              medianFracSJR=median(frac_SJR, na.rm=T), medianFracOutflow_SJL=median(meanFracOutflow_SJL, na.rm=T),
              .groups="drop")

p <- ggplot(meanFracSJRbyYear) + geom_line(aes(x=meanFracOutflow_SJL, y=meanFracSJR, color=year, group=year)) +
    geom_point(aes(x=meanFracOutflow_SJL, y=meanFracSJR, color=year, shape=scenario, group=year), size=2) +
    facet_wrap(~month, ncol=5) +
    theme_light()
ggsave(file.path(outputDir, "meanfracSJR_vs_fracSJL_byYear.png"), width=figWidth, height=figHeight)

p <- ggplot(meanFracSJRbyYear) + geom_line(aes(x=medianFracOutflow_SJL, y=medianFracSJR, color=year, group=year)) +
    geom_point(aes(x=medianFracOutflow_SJL, y=medianFracSJR, color=year, shape=scenario, group=year), size=2) +
    facet_wrap(~month, ncol=5) +
    theme_light()
ggsave(file.path(outputDir, "medianfracSJR_vs_fracSJL_byYear.png"), width=figWidth, height=figHeight)

meanFracSJRbyMonth <- fracSJRlong |> group_by(month, scenario) |> 
    summarize(meanFracSJR=mean(frac_SJR, na.rm=T), meanFracOutflow_SJL=mean(meanFracOutflow_SJL, na.rm=T), .groups="drop")

p <- ggplot(meanFracSJRbyMonth) + geom_line(aes(x=meanFracOutflow_SJL, y=meanFracSJR, color=month, group=month)) +
    geom_point(aes(x=meanFracOutflow_SJL, y=meanFracSJR, shape=scenario, group=month)) +
    theme_light()
ggsave(file.path(outputDir, "fracSJR_vs_fracSJL_byMonth.png"), width=figWidth, height=figHeight)

# # Calculate difference in median flow fraction by month
# diffMedFlowFrac <- meanFracSJRbyYear |> select(year, month, scenario, medianFracOutflow_SJL) |> 
#     pivot_wider(id_cols=c(year, month), names_from=scenario, values_from=medianFracOutflow_SJL) |> 
#     mutate(diff=preferred-baseline)
# 
# p <- ggplot(diffMedFlowFrac) + geom_boxplot(aes(x=month, y=diff)) +
#     labs(x="", y="difference in median outflow fraction (preferred - baseline)") +
#     theme_light()
# ggsave(file.path(outputDir, "diffOutflowFrac.png"), width=figWidth, height=figHeight)

fracSJR <- fracSJR |> mutate(diffFracSJR=frac_SJR_preferred - frac_SJR_baseline,
                             diffFracOutflow=meanFracOutflow_SJL_preferred - meanFracOutflow_SJL_baseline)

p <- ggplot(fracSJR) + geom_boxplot(aes(x=month, y=diffFracOutflow)) +
    labs(x="", y="difference in outflow fraction (preferred - baseline)") +
    theme_light()
ggsave(file.path(outputDir, "diffOutflowFrac.png"), width=figWidth, height=figHeight)

p <- ggplot(fracSJR) + geom_boxplot(aes(x=month, y=diffFracOutflow, fill=month)) +
    facet_wrap(~year, ncol=3) + 
    scale_fill_brewer(palette="Paired") +
    labs(x="", y="difference in outflow fraction (preferred - baseline)") +
    theme_light()
ggsave(file.path(outputDir, "diffOutflowFracByYear.png"), width=10, height=9)

p <- ggplot(fracSJR) + geom_boxplot(aes(x=month, y=diffFracOutflow, fill=factor(year))) +
    labs(x="", y="difference in mean outflow fraction (preferred - baseline)", fill="year") +
    theme_light()
ggsave(file.path(outputDir, "diffOutflowFracByYear_sideBySide.png"), width=10, height=6)

p <- ggplot(fracSJR, aes(x=diffFracOutflow, y=diffFracSJR, color=month)) +
    geom_point(size=1, alpha=0.75) +
    stat_ellipse(level=0.95, geom="path") +
    scale_color_brewer(palette="Paired") +
    geom_vline(xintercept=0) + geom_hline(yintercept=0) +
    facet_wrap(~month, ncol=5) +
    labs(x="difference in SJL outflow fraction (preferred - baseline)", y="difference in SJR junction routing fraction (preferred - baseline)") +
    theme_light() + theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5))
ggsave(file.path(outputDir, "diffOutflowFrac_ellipse.png"), width=12, height=6)

flow <- bind_rows(baselineFlow, preferredFlow) |> select(datetime, OH1, SJL, scenario) |> 
    mutate(year=as.factor(year(datetime)), month=as.factor(month(datetime, label=T)), dayOfMonth=as.factor(mday(datetime)), julianDay=yday(datetime))

thisYear <- 2015
thisDays <- c(20, 21)

for(thisMonth in c("Jun", "Sep")) {
    # OH1
    p <- ggplot(flow |> filter(month==thisMonth)) + 
        geom_line(aes(x=datetime, y=OH1, color=scenario, group=scenario)) + 
        facet_wrap(~year, ncol=5, scales="free_x") + 
        labs(title=paste("Flow in Old River,", thisMonth)) +
        theme_light() + theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5))
    ggsave(file.path(outputDir, paste0("flow_OH1_", thisMonth, "_", thisYear, ".png")), width=12, height=6)
    
    p <- ggplot(flow |> filter(month==thisMonth, year==thisYear, dayOfMonth %in% thisDays)) + 
        geom_line(aes(x=datetime, y=OH1, color=scenario, group=scenario)) + 
        facet_wrap(~year, ncol=5, scales="free") + 
        labs(title=paste("Flow in Old River,", thisMonth)) +
        theme_light()
    ggsave(file.path(outputDir, paste0("flow_OH1_", thisMonth, "_subset_", thisYear, ".png")), width=12, height=6)
    
    p <- ggplot(flow |> filter(month==thisMonth, dayOfMonth %in% thisDays)) + 
        geom_line(aes(x=datetime, y=OH1, color=scenario, group=scenario)) + 
        facet_wrap(~year, ncol=5, scales="free") + 
        labs(title=paste("Flow in Old River,", thisMonth)) +
        theme_light() + theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5))
    ggsave(file.path(outputDir, paste0("flow_OH1_", thisMonth, ".png")), width=12, height=6)
    
    # SJL
    p <- ggplot(flow |> filter(month==thisMonth)) +
        geom_line(aes(x=datetime, y=SJL, color=scenario, group=scenario)) + 
        facet_wrap(~year, ncol=5, scales="free_x") + 
        labs(title=paste("Flow in mainstem San Joaquin River downstream of HOR,", thisMonth)) +
        theme_light() + theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5))
    ggsave(file.path(outputDir, paste0("flow_SJL_", thisMonth, "_", thisYear, ".png")), width=12, height=6)
    
    p <- ggplot(flow |> filter(month==thisMonth, year==thisYear, dayOfMonth %in% thisDays)) + 
        geom_line(aes(x=datetime, y=SJL, color=scenario, group=scenario)) + 
        facet_wrap(~year, ncol=5, scales="free") + 
        labs(title=paste("Flow in mainstem San Joaquin River downstream of HOR,", thisMonth)) +
        theme_light()
    ggsave(file.path(outputDir, paste0("flow_SJL_", thisMonth, "_subset_", thisYear, ".png")), width=12, height=6)
    
    p <- ggplot(flow |> filter(month==thisMonth, dayOfMonth %in% thisDays)) + 
        geom_line(aes(x=datetime, y=SJL, color=scenario, group=scenario)) + 
        labs(title=paste("Flow in mainstem San Joaquin River downstream of HOR,", thisMonth)) +
        facet_wrap(~year, ncol=5, scales="free") + 
        theme_light() + theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5))
    ggsave(file.path(outputDir, paste0("flow_SJL_", thisMonth, ".png")), width=12, height=6)
}

