# This looks similar as the linear model code, but is not exactly so. With this you can draw a scatter plot for each individual combination of two variables within dataset
the_fun_figs <- function(tv_all, Group, hopiu, aa, bb, fn) {
  # Determine the subset of data based on the Group
  if (Group == 'male') {
    NAFLDo = tv_all[tv_all[, 'Gender'] == max(tv_all[, 'Gender']),]
  } else if (Group == 'female') {
    NAFLDo = tv_all[tv_all[, 'Gender'] == min(tv_all[, 'Gender']),]
  } else if (Group == 'All') {
    NAFLDo = tv_all
  }
  
  # Prepare the data frame SG0
  SG0 = NAFLDo[, c(2:dim(tv_all)[2])]
  oknames = colnames(SG0)
  SG0 = data.frame(SG0)
  colnames(SG0)
  colnames(SG0[, 8:27]) <- gsub("-", ".", colnames(SG0[, 8:27]))
  colnames(SG0[, 8:27]) <- gsub("/", ".", colnames(SG0[, 8:27]))
  
  # Initialize variables
  hesh = c()
  Treatment = colnames(tv_all)[52:58]
  TreatmentN = Treatment
  
  # Loop through xnam and y to generate plots
  xnam = colnames(SG0)[c(4:7)]
  y = Treatment
  for (i in 1:length(xnam)) {
    for (j in 1:length(y)) {
      hösh = hopiu[hopiu[, 1] == y[j] & hopiu[, 2] == xnam[i] & hopiu[, 3] == Group,]
      
      # Prepare data for plotting
      jeps = SG0
      r = as.numeric(hösh[4][1,])
      p.val = as.numeric(hösh[5][1,])
      rsadj = as.numeric(hösh[6][1,])
      colnames(jeps) = colnames(tv_all)[2:dim(tv_all)[2]]
      Treatment = as.character(hösh[2][1,])
      Mediator = as.character(hösh[1][1,])
      
      # Clean up Mediator names
      Mediator <- gsub("\\.", "-", Mediator)
      Mediator <- gsub("X", "", Mediator)
      if (Mediator == "T-Epi-T") { Mediator[Mediator == "T-Epi-T"] = "T/Epi-T" }
      Treatment2 = Treatment
      colnames(jeps)[colnames(jeps) == 'HOMA-IR'] = 'HOMA.IR'
      colnames(jeps) <- gsub(" ", "\\.", colnames(jeps))
      
      # Set up directories for saving plots
      main_dir = paste0("C://Users//patati//Desktop//Turku//R//", fn, Group, "/")
      sub_dir = Treatment
      if (!file.exists(file.path(main_dir, sub_dir))) {
        dir.create(file.path(main_dir, sub_dir))
      }
      setwd(file.path(main_dir, sub_dir))
      
      # Save plots
      if (Mediator != "T/Epi-T") {
        jpeg(paste("Correlations plotted", Group, Treatment2, Mediator, ".jpg"), width = 1000, height = 1000, quality = 100, pointsize = 20, res = 300)
      } else {
        jpeg(paste("Correlations plotted_alle", Treatment2, 'T.Epi.T', ".jpg"), width = 1000, height = 1000, quality = 100, pointsize = 20, res = 300)
      }
      
      # Generate and save the plot
      xcx = jeps[, Treatment]
      ycy = jeps[, Mediator]
      väh = round(sd(ycy) / 2, 2)
      a = ggplot(jeps, aes(y = ycy, x = xcx)) +
        geom_point() +
        xlab(Treatment) +
        ylab(Mediator) +
        geom_smooth(method = "lm", col = "black") +
        annotate("text", x = min(xcx), y = max(ycy), hjust = 0, vjust = 0, label = paste0("r = ", round(r, 5))) +
        annotate("text", x = min(xcx), y = max(ycy) - väh, hjust = 0, vjust = 0, label = paste0("p = ", round(p.val, 5))) +
        theme_classic()
      print(a)
      dev.off()
    }
  }
  
  # Repeat the process for different xnam and y combinations
  xnam = colnames(SG0)[c(2)]
  y = TreatmentN
  for (i in 1:length(xnam)) {
    for (j in 1:length(y)) {
      hösh = hopiu[hopiu[, 1] == y[j] & hopiu[, 2] == xnam[i] & hopiu[, 3] == Group,]
      
      jeps = SG0
      r = as.numeric(hösh[4][1,])
      p.val = as.numeric(hösh[5][1,])
      rsadj = as.numeric(hösh[6][1,])
      colnames(jeps) = colnames(tv_all)[2:dim(tv_all)[2]]
      Treatment = as.character(hösh[2][1,])
      Mediator = as.character(hösh[1][1,])
      rm(hösh)
      
      Mediator <- gsub("\\.", "-", Mediator)
      Mediator <- gsub("X", "", Mediator)
      if (Mediator == "T-Epi-T") { Mediator[Mediator == "T-Epi-T"] = "T/Epi-T" }
      colnames(jeps)[colnames(jeps) == 'HOMA-IR'] = 'HOMA.IR'
      colnames(jeps) <- gsub(" ", "\\.", colnames(jeps))
      main_dir = paste0("C://Users//patati//Desktop//Turku//R//", fn, Group, "/")
      sub_dir = Treatment
      if (!file.exists(file.path(main_dir, sub_dir))) {
        dir.create(file.path(main_dir, sub_dir))
      }
      setwd(file.path(main_dir, sub_dir))
      if (Mediator != "T/Epi-T") {
        jpeg(paste("Correlations plotted", Group, Treatment2, Mediator, ".jpg"), width = 1000, height = 1000, quality = 100, pointsize = 20, res = 300)
      } else {
        jpeg(paste("Correlations plotted", Treatment2, 'T.Epi.T', ".jpg"), width = 1000, height = 1000, quality = 100, pointsize = 20, res = 300)
      }
      
      xcx = jeps[, Treatment]
      ycy = jeps[, Mediator]
      väh = round(sd(ycy) / 2, 2)
      a = ggplot(jeps, aes(y = ycy, x = xcx)) +
        geom_point() +
        xlab(Treatment) +
        ylab(Mediator) +
        geom_smooth(method = "lm", col = "black") +
        annotate("text", x = min(xcx), y = max(ycy), hjust = 0, vjust = 0, label = paste0("r = ", round(r, 4))) +
        annotate("text", x = min(xcx), y = max(ycy) - väh, hjust = 0, vjust = 0, label = paste0("p = ", round(p.val, 5))) +
        theme_classic()
      print(a)
      dev.off()
      hesh = rbind(hesh, c(y[j], xnam[i], Group, r, p.val, rsadj))
    }
  }
  
  # Repeat the process for different xnam and y combinations
  xnam = colnames(SG0)[c(3)]
  y = TreatmentN
  for (i in 1:length(xnam)) {
    for (j in 1:length(y)) {
      hösh = hopiu[hopiu[, 1] == y[j] & hopiu[, 2] == xnam[i] & hopiu[, 3] == Group,]
      
      jeps = SG0
      r = as.numeric(hösh[4][1,])
      p.val = as.numeric(hösh[5][1,])
      rsadj = as.numeric(hösh[6][1,])
      colnames(jeps) = colnames(tv_all)[2:dim(tv_all)[2]]
      Treatment = as.character(hösh[2][1,])
      Mediator = as.character(hösh[1][1,])
      rm(hösh)
      
      Mediator <- gsub("\\.", "-", Mediator)
      Mediator <- gsub("X", "", Mediator)
      if (Mediator == "T-Epi-T") { Mediator[Mediator == "T-Epi-T"] = "T/Epi-T" }
      Treatment2 = Treatment
      colnames(jeps)[colnames(jeps) == 'HOMA-IR'] = 'HOMA.IR'
      colnames(jeps) <- gsub(" ", "\\.", colnames(jeps))
      main_dir <- paste0("C://Users//patati//Desktop//Turku//R//", fn, Group, "/")
      sub_dir <- Treatment
      if (!file.exists(file.path(main_dir, sub_dir))) {
        dir.create(file.path(main_dir, sub_dir))
      }
      setwd(file.path(main_dir, sub_dir))
      if (Mediator != "T/Epi-T") {
        jpeg(paste("Correlations plotted_alle", Treatment2, Mediator, ".jpg"), width = 1000, height = 1000, quality = 100, pointsize = 20, res = 300)
      } else {
        jpeg(paste("Correlations plotted", Treatment2, 'T.Epi.T', ".jpg"), width = 1000, height = 1000, quality = 100, pointsize = 20, res = 300)
      }
      
      xcx = jeps[, Treatment]
      ycy = jeps[, Mediator]
      väh = round(sd(ycy) / 2, 2)
      a = ggplot(jeps, aes(y = ycy, x = xcx)) +
        geom_point() +
        xlab(Treatment) +
        ylab(Mediator) +
        geom_smooth(method = "lm", col = "black") +
        annotate("text", x = min(xcx), y = max(ycy), hjust = 0, vjust = 0, label = paste0("r = ", round(r, 5))) +
        annotate("text", x = min(xcx), y = max(ycy) - väh, hjust = 0, vjust = 0, label = paste0("p = ", round(p.val, 5))) +
        theme_classic()
      print(a)
      dev.off()
    }
  }
  
  # Repeat the process for different xnam and y combinations
  xnam = TreatmentN
  y = colnames(SG0[, 8:27])
  for (i in 1:length(xnam)) {
    for (j in 1:length(y)) {
      hösh = hopiu[hopiu[, 1] == y[j] & hopiu[, 2] == xnam[i] & hopiu[, 3] == Group,]
      
      jeps = SG0
      rm(r, p.val, rsadj)
      r = as.numeric(hösh[4][1,])
      p.val = as.numeric(hösh[5][1,])
      rsadj = as.numeric(hösh[6][1,])
      colnames(jeps) = colnames(tv_all)[2:dim(tv_all)[2]]
      Treatment = as.character(hösh[2][1,])
      Mediator = as.character(hösh[1][1,])
      rm(hösh)
      
      Mediator <- gsub("\\.", "-", Mediator)
      Mediator <- gsub("X", "", Mediator)
      if (Mediator == "T-Epi-T") { Mediator[Mediator == "T-Epi-T"] = "T/Epi-T" }
      Treatment2 = Treatment
      colnames(jeps)[colnames(jeps) == 'HOMA-IR'] = 'HOMA.IR'
      colnames(jeps) <- gsub(" ", "\\.", colnames(jeps))
      main_dir <- paste0("C://Users//patati//Desktop//Turku//R//", fn, Group, "/")
      sub_dir <- Treatment
      if (!file.exists(file.path(main_dir, sub_dir))) {
        dir.create(file.path(main_dir, sub_dir))
      }
      setwd(file.path(main_dir, sub_dir))
      if (Mediator != "T/Epi-T") {
        jpeg(paste("Correlations plotted", Group, Treatment2, Mediator, ".jpg"), width = 1000, height = 1000, quality = 100, pointsize = 20, res = 300)
      } else {
        jpeg(paste("Correlations plotted", Treatment2, 'T.Epi.T', ".jpg"), width = 1000, height = 1000, quality = 100, pointsize = 20, res = 300)
      }
      
      xcx = jeps[, Treatment]
      ycy = jeps[, Mediator]
      väh = round(sd(ycy) / 2, 2)
      a = ggplot(jeps, aes(y = ycy, x = xcx)) +
        geom_point() +
        xlab(Treatment) +
        ylab(Mediator) +
        geom_smooth(method = "lm", col = "black") +
        annotate("text", x = min(xcx), y = max(ycy), hjust = 0, vjust = 0, label = paste0("r = ", round(r, 4))) +
        annotate("text", x = min(xcx), y = max(ycy) - väh, hjust = 0, vjust = 0, label = paste0("p = ", round(p.val, 5))) +
        theme_classic()
      print(a)
      dev.off()
    }
  }
  
  # Repeat the process for different xnam and y combinations
  xnam = c(aa, bb)
  y = colnames(SG0[, 8:27])
  for (i in 1:length(xnam)) {
    for (j in 1:length(y)) {
      hösh = hopiu[hopiu[, 1] == y[j] & hopiu[, 2] == xnam[i] & hopiu[, 3] == Group,]
      
      jeps = SG0
      rm(r, p.val, rsadj)
      r = as.numeric(hösh[4][1,])
      p.val = as.numeric(hösh[5][1,])
      rsadj = as.numeric(hösh[6][1,])
      colnames(jeps) = colnames(tv_all)[2:dim(tv_all)[2]]
      Treatment = as.character(hösh[2][1,])
      Mediator = as.character(hösh[1][1,])
      rm(hösh)
      
      Mediator <- gsub("\\.", "-", Mediator)
      Mediator <- gsub("X", "", Mediator)
      if (Mediator == "T-Epi-T") { Mediator[Mediator == "T-Epi-T"] = "T/Epi-T" }
      Treatment2 = Treatment
      colnames(jeps)[colnames(jeps) == 'HOMA-IR'] = 'HOMA.IR'
      colnames(jeps) <- gsub(" ", "\\.", colnames(jeps))
      main_dir <- paste0("C://Users//patati//Desktop//Turku//R//", fn, Group, "/")
      sub_dir <- Treatment
      if (!file.exists(file.path(main_dir, sub_dir))) {
        dir.create(file.path(main_dir, sub_dir))
      }
      setwd(file.path(main_dir, sub_dir))
      if (Mediator != "T/Epi-T") {
        jpeg(paste("Correlations plotted", Group, Treatment2, Mediator, ".jpg"), width = 1000, height = 1000, quality = 100, pointsize = 20, res = 300)
      } else {
        jpeg(paste("Correlations plotted", Treatment2, 'T.Epi.T', ".jpg"), width = 1000, height = 1000, quality = 100, pointsize = 20, res = 300)
      }
      
      xcx = jeps[, Treatment]
      ycy = jeps[, Mediator]
      väh = round(sd(ycy) / 2, 2)
      a = ggplot(jeps, aes(y = ycy, x = xcx)) +
        geom_point() +
        xlab(Treatment) +
        ylab(Mediator) +
        geom_smooth(method = "lm", col = "black") +
        annotate("text", x = min(xcx), y = max(ycy), hjust = 0, vjust = 0, label = paste0("r = ", round(r, 5))) +
        annotate("text", x = min(xcx), y = max(ycy) - väh, hjust = 0, vjust = 0, label = paste0("p = ", round(p.val, 5))) +
        theme_classic()
      print(a)
      dev.off()
      hesh = rbind(hesh, c(y[j], xnam[i], Group, r, p.val, rsadj))
    }
  }
  
  # Repeat the process for different xnam and y combinations
  xnam = c('AGE', 'BMI', colnames(SG0)[c(4:7)])
  y = colnames(SG0[, 8:27])
  for (i in 1:length(xnam)) {
    for (j in 1:length(y)) {
      hösh = hopiu[hopiu[, 1] == y[j] & hopiu[, 2] == xnam[i] & hopiu[, 3] == Group,]
      
      jeps = SG0
      rm(r, p.val, rsadj)
      r = as.numeric(hösh[4][1,])
      p.val = as.numeric(hösh[5][1,])
      rsadj = as.numeric(hösh[6][1,])
      colnames(jeps) = colnames(tv_all)[2:dim(tv_all)[2]]
      Treatment = as.character(hösh[2][1,])
      Mediator = as.character(hösh[1][1,])
      rm(hösh)
      
      Mediator <- gsub("\\.", "-", Mediator)
      Mediator <- gsub("X", "", Mediator)
      if (Mediator == "T-Epi-T") { Mediator[Mediator == "T-Epi-T"] = "T/Epi-T" }
      Treatment2 = Treatment
      colnames(jeps)[colnames(jeps) == 'HOMA-IR'] = 'HOMA.IR'
      colnames(jeps) <- gsub(" ", "\\.", colnames(jeps))
      main_dir <- paste0("C://Users//patati//Desktop//Turku//R//", fn, Group, "/")
      sub_dir <- Treatment
      if (!file.exists(file.path(main_dir, sub_dir))) {
        dir.create(file.path(main_dir, sub_dir))
      }
      setwd(file.path(main_dir, sub_dir))
      if (Mediator != "T/Epi-T") {
        jpeg(paste("Correlations plotted", Group, Treatment2, Mediator, ".jpg"), width = 1000, height = 1000, quality = 100, pointsize = 20, res = 300)
      } else {
        jpeg(paste("Correlations plotted_", Treatment2, 'T.Epi.T', ".jpg"), width = 1000, height = 1000, quality = 100, pointsize = 20, res = 300)
      }
      
      xcx = jeps[, Treatment]
      ycy = jeps[, Mediator]
      väh = round(sd(ycy) / 2, 2)
      a = ggplot(jeps, aes(y = ycy, x = xcx)) +
        geom_point() +
        xlab(Treatment) +
        ylab(Mediator) +
        geom_smooth(method = "lm", col = "black") +
        annotate("text", x = min(xcx), y = max(ycy), hjust = 0, vjust = 0, label = paste0("r = ", round(r, 5))) +
        annotate("text", x = min(xcx), y = max(ycy) - väh, hjust = 0, vjust = 0, label = paste0("p = ", round(p.val, 5))) +
        theme_classic()
      print(a)
      dev.off()
      hesh = rbind(hesh, c(y[j], xnam[i], Group, r, p.val, rsadj))
    }
  }
  
  # Finalize the results
  hesa = hesh
  hoi = as.data.frame(hesh)
  main_dir = paste0("C://Users//patati//Desktop//Turku//R//", fn)
  setwd(main_dir)
  
  return(list(hopiu))
}