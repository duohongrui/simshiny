umap_result <- function(ref_data, simulated_data, ref_prior, cell_info){
  ##### plot UMAP
  ref_umap <- umap::umap(ref_data %>% as.matrix() %>% t())
  ref_umap_plot <- plot_umap(umap = ref_umap, other_info = ref_prior)
  if(length(ref_umap_plot) == 2){
    ref_umap_plot <- ref_umap_plot[[1]] / ref_umap_plot[[2]]
  }
  simulated_umap <- umap::umap(simulated_data %>% as.matrix() %>% t())
  sim_umap_plot <- plot_umap(umap = simulated_umap, other_info = cell_info, which_data = "Simulated data")
  if(length(sim_umap_plot) == 2){
    sim_umap_plot <- sim_umap_plot[[1]] / sim_umap_plot[[2]]
  }
  umap_plot <- patchwork::wrap_plots(ref_umap_plot, sim_umap_plot, ncol = 2)
  return(umap_plot)
}

property_result <- function(ref_data, simulated_data){
  ##### data properties
  ref_cell_property <- cell_properties(data = ref_data, verbose = FALSE)
  ref_gene_property <- gene_properties(data = ref_data, verbose = FALSE)
  sim_cell_property <- cell_properties(data = simulated_data, verbose = FALSE)
  sim_gene_property <- gene_properties(data = simulated_data, verbose = FALSE)
  data_property_plot <- plot_violin(ref_cell_property = ref_cell_property,
                                    ref_gene_property = ref_gene_property,
                                    sim_cell_property = sim_cell_property,
                                    sim_gene_property = sim_gene_property)
  data_property_plot <- data_property_plot$cell_property_plot / data_property_plot$gene_property_plot + patchwork::plot_layout(guides = "collect")
  return(data_property_plot)
}


plot_umap <- function(umap, other_info, which_data = "Reference data"){
  colors <- c("#4c98c8", "#ec671e", "#2e8d3a", "#774d9c", "#29a8b9", "#9cb5db", "#f4a965",
              "#ee7e80", "#b38880", "#f5c290", "#838384", "#cdce79", "#be1a22", "#f4c33f",
              "#37b380", "#1d5e9b", "#36546d", "#b98519", "#dc4530", "#B3DE69", "#5d5098",
              "#edec73", "#f18a1c", "#0f86a9")
  spatial_coordinate <- umap$layout %>%
    as.data.frame()
  if("group.condition" %in% colnames(other_info) &
     length(unique(other_info$"group.condition")) > 1){
    spatial_coordinate <- spatial_coordinate %>%
      mutate(
        "group" = other_info$group.condition
      )
  }
  if("group" %in% colnames(other_info) &
     length(unique(other_info$"group")) > 1){
    spatial_coordinate <- spatial_coordinate %>%
      mutate(
        "group" = other_info$group
      )
  }
  if("batch.condition" %in% colnames(other_info) &
     length(unique(other_info$"batch.condition")) > 1){
    spatial_coordinate <- spatial_coordinate %>%
      mutate(
        "batch" = other_info$batch.condition
      )
  }
  if("batch" %in% colnames(other_info) &
     length(unique(other_info$"batch")) > 1){
    spatial_coordinate <- spatial_coordinate %>%
      mutate(
        "batch" = other_info$batch
      )
  }
  if("plate" %in% colnames(other_info) &
     length(unique(other_info$"plate")) > 1){
    spatial_coordinate <- spatial_coordinate %>%
      mutate(
        "batch" = other_info$plate
      )
  }

  if("group" %in% colnames(spatial_coordinate) &
     "batch" %in% colnames(spatial_coordinate)){
    if(length(unique(spatial_coordinate$group)) > 1 |
       length(unique(spatial_coordinate$batch)) > 1){
      index <- c("group", "batch")
    }else{
      if(length(unique(spatial_coordinate$group)) == 1 |
         length(unique(spatial_coordinate$batch)) == 1){
        index <- NULL
      }else if(length(unique(spatial_coordinate$group)) > 1){
        index <- "group"
      }else if(length(unique(spatial_coordinate$batch)) > 1){
        index <- "batch"
      }
    }
  }else if("group" %in% colnames(spatial_coordinate)){
    index <- "group"
  }else if("batch" %in% colnames(spatial_coordinate)){
    index <- "batch"
  }else{
    index <- NULL
  }
  if(which_data != "Reference data"){
    which_data <- "Simulated data"
  }
  if(is.null(index)){
    p <- ggplot(spatial_coordinate, mapping = aes(x = V1, y = V2)) +
      geom_point(color = sample(colors, 1)) +
      theme_bw() +
      theme(
        legend.title = element_blank(),
        panel.grid = element_blank(),
        axis.text = element_text(size = 12,
                                 color = "black"),
        axis.title = element_text(size = 12),
        plot.title = element_text(hjust = 0.5,
                                  size = 18),
        legend.text = element_text(size = 12,
                                   color = "black")
      ) +
      xlab("UMAP1") +
      ylab("UMAP2") +
      ggtitle(which_data)
  }else{
    p <- purrr::map(index, .f = function(x){
      ggplot(spatial_coordinate, mapping = aes(x = V1, y = V2)) +
        geom_point(aes(color = get(x))) +
        theme_bw() +
        theme(
          legend.title = element_blank(),
          panel.grid = element_blank(),
          axis.text = element_text(size = 12,
                                   color = "black"),
          axis.title = element_text(size = 12),
          plot.title = element_text(hjust = 0.5,
                                    size = 18),
          legend.text = element_text(size = 12,
                                     color = "black")
        ) +
        scale_color_manual(values = colors) +
        xlab("UMAP1") +
        ylab("UMAP2") +
        ggtitle(which_data)
    })
    if(length(p) == 1){
      p <- p[[1]]
    }
  }
  return(p)
}


plot_violin <- function(ref_cell_property,
                        ref_gene_property,
                        sim_cell_property,
                        sim_gene_property){
  colors <- c("#4c98c8", "#ec671e", "#2e8d3a", "#774d9c", "#29a8b9", "#9cb5db", "#f4a965",
              "#ee7e80", "#b38880", "#f5c290", "#838384", "#cdce79", "#be1a22", "#f4c33f",
              "#37b380", "#1d5e9b", "#36546d", "#b98519", "#dc4530", "#B3DE69", "#5d5098",
              "#edec73", "#f18a1c", "#0f86a9")
  ref_cell_property <- as.data.frame(ref_cell_property) %>%
    tibble::rownames_to_column("cell_name") %>%
    tidyr::pivot_longer(cols = 2:5, names_to = "property", values_to = "value") %>%
    mutate(
      "which_cell" = "real cells"
    )
  sim_cell_property <- as.data.frame(sim_cell_property) %>%
    tibble::rownames_to_column("cell_name") %>%
    tidyr::pivot_longer(cols = 2:5, names_to = "property", values_to = "value") %>%
    mutate(
      "which_cell" = "simulated cells"
    )
  ref_gene_property <- as.data.frame(ref_gene_property) %>%
    tibble::rownames_to_column("gene_name") %>%
    tidyr::pivot_longer(cols = 2:6, names_to = "property", values_to = "value") %>%
    mutate(
      "which_gene" = "real genes"
    )
  sim_gene_property <- as.data.frame(sim_gene_property) %>%
    tibble::rownames_to_column("gene_name") %>%
    tidyr::pivot_longer(cols = 2:6, names_to = "property", values_to = "value") %>%
    mutate(
      "which_gene" = "simulated genes"
    )
  #### cell property
  cell_property <- rbind(ref_cell_property, sim_cell_property) %>%
    mutate(
      "property" = case_when(
        property == "library_size" ~ "Library size (log2)",
        property == "effective_library_size" ~ "Effective library size (log2)",
        property == "TMM_factor" ~ "TMM factor (log2)",
        property == "zero_fraction_cell" ~ "Zero fraction of cells",
      )
    )
  cell_property$property <- factor(cell_property$property, levels = c("Library size (log2)", "Effective library size (log2)", "TMM factor (log2)", "Zero fraction of cells"))
  cell_property_plot <- ggplot(data = cell_property, aes(x = which_cell, y = value))+
    geom_violin(aes(fill = which_cell), show.legend = FALSE, alpha = 0.9)+
    facet_wrap(~property, scales = "free", nrow = 1) +
    scale_fill_manual(values = sample(colors, 2))+
    theme_bw() +
    theme(panel.grid = element_blank(),
          axis.text.x = element_text(angle = 30,
                                     hjust = 1,
                                     size = 15),
          axis.text = element_text(size = 12,
                                   color = "black"),
          plot.title = element_text(hjust = 0.5,
                                    size = 18),
          strip.background = element_rect(fill = "white",
                                          color = "black"),
          strip.text = element_text(size = 14))+
    labs(x = NULL, y = NULL, title = "Data property of cells")

  #### cell property
  gene_property <- rbind(ref_gene_property, sim_gene_property) %>%
    mutate(
      "property" = case_when(
        property == "cv" ~ "CV",
        property == "mean_expression" ~ "Mean expression",
        property == "sd" ~ "SD",
        property == "zero_fraction_gene" ~ "Zero fraction of genes",
        property == "dispersion" ~ "Dispersion",
      )
    )
  gene_property$property <- factor(gene_property$property, levels = c("Mean expression", "SD", "CV", "Zero fraction of genes", "Dispersion"))
  gene_property_plot <- ggplot(data = gene_property, aes(x = which_gene, y = value))+
    geom_violin(aes(fill = which_gene), show.legend = FALSE, alpha = 0.9)+
    facet_wrap(~property, scales = "free", nrow = 1) +
    scale_fill_manual(values = sample(colors, 2)) +
    theme_bw() +
    theme(panel.grid = element_blank(),
          axis.text.x = element_text(angle = 30,
                                     hjust = 1,
                                     size = 15),
          axis.text = element_text(size = 12,
                                   color = "black"),
          plot.title = element_text(hjust = 0.5,
                                    size = 18),
          strip.background = element_rect(fill = "white",
                                          color = "black"),
          strip.text = element_text(size = 14)) +
    labs(x = NULL, y = NULL, title = "Data property of genes")

  return(list(cell_property_plot = cell_property_plot,
              gene_property_plot = gene_property_plot))
}
