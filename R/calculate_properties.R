cell_properties <- function(data,
                            verbose = FALSE){
  ## 1) library size
  if(verbose){
    message("Calculating library size of cells...")
  }
  library_size <- log2(colSums(data) + 1)
  ## 2) fraction of zero in cells
  if(verbose){
    message("Calculating fraction of zero in cells...")
  }
  zero_fraction_cell <- apply(data, 2, function(x){
    sum(x == 0)/length(x)
  })
  ## 3) TMM normalization factor
  if(verbose){
    message("Calculating TMM normalization factor...")
  }
  dge <- edgeR::DGEList(counts = data)
  error_detect <- try(dge <- edgeR::calcNormFactors(dge, method = "TMM"),
                      silent = TRUE)
  if("try-error" %in% class(error_detect)){
    TMM_factor <- NA
  }else{
    TMM_factor <- log2(dge[["samples"]][["norm.factors"]] + 1)
  }
  ## 4) Effective library size
  if(verbose){
    message("Calculating effective library size...")
  }
  effective_library_size <- log2(library_size * TMM_factor + 1)
  ### list
  cell_properties <- dplyr::lst(library_size,
                                zero_fraction_cell,
                                TMM_factor,
                                effective_library_size)
  if(verbose){
    message("Done...")
  }
  return(cell_properties)
}


gene_properties <- function(data,
                            cpm_norm = TRUE,
                            verbose = FALSE){
  if(cpm_norm){
    if(verbose){
      message("Performing log2 CPM nomalization...")
    }
    error <- try({
      norm_data <- log2(edgeR::cpm(data)+1)
    }, silent = TRUE)
    if(is(error, "try-error")){
      norm_data <- log2(data+1)
    }
  }
  ## 1) mean expression of log2 CPM of genes
  if(verbose){
    message("Calculating mean expression for genes...")
  }
  mean_expression <- apply(norm_data, 1, mean)
  ## 2) standard variance of log2 CPM of genes
  if(verbose){
    message("Calculating standard variance of genes...")
  }
  sd <- apply(norm_data, 1, sd)
  ## 3) coefficient of variance
  if(verbose){
    message("Calculating coefficient of variance...")
  }
  cv <- apply(data, 1, sd)/apply(data, 1, mean) * 100
  ## 4) fraction of zero in genes
  if(verbose){
    message("Calculating fraction of zero in genes...")
  }
  zero_fraction_gene <- apply(data, 1, function(x){
    sum(x == 0)/length(x)
  })
  ## 5) dispersion
  if(verbose){
    message("Calculating gene dispersions using Seurat...")
  }
  if(!requireNamespace("Seurat", quietly = TRUE)){
    install.packages("Seurat")
  }
  data_seurat <- Seurat::CreateSeuratObject(counts = data, min.cells = 0, min.features = 0)
  data_seurat <- Seurat::NormalizeData(data_seurat,
                                       normalization.method = "LogNormalize",
                                       scale.factor = 1e6)
  data_seurat <- Seurat::FindVariableFeatures(data_seurat, selection.method = "disp")
  dispersion <- Seurat::HVFInfo(data_seurat)$dispersion
  ### list
  gene_properties <- dplyr::lst(mean_expression,
                                sd,
                                cv,
                                zero_fraction_gene,
                                dispersion)
  if(verbose){
    message("Done...")
  }
  return(gene_properties)
}

