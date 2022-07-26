check_under <- function(items, bg.file,
                        what = "category_name_here",
                        fdr=5, 
                        sep.sym="; ",
                        atleast=3) {
  
  ## FOR TROUBLESHOOTING ONLY ##
  # items = list.x[[i]][[j]]
  # bg.file = bg.dat
  # what = names(list.x[[i]])[j]
  ## ## ## ## ## ## ## ## ## ## 
  
  # define background (all IDs)
  bg <- bg.file %>% pull(ID) %>% unique()
  ### Add an option here to specify custom backgrounds
  
  
  # flatten the background file
  bg.file <- 
    bg.file %>% separate_rows(role_attended_before, sep = sep.sym) %>% filter(ID %in% bg)
  # replace NAs with "no_annot"
  bg.file[is.na(bg.file)] <- "no_annot"
  # obtain test set
  bg.test <- bg.file %>% filter(ID %in% items) %>% distinct()
  
  ## Enrichment to be tested for all unique "role_attended_before" terms that are:
  ## 1. Present in the test geneset
  ## 2. terms annotated for ≥ (at least) 5 number of IDs
  annot_terms <-
    bg.file %>%
    # Keep only the genes in my test geneset
    filter(ID %in% items) %>%
    distinct() %>% 
    group_by(role_attended_before) %>%
    summarize(num_ids = n()) %>%
    arrange(num_ids) %>%
    # Keep only the annotation terms that are annotated in at least 5 genes
    filter(num_ids >= atleast) %>%
    pull(role_attended_before)
  
  annot_terms_background <-
    bg.file %>%
    group_by(role_attended_before) %>%
    summarize(num_ids=n()) %>%
    filter(num_ids >= atleast) %>%
    pull(role_attended_before)
  
  
  ## Make an empty list that can save your results for each annotation term
  df.list <- list()
  
  # print("Testing for enrichment...")
  
  # Test the enrichment for each of the annot terms
  for (i in 1:length(annot_terms)) {
    
    #-#-#-##-#-#-##-#-#-##-#-#-#
    ### RUN HYPERGEOMETRIC TEST
    #-#-#-##-#-#-##-#-#-##-#-#-#
    
    ### For understanding the rationale behind the setup for the
    ### hypergeometric test, I would recommend reading the following:
    ### http://pedagogix-tagc.univ-mrs.fr/courses/ASG1/practicals/go_statistics_td/go_statistics_td_2015.html
    
    # get the annotation term to be tested for enrichment
    annot.i <- annot_terms[i]
    
    # number of items in the test set
    n_test <- items %>% length()
    
    # number of items of interest
    n_test_annotated <-
      bg.file %>% filter(ID %in% items) %>% filter(role_attended_before %in% annot_terms_background) %>%
      pull(ID) %>% unique() %>% length()
    
    # Number of items annotated with the annot term in the background gene set
    n_annot_background <-
      bg.file %>% filter(role_attended_before == annot.i) %>% distinct() %>% nrow()
    
    ### THIS ONE NEEDS SOME THOUGHT!!
    # Number of genes annotated with some annot term (at least 5) in the background gene set
    n_background_annotated <-
      bg.file %>% filter(role_attended_before %in% annot_terms_background) %>% distinct() %>% nrow()
    
    # Number of genes NOT annotated with the annot term in the background gene set (at least 5)
    n_not_annot_background <- n_background_annotated - n_annot_background
    
    # Number of genes annotated with the annot term in the test set
    n_annot_test <- bg.test %>% filter(role_attended_before == annot.i) %>% nrow()
    
    # define the number of possible genes with the given annotation
    x <- min(n_test, n_annot_background)
    
    #-#-#-##-#-#-##-#-#-##-#-#-#
    ### OBTAIN PROBABALITY
    #-#-#-##-#-#-##-#-#-##-#-#-#
    
    # calculates the total probability of obtaining at least the observed
    # number of genes annotated with the given term (GOs, pfams)
    #
    # i.e. summation of probabilities of all overlaps equal to or
    #       higher than the one observed in our test set
    #
    pval <-
      phyper(q=n_annot_test:x,
             m=n_annot_background,
             n=n_not_annot_background,
             k=n_test_annotated,
             lower.tail=F) %>%
      sum()
    
    
    #-#-#-##-#-#-##-#-#-##-#-#-#
    ### SAVE TEST RESULTS
    #-#-#-##-#-#-##-#-#-##-#-#-#
    
    df.list[[i]] <- data.frame(annot_term = annot.i,
                               what_desc = what,
                               sam_freq = round(n_annot_test/n_test_annotated, 3),
                               back_freq = round(n_annot_background/n_background_annotated, 3),
                               n_annot_test = n_annot_test,  ## x
                               n_test_total = n_test,
                               n_test_annotated = n_test_annotated, ## k
                               n_annot_bg = n_annot_background, ## m
                               n_not_annot_bg = n_not_annot_background, ## n
                               pVal = pval)
  }
  
  #-#-#-##-#-#-##-#-#-##-#-#-#
  ### MAKE OUTPUT FILE
  #-#-#-##-#-#-##-#-#-##-#-#-#
  
  ## Make the result table:
  df.enriched <-
    bind_rows(df.list, .id = "column_label") %>%
    dplyr::select(-column_label) %>%
    arrange(pVal) %>%
    mutate(adj_pVal = round(p.adjust(pVal, "BH"),5)) %>%
    # keeps only the annot terms that are found in the test set
    filter(n_annot_test != 0) %>%
    mutate(over_under = ifelse(sam_freq > back_freq, "over", "under")) %>%
    dplyr::select(annot_term, what_desc, over_under, adj_pVal, everything()) %>%
    arrange(over_under, adj_pVal)
  
  
}
