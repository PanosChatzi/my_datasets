# Custom functions to find the cysteine residue number and i space from UniProt entries

uniprot_to_cys <- function(protein_name, dataframe, print_numeric = TRUE) {
  error_msg1 <- "Protein not found.\nPlease, check if the UniProt accession number is correct or if the protein is included in the human proteome."
  # Find the column index where the protein name is located
  column_index <- which(apply(dataframe, 2, function(x) protein_name %in% x))
  
  # Get the ID of the protein from the first row of the found column
  if(length(column_index) > 0) {
    if(print_numeric == TRUE) {
      return(as.numeric(colnames(dataframe)[column_index]))
    } else {
      return(paste("Protein", protein_name, "has", colnames(dataframe)[column_index], "cysteine residues."))
    }
  } else {
    return(cat(error_msg1))
  }
}
#

uniprot_to_ispace <- function(protein_name, dataframe) {
  error_msg1 <- "Protein not found.\nPlease, check if the UniProt accession number is correct or if the protein is included in the human proteome."
  # Find the column index where the protein name is located
  column_index <- which(apply(dataframe, 2, function(x) protein_name %in% x))
  
  # Get the ID of the protein from the first row of the found column
  if(length(column_index) > 0) {
    cys_number_R <- as.numeric(colnames(dataframe)[column_index])
  } else {
    return(cat(error_msg1))
  }
  
  # Calculate the theoretical i space using [Equation 1]: n^R from Chatzinikolaou and Cobley, 2024
  # where n and r are the redox phase space and cysteine residue integers, respectively
  n <- 2 # redox phase space is binary (oxidised or reduced)
  i_space <- n^cys_number_R
  return(i_space)
}