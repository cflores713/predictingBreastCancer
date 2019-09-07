## Using Machine Learning tools to predict a patient's diagnosis from biopsy data
## Carlos Flores & Shawn Byrnes

![Image of biopsy](https://github.com/cflores713/predictingBreastCancer/blob/master/biopsy.png)

Recent studies have found that women with a history of a false-positive mammogram results may be at increased risk of developing subsequent breast cancer, but the origin of these regularities has remained opaque. We analyzed the physical characteristics of biopsy data to develop various successful predictive models needed to for such regularities to emerge. 

![Image of CM](https://github.com/cflores713/predictingBreastCancer/blob/master/rp2c1.png)

The result is a Radial SVM model to help radiologists and pathologists to investigate abnormal cellular growth irrespective of preconceived demographic biases. The model efficiently leverages statistical information by training only on the feature space of the physical bio-markers. Therefore, creating a meaningful substructure with significantly less computational demand given the reduced dimensionality. 

![Image of KMeans](https://github.com/cflores713/predictingBreastCancer/blob/master/kmRaw10.png)

Two other models, KMeans and KNN, performed similarly and give us greater insight into the emergent regularities.

Included in the repository is the source code in the R file, the biopsy data in the CSV file, and the analysis report in the pdf file. 
