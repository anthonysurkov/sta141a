# STA 141A Project, W2025
## Anthony Surkov, 03.17.2025

These are all of the files for the STA 141A project, including models, data on the models, data generated as part of feature collection, and all programs.

### EDA
- eda_session11.R: a brief exploration into the data's structure based on one session.
- eda_structure.R: an exploration of pertinent data structures across sessions.
- eda_pca_exploration.R: a look into whether PCA could model this data.
- eda_pca_output.R: more on PCA.
- eda_temporal.R: provides substantiation for use of certain autocorrelation features.

### Integration
- restructuring.R: provides tools to restructure data, load it, modify it, etc.
- integration.R: provides tools for feature mining and exporting.
- future_direction.R: an attempt at proper feature mining following model collapse.

### Modeling
- Data/: holds data concerning model epochs in CSVs.
- Models/: holds .pth transformers.
- training.py: defines binary classification transformers and trains them.
- validation.py: validates transformer models against provided validation datasets.
- training_graphs.R: generates ggplot2 graphs from Data/ items.

The models included in the report are...
- model_der_eigen_lr_d32_l1e-5.pth: Derivative-Eigenvalue-L/R 32-D model; this one was used in validation
- model_der_lr_d32_l1e-5.pth:       Derivative-L/R 32-D model
- model_eigen_lr_d32_l1e-5.pth:     Eigenvalue-L/R 32-D model
- model_full_d32_l1-e5_shuffled.pth Shuffled full 32-D model
- model_full_d32_l1e-4.pth          Full 32-D model with faster learning
- model_full_d32_l1e-5.pt           Full 32-D model; this one was best in validation
- model_noglobals_d32_l1e-4.pth     No globals 32-D model with faster learning
- model_noglobals_d32_l1e-5.pth     No globals 32-D model
- model_noglobals_d32_l1e-5.pth     No globals 64-D model
Included also are many other random experimental models, with varying degrees of data available.

A typical training pipeline looks like...
- Commenting out interesting features in integration.R between lines 464 and 507;
- Changing the output files in integration.R on lines 611 and 612;
- Uncommenting main() on line 630 and running it;
- Loading the datasets into training.py and selecting appropriate hyperparameters.

To use or generate any model, the appropriate datasets, model names, and hyperparameters need to be modified in Modeling files.
For example, in training.py, the NAME= field should have the name of the model substituted in. I have been treating this as some_name_dxx_l1-ey_etc, where dxx is the dimensionality, l1-ey is the learning speed, and 'etc' is a stand-in for any other parameters you feel may be of interest.
Each model has a different set of associated input dimensions; for example, the full model has INPUT_DIM=82. If there is a mismatch between the loaded model and the dimensionality, you will receive an error telling you you cannot multiply matrices of differing dimensions together; ameliorate the INPUT_DIM accordingly.
Lastly, training npy's are specific to models and therefore also need to be matched. The DATA_TRAIN and DATA_VALID fields are for this. The training data files share naming conventions with the model names.

