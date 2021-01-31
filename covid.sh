##!~/Documents/A-tracking-of-COVID-19/

# activating anaconda environment
source activate Data_Science_in_R


############################################

# opening a project in R-Studio
# rstudio A-tracking-of-COVID-19.Rproj

# exicuting the desired R-Script(s)

echo "---------------------------- Running dataCleaner.R ----------------------------"
Rscript COVID-19/dataCleaner.R

echo "---------------------------- Running datasets.R ----------------------------"
Rscript COVID-19/datasets.R

echo "----------------------- Over For A-tracking-of-COVID-19 -----------------------"


############################################

# opening other project
cd ../COVID-19-India/india-today


echo "----------------------------- Running scrapingData.R -----------------------------"
Rscript scrapingData.R

echo "----------------------------- Running india.R -----------------------------"
Rscript india.R

echo "----------------------------- Running dataset.R -----------------------------"
Rscript dataset.R

echo "----------------------------- Over For COVID-19-India ----------------------------"


############################################


# didplaying msg before quitting anaconda
echo "All done for now!!"

# deactivating the environment
conda deactivate
