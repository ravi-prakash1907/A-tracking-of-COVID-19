##!~/Documents/A-tracking-of-COVID-19/

# activating anaconda environment
source activate Data_Science_in_R


############################################

# opening other project
cd ../COVID-19-India/india-today

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
