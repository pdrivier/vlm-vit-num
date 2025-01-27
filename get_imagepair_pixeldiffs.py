## Take the average pixel differences between image pairs 
import os
import pickle

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

from get_image_pairs import generate_imagepair_list
from PIL import Image

image_type = "dots"
dpath = "../vlm-vit-num-tmp/data/stimuli/{x}/".format(x=image_type)

metadata = pd.read_csv(os.path.join(dpath,"metadata.csv"))
imagepair_filename = "imagepair-names-" + image_type + ".pkl"
if os.path.exists(imagepair_filename):
    with open(imagepair_filename, 'rb') as file:
        list_of_imagepair_names = pickle.load(file)
    image_ix_list = [item for sublist in list_of_imagepair_names for item in sublist]
    image_ix_list = [i.split("_")[1].split(".png")[0] for i in image_ix_list]
    select_metadata = metadata.iloc[image_ix_list]
else:
    select_metadata, list_of_imagepair_names = generate_imagepair_list(metadata,dpath)

for pair in list_of_imagepair_names:

	image1_pix = Image.open(os.path.join(dpath,pair[0])).convert("RGB")
	image2_pix = Image.open(os.path.join(dpath,pair[1])).convert("RGB")

	## Check that images are the same size
	if image1_pix.size != image2_pix.size:
	    image2_pix = image2_pix.resize(image2_pix.size)

	# Convert images to NumPy arrays
	image1_array = np.array(image1_pix)
	image2_array = np.array(image2_pix)

	# Compute absolute differences between pixel values
	difference = np.abs(image1_array - image2_array)

	# Optionally, visualize the differences as an image
	difference_image = Image.fromarray(difference.astype('uint8'))
	difference_image.show()

	# For further analysis, compute statistics like mean difference
	mean_difference = np.mean(difference)

	## Store dataframe with average pixel differences pair image pair
	d = {"image_1": pair[0],
		 "image_2": pair[1],
		 "mean_pix_diff": mean_difference
	}