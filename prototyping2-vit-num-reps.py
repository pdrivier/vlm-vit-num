#tmp prototyping
import itertools
import os 
import random 

import matplotlib.pyplot as plt
import pandas as pd
import os.path as op
import seaborn as sns
import numpy as np
from random import sample
from scipy.spatial.distance import cosine

from tqdm import tqdm

from transformers import ViTImageProcessor, ViTModel
import torch
from PIL import Image
from torch.nn.functional import cosine_similarity
import torch.nn.functional as F

##TODO: need to ensure target number of same/diff 
def generate_random_pairs(items, n_pairs):
    # Make sure we can generate the requested number of pairs
    max_pairs = len(items) * (len(items) - 1) // 2
    if n_pairs > max_pairs:
        raise ValueError(f"Can't generate {n_pairs} unique pairs from {len(items)} items. Maximum possible is {max_pairs}")
    pairs = set()
    while len(pairs) < n_pairs:
        # Get two random items
        a, b = sample(items, 2)
        # Add as tuple, sorting to ensure (a,b) and (b,a) are treated as same pair
        pairs.add(tuple(sorted([a, b])))
    return list(pairs)

image_type = "rectangles"
# dpath = "data/stimuli/{x}/".format(x=image_type)
dpath = "../vlm-vit-num-tmp/data/stimuli/{x}/".format(x=image_type) #prototyping dpath
metadata = pd.read_csv(os.path.join(dpath,"metadata.csv"))

## Sample multiple images matched for numerosity per image property (e.g. cum_area)
min_area = metadata["cum_area"].min()
max_area = metadata["cum_area"].max()

## Define stimulus cumulative area bins and labels
nbins = 10
bins = np.linspace(min_area, max_area, nbins)
labels = np.linspace(0, nbins-1, nbins)
labels = labels[:-1]

## Bin cumulative area
metadata['cum_area_bins'] = pd.cut(metadata["cum_area"], bins=bins, labels=labels, include_lowest=True)
_, area_bin_intervals = pd.cut(bins,10,retbins=True)

## Create the list of image pairs you'll get cosine similarity for
numerosities = list(set(metadata["numerosity"].values))
imagepair_ix_list_same = []
n_pairs = 10
for numerosity in numerosities:
	subnum = metadata[metadata["numerosity"] == numerosity]
	imagepair_ix_list_same.append(generate_random_pairs(subnum.index.tolist(),n_pairs))

imagepair_ix_list_same = [item for sublist in imagepair_ix_list_same for item in sublist]
image_ix_list = [item for tup in imagepair_ix_list_same for item in tup]
select_metadata = metadata[metadata.index.isin(image_ix_list)]

## Create list of different-numerosity image pairs
imagepair_ix_list_diff = generate_random_pairs(image_ix_list,len(image_ix_list)/2)

## Concatenate the list of same- and different-numerosity pairs
imagepair_ix_list = [imagepair_ix_list_same, imagepair_ix_list_diff]
imagepair_ix_list = [item for sublist in imagepair_ix_list for item in sublist]

## Create list of image names for the corresponding images
list_of_imagepair_names = [("stimulus_"+str(i[0])+".png","stimulus_"+str(i[1])+".png") for i in imagepair_ix_list]

## Load your processor and model
processor = ViTImageProcessor.from_pretrained('google/vit-base-patch16-224-in21k')
model = ViTModel.from_pretrained('google/vit-base-patch16-224-in21k')

gather_df = []
for pair in tqdm(list_of_imagepair_names): 

	## Grab metadata for each image in the pair
	imix1 = int(pair[0].split("_")[1].split(".png")[0])
	im1_numerosity = metadata.loc[imix1]["numerosity"]
	im1_area = metadata.loc[imix1]["cum_area"]
	im1_areabin = metadata.loc[imix1]["cum_area_bins"]

	imix2 = int(pair[1].split("_")[1].split(".png")[0])
	im2_numerosity = metadata.loc[imix2]["numerosity"]
	im2_area = metadata.loc[imix2]["cum_area"]
	im2_areabin = metadata.loc[imix2]["cum_area_bins"]

	## Figure out what kind of comparison type this is
	if (im1_numerosity - im2_numerosity == 0):
		comparison_type = "same"
	else:
		comparison_type = "different"

	## Figure out what the differences in surface area and numerosity are
	area_diff = np.abs(im1_area-im2_area) 
	numerosity_diff = np.abs(im1_numerosity-im2_numerosity)

	##TODO: Here is where you'd want to iterate by model layer to get intermediate representations
	layer = "last"

	## Grab the CLS representation for the image (token index 0)
	imfeats = {}
	for image_name in pair: 
	
		image = Image.open(os.path.join(dpath,image_name)).convert("RGB")

		# Encode image
		inputs = processor(images=image, return_tensors="pt")
		with torch.no_grad():
			outputs = model(**inputs)
			## Grab just the CLS token out of the sequence of image tokens (this is 
			# the middle "0" index in the outputs.last_hidden_state variable)
			imfeats[image_name] = outputs.last_hidden_state[:,0,:]
			## TODO: another option is to mean-pool the tokens excepting the CLS
			# imfeats[image_name] =  outputs.last_hidden_state[:,1:,:].mean(dim=1)
			### TODO: not sure if I should normalize? 
			# imfeats[image_name] = tmp / tmp.norm(dim=-1, keepdim=True)

	## Compute the cosine similarity of this pair
	cos_sim = cosine_similarity(imfeats[pair[0]],imfeats[pair[1]])

	## Store the results and corresponding metadata
	d = {"image_1": pair[0],
		 "image_2": pair[1],
		 "cosine_similarity": cos_sim.detach().numpy()[0],
		 "numerosity_1": im1_numerosity, 
		 "numerosity_2": im2_numerosity, 
		 "area_diff": area_diff,
		 "area_bin": area_bin,
		 "numerosity_comparison_type": comparison_type}
	gather_df.append(d)


cosdf = pd.DataFrame(gather_df)


