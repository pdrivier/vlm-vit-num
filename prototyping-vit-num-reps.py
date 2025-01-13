#tmp prototyping
import itertools
import os 
import random 

import matplotlib.pyplot as plt
import pandas as pd
import os.path as op
import seaborn as sns
import numpy as np
from scipy.spatial.distance import cosine

from tqdm import tqdm

from transformers import ViTImageProcessor, ViTModel
import torch
from PIL import Image
from torch.nn.functional import cosine_similarity
import torch.nn.functional as F

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

## Create the list of images you'll get cosine similarity for
k = 3 ## num of images per area bin
numerosities = list(set(metadata["numerosity"].values))
image_ix_list = []
for numerosity in numerosities:
	subnum = metadata[metadata["numerosity"] == numerosity]
	for area_bin in labels: 
		subarea = subnum[subnum["cum_area_bins"]==area_bin]
		imix = random.sample(list(subarea.index.values),k)
		image_ix_list.append(imix)


image_ix_list = [item for sublist in image_ix_list for item in sublist]
select_metadata = metadata[metadata.index.isin(image_ix_list)]

## Create list of image names for the corresponding images
list_of_images = ["stimulus_"+str(i)+".png" for i in image_ix_list]

## Load your processor and model
processor = ViTImageProcessor.from_pretrained('google/vit-base-patch16-224-in21k')
model = ViTModel.from_pretrained('google/vit-base-patch16-224-in21k')

gather_df = []
for numerosity in tqdm(numerosities): 
	subnum = select_metadata[select_metadata["numerosity"]==numerosity]
	for area_bin in tqdm(labels): 
		subarea = subnum[subnum["cum_area_bins"]==area_bin]
		imixs = subarea.index.values
		image_names = ["stimulus_"+str(i)+".png" for i in imixs]
		imfeats = {}
		for image_name in image_names:
			## TODO: does the representation change if we convert image to "RGB" format? 
			image = Image.open(os.path.join(dpath,image_name)).convert("RGB")
			## Encode image
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
		
		## Now, do all pairwise cosine similarity comparisons
		image_pairs = list(itertools.combinations(imfeats.keys(),2))
		for pair in image_pairs: 
			cos_sim = cosine_similarity(imfeats[pair[0]],imfeats[pair[1]])

			imix1 = int(pair[0].split("_")[1].split(".png")[0])
			imix2 = int(pair[1].split("_")[1].split(".png")[0])

			im1_numerosity = numerosity
			im2_numerosity = numerosity
			if (im1_numerosity - im2_numerosity == 0):
				comparison_type = "same"
			else:
				comparison_type = "different"

			im1_area = subarea.loc[imix1]["cum_area"]
			im2_area = subarea.loc[imix2]["cum_area"]
			area_diff = im1_area-im2_area 

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



