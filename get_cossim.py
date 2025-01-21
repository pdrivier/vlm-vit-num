import gc #garbage collection
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

from get_image_pairs import generate_random_pairs, generate_imagepair_list
from tqdm import tqdm

from huggingface_hub import try_to_load_from_cache, scan_cache_dir
from transformers import ViTImageProcessor, ViTModel, CLIPProcessor, CLIPModel
import torch
from PIL import Image
from torch.nn.functional import cosine_similarity
import torch.nn.functional as F


def count_parameters(model):
    """credit: https://stackoverflow.com/questions/49201236/check-the-total-number-of-parameters-in-a-pytorch-model"""
    
	total_params = 0
	for name, parameter in model.named_parameters():
	    # if the param is not trainable, skip it
	    if not parameter.requires_grad:
	        continue
	    # otherwise, count it towards your number of params
	    params = parameter.numel()
	    total_params += params
	print(f"Total Trainable Params: {total_params}")
    
    return total_params
   

## TODO: Load list of image pairs
image_type = "rectangles"
dpath = "../vlm-vit-num-tmp/data/stimuli/{x}/".format(x=image_type)
list_of_imagepair_names = ...

metadata = pd.read_csv(os.path.join(dpath,"metadata.csv"))

## Define the hugging face paths for models and corresponding image processors
MODELS = {
    'clip-vit-base-patch32': ['openai/clip-vit-base-patch32', CLIPModel, CLIPProcessor]#,
    # 'clip-vit-large-patch14': ['openai/clip-vit-large-patch14', CLIPModel, CLIPProcessor],
    # 'clip-huge-14': ['laion/CLIP-ViT-H-14-laion2B-s32B-b79K', CLIPModel, CLIPProcessor],
    # 'clip-giant': ['laion/CLIP-ViT-g-14-laion2B-s12B-b42K', CLIPModel, CLIPProcessor],
    # 'clip-big-giant': ['laion/CLIP-ViT-bigG-14-laion2B-39B-b160k', CLIPModel, CLIPProcessor],
    # 'vit-base-patch16-224-in21k':['google/vit-base-patch16-224-in21k',ViTModel, ViTImageProcessor]
    }


## Set up results-gathering variable
gather_df = []
for mname, mspecs in MODELS.items(): 
	## Load your processor and model
	mpath, mclass, mprocessor = mspecs

	## Set up the dataframe to save results
	cosdf = []

	try:
		processor = mprocessor.from_pretrained(mpath)
		model = mclass.from_pretrained(mpath)
		model.eval()

		## Grab the number of trainable parameters in the model
		# TODO: check that it works for both clip and vit-only models!
		n_params = count_parameters(model)

		## Grab model-specific configuration information
		if "clip" in mname.lower(): 
			nlayers = len(model.vision_model.encoder.layers)
			patch_size = model.config.vision_config.patch_size

		else: 
			nlayers = len(model.encoder.layer)
			patch_size = model.config.patch_size

		## Iterate through the image pairs and get layerwise cosine distances
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

			### First, map image name to hidden states
			imfeats = {}
			for image_name in pair: 
				image = Image.open(os.path.join(dpath,image_name)).convert("RGB")
				# Encode image
				inputs = processor(images=image, return_tensors="pt")
				with torch.no_grad():
					## Grab just the CLS token out of the sequence of image tokens (this is 
					# the middle "0" index in the outputs.last_hidden_state variables
					if "clip" in mname.lower():
						outputs = model.vision_model(pixel_values=inputs.pixel_values, output_hidden_states=True)
						imfeats[image_name] = outputs.hidden_states
					else:
						outputs = model(**inputs, output_hidden_states=True)
						imfeats[image_name] = outputs.hidden_states		

			### Then, for each layer, grab the corresponding hidden state
			for layer in range(nlayers + 1):

				pair1 = imfeats[pair[0]][layer][:, 0, :]
				pair2 = imfeats[pair[1]][layer][:, 0, :]

				cos_sim = cosine_similarity(pair1, pair2)

				## Store the results and corresponding metadata
				d = {"model_name": mname,
					 "image_type": image_type,
					 "image_1": pair[0],
					 "image_2": pair[1],
					 "cosine_similarity": cos_sim.detach().numpy()[0],
					 "numerosity_1": im1_numerosity, 
					 "numerosity_2": im2_numerosity, 
					 "area_diff": area_diff,
					 "numerosity_comparison_type": comparison_type,
					 "layer": layer,
					 "n_params": n_params,
					 "patch_size": patch_size}
				gather_df.append(d)


		cosdf = pd.DataFrame(gather_df)
		savepath = "results/"
		filename = mname + "-" + image_type + "-cossim.csv"
		if not os.path.exists(savepath): 
			os.mkdir(savepath)
		cosdf.to_csv(os.path.join(savepath, filename))

	finally:
		## Cleanup model files to make room in machine!
		if torch.cuda.is_available():
		    torch.cuda.empty_cache()
		del model
		gc.collect()
		# Clear from disk cache
		cached_files = scan_cache_dir()
		for repo in cached_files.repos:
		    if mpath in repo.repo_id:
		        delete_repo_from_cache(repo.repo_id)
