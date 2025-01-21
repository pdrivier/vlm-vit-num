## Sample list of same/different-numerosity image pairs
import os
import pickle
import random

import pandas as pd

def generate_random_pairs(items, n_pairs):
    # Make sure we can generate the requested number of pairs
    max_pairs = len(items) * (len(items) - 1) // 2
    if n_pairs > max_pairs:
        raise ValueError(f"Can't generate {n_pairs} unique pairs from {len(items)} items. Maximum possible is {max_pairs}")
    pairs = set()
    while len(pairs) < n_pairs:
        # Get two random items
        a, b = random.sample(items, 2)
        # Add as tuple, sorting to ensure (a,b) and (b,a) are treated as same pair
        pairs.add(tuple(sorted([a, b])))
    return list(pairs)

def generate_imagepair_list(df, dpath): 
	## Create the list of image pairs you'll get cosine similarity for
	numerosities = list(set(df["numerosity"].values))
	imagepair_ix_list_same = []
	n_pairs = 10 #number of image pairs per numerosity value
	for numerosity in numerosities:
		subnum = df[df["numerosity"] == numerosity]
		imagepair_ix_list_same.append(generate_random_pairs(subnum.index.tolist(),n_pairs))
	imagepair_ix_list_same = [item for sublist in imagepair_ix_list_same for item in sublist]
	image_ix_list = [item for tup in imagepair_ix_list_same for item in tup]
	select_metadata = df[df.index.isin(image_ix_list)]
	## Create list of different-numerosity image pairs
	imagepair_ix_list_diff = []
	for numerosity in numerosities: 
		# Grab df of target numerosity
		subnum_target = select_metadata[select_metadata["numerosity"]==numerosity]
		# Grab df of all other numerosities, excluding target
		subnum_others = select_metadata[select_metadata["numerosity"]!=numerosity]
		# Randomly sample image indices from the target numerosity list
		target_ix_list = random.sample(subnum_target.index.tolist(),n_pairs)
		for target_ix in target_ix_list:
			# Randomly sample a single image index from a different numerosity bucket
			other_ix = random.sample(subnum_others.index.tolist(),1)[0]
			# Create a tuple containing (image_target_numerosity, image_other_numerosity)
			imagepair_ix_list_diff.append((target_ix, other_ix))
	## Concatenate the list of same- and different-numerosity pairs
	imagepair_ix_list = [imagepair_ix_list_same, imagepair_ix_list_diff]
	imagepair_ix_list = [item for sublist in imagepair_ix_list for item in sublist]
	## Create list of image names for the corresponding images
	list_of_imagepair_names = [("stimulus_"+str(i[0])+".png","stimulus_"+str(i[1])+".png") for i in imagepair_ix_list]
	return select_metadata, list_of_imagepair_names

## Load metadata (containing info about numerosity)
image_type = "rectangles"
dpath = "../vlm-vit-num-tmp/data/stimuli/{x}/".format(x=image_type)
metadata = pd.read_csv(os.path.join(dpath,"metadata.csv"))

select_metadata, list_of_imagepair_names = generate_imagepair_list(metadata, dpath)

## Save the list to a file, so you can later load into the get_cossim.py script
image_filename = "imagepair-names-" + image_type + ".pkl"
with open(image_filename, 'wb') as file:
    pickle.dump(list_of_imagepair_names, file)
