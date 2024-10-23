# from transformers import ViTImageProcessor, ViTModel
# from PIL import Image
# import requests

# url = 'http://images.cocodataset.org/val2017/000000039769.jpg'
# image = Image.open(requests.get(url, stream=True).raw)

# processor = ViTImageProcessor.from_pretrained('google/vit-base-patch16-224-in21k')
# model = ViTModel.from_pretrained('google/vit-base-patch16-224-in21k')
# inputs = processor(images=image, return_tensors="pt")

# outputs = model(**inputs)
# last_hidden_states = outputs.last_hidden_state

"""Run HF models on either natural or synthetic affordance images."""

import pandas as pd
import os.path as op
import seaborn as sns
import numpy as np
from scipy.spatial.distance import cosine

from tqdm import tqdm

from transformers import CLIPProcessor, CLIPModel, FlavaProcessor, FlavaModel, BlipProcessor, BlipModel, AlignProcessor, AlignModel
from transformers import ViltForImageAndTextRetrieval, ViltProcessor, ViltConfig, BridgeTowerForImageAndTextRetrieval, BridgeTowerProcessor
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

class HFModelRunner(object):



    def __init__(self, model_name, image_type):
        self.model_name = model_name
        self.model_path = MODELS[model_name][0]
        self.image_type = image_type 

        self.load_model(model_name)
        self.load_dataset(image_type)

    def load_model(self, model_name):

        classes = (MODELS[model_name][1], MODELS[model_name][2])

        self.processor = classes[1].from_pretrained(self.model_path)
        self.model = classes[0].from_pretrained(self.model_path)

        self.num_params = count_parameters(self.model)


    def load_dataset(self, image_type = "rectangles"):
        path = "data/stimuli/{x}/metadata.csv".format(x = image_type)
        self.df = pd.read_csv(path)


    def compare_inputs(self, image):

        if self.model_name in ['clip-vit-base-patch32', 'clip-vit-large-patch14', 
                              'blip-image-captioning-base', 'blip-image-captioning-large',
                              'align-base', 'clip-huge-14', 'clip-giant', 'clip-big-giant']:

            ## Encode image
            image_input = self.processor(images=image, return_tensors="pt")

            # Get the image and text representations
            with torch.no_grad():
                image_features = self.model.get_image_features(**image_input)
            

            # Normalize the features
            image_features = image_features / image_features.norm(dim=-1, keepdim=True)
            

            return cosine_similarity(image_features, text_features).item()


    def run_model(self):

        ### Track results
        results = []

        for index, row in tqdm(self.df.iterrows(), total=self.df.shape[0]):
           

            for cond in ['afforded_image', 'non-afforded_image', 'related_image']:

                ## Get image path
                img_name = row[cond]
                path = "data/stimuli/{x}/images/".format(x = self.version)
                img_path = op.join(path, img_name)

                # Load and preprocess the image
                image = Image.open(img_path).convert("RGB")

                # Get features
                response = self.compare_inputs(text, image)

                # Response type
                response_type = 'logits' if self.model_name in ['vilt-coco', 'bridgetower'] else 'cosine_similarity'

                # track data
                results.append({
                    'text': text,
                    'condition': cond,
                    'img_name': img_name,
                    'group_id': row['group_id'],
                    'prompt_type': row['prompt_type'],
                    'response_type': response_type,
                    'response': response,
                    'model_name': self.model_name,
                    'version': self.version,
                    'num_params': self.num_params
                })


        # Turn results into DataFrame
        self.df_results = pd.DataFrame(results)
        
        # Save to .csv
        SAVE_PATH = "data/processed/models/hf_models/{model}_{version}_affordances.csv".format(model = self.model_name, version = self.version)
        print("Saving to " + SAVE_PATH)
        self.df_results.to_csv(SAVE_PATH, index = False)

MODELS = {"vit-base-patch16-224-in21k": ["google/vit-base-patch16-224-in21k",ViTModel,ViTImageProcessor]
    }
models_to_run = ['vit-base-patch16-224-in21k']

for model_name in models_to_run:
    print(model_name)

    hf = HFModelRunner(model_name = model_name,
    				   version = version)

    hf.run_model()
                


    
