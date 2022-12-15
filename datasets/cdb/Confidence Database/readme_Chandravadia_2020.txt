Contributors: Nand Chandravadia (Nand.Chandravadia@cshs.org), Ueli Rutishauser (Ueli.Rutishauser@cshs.org)

Citation: N. Chandravadia, D. Liang, A. G.P. Schjetnan, A. Carlson, M. Faraut, J.M. Chung, C.M. Reed, B. Dichter, U. Maoz, S. Kalia, T. Valiante, A. N. Mamelak, and U. Rutishauser. A NWB-based dataset and processing pipeline of human single-neuron activity during a declarative memory task. Scientific Data 7, 1-12 (2020). 

These data are from the Recognition Block of the New/Old Recognition Task. 

Stimulus and task:
During each session, the patients performed a Recognition Task, with an encoding and a recognition block. In the encoding block, the patients were shown 100 novel images, consisting of distinct visual categories (i.e, houses, landscapes, mobility, phones, animals, fruits, kids, military, space, cars, food, people, and spatial). Subsequently, in the recognition block, the patients were presented with 50 'novel' images (never seen before) and 50 'familar' images (shown in the encoding block) for a total of 100 trials. After presentation of each image, the subjects are instructed to indicate whether the image was 'old' or 'new' and with Confidence Judgments. 

Stimulus Columns

*Stimulus: Indicates whether the stimulus presented was 'novel' (never seen before) or 'familiar' (seen during encoding). 
0 (New), 1 (Old)


*Response: The patient's response judgment about whether the image had appeared before. 
0 (New), 1 (Old)

*Confidence: The patient's confidence judgment of their response to whether the image had appeared before. 
1 (guess), 2 (probably), 3 (confident)


*RT_decConf: Time elapsed, in seconds, between stimulus-onset and response. 
*Accuracy: Indicates whether or not the patient's response was correct. 
1 (correct), 0 (incorrect)

* Category: Each category encodes a particular visual category depending of the variant of the task. 

var 1: Houses (1), Landscapes (2), Mobility/Vehicles (3), Phones/Objects (4), Animals (5)
var 2: Fruit (1), Kids/People (2), Military/Vehicles (3), Space (4), Animals (5)
var 3: Cars (1), Food (2), People (3), Spatial (4), Animals (5)

*Response_Raw: The patient's response judgment about whether the image had appeared before. 
1 (new, confident), 2 (new, probably), 3 (new, guess), 4 (old, guess), 5 (old, probably), 6 (old, confident)

*Variant: Indicates the variant of the task. Each variant differs only in the visual categories presented during the task (see above)
*patientNumber: Indicates the patient Number since some patients perform different variants of the task. total = 57 patients. 
*ptDiagnosis: The Epilepsy Diagnosis of the patient based on intracranial depth electrode localization. 
0 (not localized) 1 (Right Mesial Temporal) 2 (Left Mesial Temporal) 3 (Right Neocortical Temporal) 4 (Left Neocortical Temporal) 5 (Right Lateral Frontal) 6 (Left Lateral Frontal) 7 (Bilateral Independent Temporal) 8 (Bilateral Independent Frontal) 9 (Right Other) 10 (Left Other) 11 (Right Occipital Cortex) 12 (Left Occipital Cortex) 13 (Bilateral Occipital Cortex) 14 (Right Insula) 15 (Left Insula) 16 (Independent Bilateral Insula)
*age: The Patient age, in years. 
*sex: The Patient sex assigned at birth. 

Block size: 100 trials per block.

Feedback: Trial-by-trial feedback was not provided.

Subject Population: range [16, 70], mean [36.7], std [14.3] (measured in years). The patient's within this dataset have intractable epilepsy, and thus are implanted with hybrid depth electrodes to localize the source of their epilepsy. 

Experiment Setting: Experiments were conducted in the Epilepsy Monitoring Unit (EMU). 

Response device: Cedrus Response Pad (RB-740)

Link to material/code: https://github.com/rutishauserlab/recogmem-release-NWB, https://osf.io/hv7ja/wiki/home/

Number of trials per subject: 100

Number of subjects: 57

Data collection dates: May 2005 - April 2019. 

Location of data collection: Rutishauser Lab, Cedars-Sinai/Caltech, Los Angeles, CA, USA; Toronto Western Hospital, Toronto, ON, Canada