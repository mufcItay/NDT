Contributor(s):
Marika Constant (marika.constant@gmail.com), Roy Salomon and Elisa Filevich

Citation:
In prep.
Preprint (https://www.biorxiv.org/content/10.1101/2021.04.28.441761v2.full.pdf+html).
Preregistration (https://osf.io/pyjhm).

Task:
Participants held their hand out of view under a LEAP Motion infrared tracker while viewing a virtual representation of their hand that displayed tracked movements of the index finger. For the task, they had to make two consecutive movements with their index finger. One would be displayed in synchrony with the real movement and the other would be displayed with a small temporal delay. Participants made a two-interval forced choice between the two displayed movements of their index finger, indicating for which movement they felt more agency and then gave a confidence rating of their decision. Correct responses consisted in selecting the movement that had no temporal delay.

Columns:

* Stimulus: Indicates which of the two movements had no temporal delay

* Response: The subject’s belief about the above

* Confidence: Participants rated the confidence in their decision on a discrete scale from 1 (lowest) to 6 (highest). They did so by choosing one of the 6 numbers using the arrow keys.

* RT_dec: Time elapsed, in seconds, between type 1 decision cue and response

* RT_conf: Time elapsed, in seconds, between confidence cue and confidence rating response

* Noise:  Low sensory noise (0), high sensory noise (1) (see manipulations).

* Difficulty: The difficulty of the task was determined by the delay (in seconds) of the delayed movement. This was adjusted using an online staircasing procedure that adjusted the delay by increments or decrements of 0.01 seconds ranging from 0.01-0.4 (see staircase procedure).

* Error_trial: Participants were able to mark trials as errors (see NAN fields). If the trial was marked as an error at any point the Error_trial value is 1, otherwise it is 0.

Confidence scale:
Participants rated the confidence in their decision using a discrete scale from 1 (lowest) to 6 (highest). They did so by choosing one of the 6 numbers using the arrow keys, starting from a random initial position on each trial.

Manipulations:
A sensory noise manipulation was implemented to create two conditions:
* Low sensory noise (0): The virtual hand was displayed in bright, high contrast illumination.
* High sensory noise (1): The virtual hand was displayed in dim, low contrast illumination.

Participants performed 100 trials of each condition in a randomised order. The level of screen brightness in the high sensory noise condition was determined individually for each participant in order to account for differences in eyesight and lighting conditions in the room. To determine the brightness level, participants performed a short thresholding procedure where they placed their right hand on the table, under the LEAP tracker, and held it still. On each trial, participants first saw a fixation cross, followed by two consecutive presentations (separated by a flashed grey screen) of the virtual hand on the screen in the dark illumination condition, and in one case it was artificially enlarged. Participants then discriminated which of the two intervals contained the larger hand. We ran this in blocks of 10 trials and adjusted the brightness setting of the screen after each block until participants achieved 70-80 % correct in a block, and additionally did not report discomfort from straining to see the hand. This thresholding procedure took approximately 5 minutes. The brightness was, however, further adjusted if participants reported not being able to see the virtual hand movement during the training or at the beginning of the task. The brightness was only re-adjusted prior to the confidence task for one participant. 


Block size:
Participants completed 5 blocks with 40 trials each (200 trials in total). However, due to technical issues with the software (eg. freezing) some trials were skipped, so some subjects have missing trials.

Feedback:
Participants never received feedback on their performance, not even during training.

NaN fields:
NaN fields (found under RT_dec and RT_conf) indicate trials marked as error trials by participants. Participants were instructed to mark error trials using the Space key if there was a glitch of the virtual hand (such as flipping or contorting), they saw no virtual hand, they made the wrong hand movement, or accidentally pressed the wrong button during the type 1 decision. Participants were explicitly told not to mark errors only because they felt unsure about their decision, even if they felt they were guessing. Participants were able to mark error trials during the type 1 decision or the confidence rating, so some error trials have a recorded type 1 response. For simplicity, use the Error_trial column to identify trials marked as errors by the participants.

Subject population: 					
We tested 47 young, healthy participants between 18 and 35 years of age (M = 27.15, SD = 4.68) in Berlin. To participate in the study, we required that participants were right handed (Edinburgh Handedness Inventory score: M = 79.5, SD = 23.6), had no injury or condition preventing or restricting movement of the right index finger, had normal or corrected-to-normal vision, and were fluent in English. 

Response device: 
Keyboard
 

Experiment setting:
In lab.

Training:
Prior to the presented task, participants had performed a slightly similar task where they also viewed virtual representations of their self-generated movements of the index finger. Nevertheless, participants completed another short training consisting of 10 trials, but only in the low-noise condition, to adjust to the new movement and response structure.

Experiment goal:
The current experiment was part of a broader study investigating whether judgements of agency incorporate uncertainty in the same way that confidence judgements do in order to examine whether the former share computational mechanisms with metacognitive judgements. Here we include the confidence judgements task only.

Special instructions:
Participants were encouraged to use the whole confidence scale.
 
Experiment dates:
08.2020 - 09.2020
 
Location of data collection:
Metamotor lab, Humboldt-Universität zu Berlin, Germany.

Other Important Information

Staircase procedure:
The difference in delay levels between the movements in each trial was staircased, with one of the two movements always having no delay and the other being adjusted according to an online 2-down-1-up staircasing procedure aiming to achieve an overall accuracy of approximately 71%. Only the low-noise condition was staircased, and the delays of the high-noise condition were set to match those of the low-noise.
 
Apparatus:
We used a LEAP Motion controller (Leap Motion Inc., San Francisco, CA) to track participants’ hand motion and to control in real time the movement of a virtual hand displayed on the screen. The experiment ran on a Dell Latitude 5591 laptop (Intel core i5 with 16GB of RAM) with a display resolution of 1,920 x 1,080 (refresh rate = 60Hz) using software built in Unity 5.6.1, and was modified from software used in previous studies (Krugwasser et al., 2019; Stern et al., 2020). The computer was placed to the left of an opaque board, which occluded participants’ right hand from view. Participants placed their right hand under the LEAP Motion tracker, which was fixed with its sensors facing downward. Blackout curtains were used during all testing to keep the lighting conditions within the room as consistent as possible.

Missing trials:
Due to technical issues with the setup, such as freezing, some trials were skipped and not recorder (not even marked as error trials). Therefore some participants have less than 200 trials recorded.

Duration:
This task of the experiment took approximately 45 minutes.
