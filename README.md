# logos
Performance art about slide-based research presentations.

## What it do
The content of the slide show is pre-composed and lives in `resources/slides.org`. On each frame, the software records the the speaker and performs onset-detection analysis of the audio stream (for now, it basically counts onsets). Then, the software compares the number of onsets to the word content of the slide. Using that information, the it will manipulate the textual content of each subsequent slide to try to maximize onsets within a given slide. 

The amount of change allowed slowly increases over time, peaking and ceasing roughly 2/3 through the presentation to add a "dramatic arc" to the piece. 

## Why?
In my work I try to pull drama out of somewhat mundane activities. For this performance, the "material" is audience attention. Ideally, it isn'at apparent when the speaker transitions from organized text to disorganized text. Audience members should struggle to derive linguistic meaning from what is gibberish for some time before having a moment of realization and shifting their listening mode to one more appropriate to sound art. Happily, audience members have told me this is the case! That shift is palpable and apparently enjoyable.
