# MLB Umpires -- The Study of Imperfection

Analyzed MLB Umpires looking at looking at accuracy and decision-making, with the attempt to predict when and why umpires are more and less accurate.
 --> Scraped 3 years of pitch-by-pitch data using baseballr
 --> Using PITCHF/x data to create an accurate "strike-zone," which then paired with PITCHF/x coordinate data, allowed me to find any and all instances of Incorrect Calls.
 --> Using a study by @tangotiger to determine values for each pitch and situation, allowing me to put a value to the missed calls in the form of "favor" and "impact"
 --> Conduct an analysis on umpire accuracy versus years of experience
 --> Looked at the season level of umpire impact, finding instances where the playoff picture could have drastically changed without umpire favor
 --> Summarizing Incorrect Call percentages by pitch location, frequency, and individual pitches to create probabilities of missed calls, and utilizing those to find the number of Incorrect Calls per 100 pitches.
 --> Used the favor and impact variables to pair with pitch probabilities, and find which pitches are most positively (for the pitcher) and negatively (against the pitcher) impacted by umpire-calling tendencies by umpire.
