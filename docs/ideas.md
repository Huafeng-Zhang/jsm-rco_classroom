

## Data
 + We need to identify fake users (interesting to see how many of them are fake users)
   + make a threshold: number of quiz they took
   + Time Spent 
 + Used fixed data base to build model
 + Use new database to learn data after creating two users

## Users of interest
 + Only users who participated in GED, Citizenship course are most important

## Possible uses of this project
- Send students summary report
 - f(sid,qid|category_id, nationality, lang, stay_time, gender, age) --> right/wrong
 - summarize "you're doing great in Eng, better than 90% of your friends in your country,  but you are 5% compared to China", positive/negative feedback.
 - performance metrics; You want to test if your actions are effective!!
 - age: 15-20, math --> notify and say 75% of your incorrect answer comes from math, lean math!
   - age < 7, qid_math --> no actions
- make suggestions to the committee or organization
  - translation: accuracy
  - loophole in the learning system, some questions are more difficult than others
  - some countries have more difficult time than others in some type of questions --> inform the instructor to emphasize that subject