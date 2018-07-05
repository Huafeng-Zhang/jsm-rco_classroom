
#### Keywords: 

Logistic Regression, Model Selection, Natural Language Processing, Online Education


#### Abstract

A major online education problem is the high dropout rate. One way to reduce dropouts is to monitor student progress and proactively assist students who appear to be struggling. To identify such students, we construct a model of student performance which predicts the likelihood of successful course completion, based on quiz scores and other predictors such as type of quiz questions, time spent on each question, and interval between quizzes. Students whose predicted completion probability falls below a threshold at some point in the course can be offered additional support, such as free tutoring or a support forum. We adopt this approach to an online GED course for refugees in the U.S. 

A mixed effects logistic regression approach is used to create the base model, with model selection techniques to find the optimal base model. Then we investigate several methods--including keyword extraction, clustering, and topic modeling--to derive additional features from unstructured text responses. We use cross validation to assess whether the augmented model leads to better predictions. We report the results of this model augmentation and discuss key issues in implementing such techniques.
