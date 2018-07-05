
## Data
4/1/2018
  + What are the difference among `quiz_date`, `completed_date`, `last_active`, 'joined`' and `reg_date`?
  + What is the definition of `quiz` column?
  + What is the format of `create_time`? Definition for `create_time`?
  + What is `count`?
  + Do we have data on which language the users used when answering questions?
  + How can students pass with score of zero?
  + Does `pass` in score table means that they pass the entitle course?
  + Weird last active date ("2024-01-17")
  + Time spent for each quiz? (seconds?)
  + Score, points scale? 
  + Can students submit a quiz without completing it?

  ## Decision:
  + do not need to look at following tables:
    + wpt1_wp_pro_quiz_form
    + wpt1_wp_pro_quiz_master
    + wpt1_wp_pro_quiz_prerequisite
    + wpt1_wp_pro_quiz_template

4/18/2018
  + What are the topics for quiz 18 and quiz 169?
  + What are *sfwd*, *nav*, and *acf* short for in **post_type**?
    + sfwd relates to the LMS=Learning Management System, for some historical reason.
    + nav relates to items in Wordpress menus (for navigation)
    + acf relates to Advanced Custom Fields, used for WordPressy stuff
  + *correct_same_text* in **quiz_question** table, what does 0 and 1 mean?
  + same question for *answer_points_diff_modus_activated* in **quiz_question** table?
  + What is *meta_value* in **postmeta** data?
    + Metadata is stored in key : value pairs, i.e. meta_key : meta_value. Keys can be anything needed for some purpose, like question_id, etc. 

4/22/2018
+ Why only 901 users listed in the toplist table?
  + Only for users have high ranks
+ What are points and results in *toplist* table?

05/15/2018
[List of Questions](https://docs.google.com/document/d/1itC4tNc04un5SDcuT-GYcI1KM0HgVRixYWsW80Ve4M4/edit?usp=sharing) 