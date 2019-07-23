# csci2322-p2
Project 2

On this project, you are not allowed to collaborate at the keyboard. Do not look at other student's
code.  You may collaborate on a whiteboard or on paper.
If you are having problems with syntax or semantics errors, I encourage you to come to office hours.

Student Name:
Trinity ID: 
Project Grade: 85/100(B+)

Core Project: 74/74
- not (null plays) is more idomatic than plays /= [], but functionally identical.

Complexities: 6/6

Avoiding Unnecessary Work: 0/14

Style: 5/6
- isValidMove can use one if statement and && the two conditions instead especially as combined and
  separately they result in false (-0.5).
-  in maximumPlay the list comprehension to find the max for xs recomputes the list twice, rather
   this could just be maximumPlay xs and be computed recursively (-0.5).
