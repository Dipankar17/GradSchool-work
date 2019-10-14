# -*- coding: utf-8 -*-
"""
Problem : Split '.txt' file into multiple files based on the paragraphs 
and store every paragraph into a different file.

Solution:
    
E.g.  File 1 has the below file structure:
    
Text 1:
This is line 1 of para 1
This is line 2 of para 1
This is line 3 of para 1

Text 2:
This is line 1 of para 2
This is line 2 of para 2
This is line 3 of para 3

Now, the below scipt will split this file into two paragraphs and store them into
two different files.

@author: Dipankar
"""

import os
import re

#iterating over the files from directory
for file in os.listdir(r'C:\Users\dipan\Desktop\UB Sem 1 Course\Distributed Computing & Big Data\Bigdata project 1\Data\McCutchionAndBhowmick_Songs'):
    if file.endswith(".txt"): #matching text file
        print(file)
        count = 0
        flag = 0
        with open(os.path.join(r'C:\Users\dipan\Desktop\UB Sem 1 Course\Distributed Computing & Big Data\Bigdata project 1\Data\McCutchionAndBhowmick_Songs',file)) as f:
           for checkline in f: #iterating by each line over the file
               if(re.match('^text:$',checkline.lower())): 
                   flag = 1
               elif flag == 1:
                   if checkline.strip() == '': #stripping the file on the basis of empty lines
                       count+=1
                       pass
                   else:
                       with open(r'C:\Users\dipan\Desktop\UB Sem 1 Course\Distributed Computing & Big Data\Bigdata project 1\Data\McCutchionAndBhowmick_Songs'+ file + str(count)+'.txt','a+') as outf:
                           outf.write(checkline)
                           
               else:
                    pass
               
                   
                   