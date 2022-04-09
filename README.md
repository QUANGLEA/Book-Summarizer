# Book Summarizer 

This project is written with Racket 8.3 and using existing Racket libraries with some imported libraries from Grinnell College CSC 151 course.

### Summary:
The Book Summarizer will summarize the books by section and by the number of sentences the user inputs. The application will also create a graphic visualizer of the number of occurences of keywords in the text. 

![book-summary-runthrough](https://user-images.githubusercontent.com/85794656/162549824-6c89a726-3d1d-4f5c-85d9-2d9ee0aee592.gif)

### Description:
This book summarizer takes in a file and then filters out the stop words (words that are too commonly used in sentences). By filtering out these words, the result would be lists of unque words that can be used to analyze further. The next step would be comparing the similarity of two sentences. I'm doing this by using the Cosine Similarity Formuata, which is known to measure the cosine angle between two vectors to see how far apart they are. To use the Cosine Simlarity Formula, I must transform the two sentences into two vectors by finding the union words between the sentences and tally those words inside each sentences. This would then give me two equal length lists of word tallys that I can use in my calculation of the Cosine Similarity value. I proceed to computing the value by using the formula and the result is a number between 0 and 1 (0 being that two sentences have no resemblences, 1 being that the two sentences are identical). With this, I then create a sum of Cosine Similarity value for the sentence against all other sentences in the text file. I then map all these values to a hash table that I could then get later to formulate my summary (a number of most relevant sentences in the text). For the graphic visualizer, I tallied all the words in the text and see which words occured the mmost often in the text. Then based on the number of words the user want, the graph would grab that number of words from the list of keywords and occurences. The graph then displays the keywords in order from most to least occurences. Both the summary and graph will be saved as separate files in the directory of the main racket file. 

### Installation: 
1. Install the latest version of Racket (for me it was Racket 8.3)
2. Download both the book-summarizer.rkt and the stop-words.txt files (make sure they are in the same directory) 
3. Go to File -> Package Manager -> then type in https://github.com/grinnell-cs/csc151.git#main and click update 
4. This should install the Grinnell College CSC 151 library into your Dr Racket 
5. Run the code (a GUI should pop up right after) 
6. Follow the instruction of the GUI and put in the appropriate information 
7. After you click submit, the GUI should tell you where your files are saved 
8. Go to the same directory of book-summarizer.rkt and view your files 
9. One would be a summary and the other would be an image of keywords

### Contact: 
Please feel free to open an issue or PR to discuss more features/bug reports. Also, feel free to contact me at lequang@grinnell.edu.
