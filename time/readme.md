## time

### description

a module with convenient subroutine that records program starting and ending times and displays the duration of the run

### note

- this might not work in MacOS since it retrives system time info from the OS and Mac might have access restrictions
- you can change the format

### main

The main example calls a subroutine in the beginning and ending of the program,  
then calls the print subroutine to display the duration between the two times.  
One can use it as a stopwatch by calling the stop subroutine at desired locations.  
By calling the start subroutine, one resets the stopwatch.
