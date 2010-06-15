
int main (int argc, char ** argv) {
A    int ret = 0;
B    int input1 = atoi(argv[1]); // get user input. 
C    int input2 = atoi(argv[2]);

D    if (input1 < 10) {
E        if( input2 < 0) { 
G            input2 = 0;
        }
H        return ret;
    }
F    if(input1 < 20) {
I        if(input2 < 0) {
L            ret = 1;
            return ret;  // error because we didn't check for negative
        } else {
J            return ret;
        }
    }
M    return ret;
}
