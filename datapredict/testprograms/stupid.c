
int main (int argc, char ** argv) {
    int ret = 0;
    int input1 = atoi(argv[1]); // get user input. 
    int input2 = atoi(argv[2]);

    if (input1 < 10) {
        if( input2 < 0) { 
            input2 = 0;
        }
        return ret;
    }
    if(input1 < 20) {
        if(input2 < 0) {
            ret = 1;
            return ret;  // error because we didn't check for negative
        } else {
            return ret;
        }
    }
    return ret;
}
