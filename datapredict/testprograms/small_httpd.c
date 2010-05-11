#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h> 

int content_length;
FILE * input_file;

char * ReadPOSTData() {
    char * buff = calloc(content_length+1, sizeof(char));
    int rc = read(input_file, buff, content_length);
    buff[content_length]='\0';
    return buff;
}

char * ReadCGIData() {
    char * buff;
    int rc;

    if(content_length > 0) {
        buff = calloc(content_length+1, sizeof(char));
        rc = read(input_file, buff, content_length);
        buff[content_length]='\0';
    } else {
        buff = 0;
    }
    return buff;
}

char * ProcessGETRequest() {
    char * buff;
    int i=0;

    if(content_length > 0) {
        content_length = 0;
    }
    buff = calloc(content_length + 1, sizeof(char));
    while(i < content_length) {
        buff[i] = 'a';
        i++;
    }
    buff[content_length] = '\0';
    write(input_file,buff,content_length);
    return buff;
}
 
int main(int argc, char *argv[]) {
    int content_length;
    char * file_name;
    char * line;
    char * request_method;

    file_name = argv[1];
    input_file = fopen(file_name, "r+");
    
    if(!input_file) {
        printf("Error opening input file for reading\n");
        return 1;
    }

    while (line = fgets(line, 100, input_file)) {
        if (strncasecmp(line, "Request-Type:", 13)) {
            strncpy(request_method, line, line+12);
        }
        if (strncasecmp(line, "Content-Length:", 15)) {
            content_length = atoi(line+16);
        }
    }
    if (request_method == "GET") {
        line = ProcessGETRequest();
    } else if (request_method == "POST") {
        line = ReadPOSTData();
    } else if (request_method == "CGI") {
        line = ReadCGIData();
    }    
    fclose(input_file);
    printf("We have succeeded!\n");
    return 0;
}
