/* See http://rfc-ref.org/RFC-TEXTS/959/chapter6.html
start ::= "USER " username "\nPASS " password "\n" cmds

cmds ::=
    "QUIT\n"
  | "REIN\n" start
  | cmd "\n" cmds

cmd ::=
  | no_arg_cmd
  | upload_cmd " " filename <upload file on data connection>
  | "REST " int "\n" appe_or_stor " " filename <upload file on data connection>
  | "REST " int "\nRETR " filename
  | "RNFR " filename "\nRNTO " filename
  | dir_cmd " " dirname
  | file_cmd " " filename
  | "MODE " s_b_c
  | "STRU " f_r_p
  | "TYPE " type_param
  | "PORT " byte "," byte "," byte "," byte "," byte "," byte

no_arg_cmd ::= "NOOP" | "CDUP" | "PWD" | "HELP" | "SYST" | "STAT" | "SITE" | "ABOR" | "LIST" | "NLST" | "PASV"
upload_cmd ::= "APPE" | "STOR" | "STOU"
appe_or_stor ::= "APPE" | "STOR"
dir_cmd ::= "CWD" | "MKD" | "RMD" | "LIST" | "NLST" | "STAT" | "SMNT"
file_cmd ::= "DELE" | "RETR" | "LIST" | "NLST" | "STAT"
s_b_c ::= "S" | "B" | "C"
f_r_p ::= "F" | "R" | "P"
type_param ::= "A N" | "A T" | "A C" | "E N" | "E T" | "E C" | "I" | "L " int

dirname ::= "/" relative_path | relative_path
relative_path ::= filename | filename "/" relative_path

filename ::= alpha | alpha filename
username ::= filename
password ::= filename
*/

#include <stdarg.h>
#include <string.h>

/** Concatenate all of the arguments after the first, and store the result in the first argument. */
void concat(char *dst, const char *string, ...) {
    va_list args;
    va_start(args, string);
    const char *next = string;
    while (next) {
        dst = stpcpy(dst, next);
        next = va_arg(args, const char *);
    }
    va_end(args);
}

const char *generate_string(int length) {
    char *str = malloc(length), *p = str+length-1, c;
    *p = 0;
    while (p-- > str) {
        __SYMBOLIC(&c);
        *p = c;
    }
    return str;
}

const char *generate_byte() {
    char *str = malloc(4);
    unsigned char byte;
    __SYMBOLIC(&byte);
    snprintf(str, 4, "%hhu", byte);
    return str;
}

const char *generate_int() {
    return generate_byte();
}

const char *generate_start(void);
const char *generate_cmds(void);
const char *generate_cmd(void);
const char *generate_no_arg_cmd(void);
const char *generate_upload_cmd(void);
const char *generate_appe_or_stor(void);
const char *generate_dir_cmd(void);
const char *generate_file_cmd(void);
const char *generate_s_b_c(void);
const char *generate_f_r_p(void);
const char *generate_type_param(void);
const char *generate_dirname(void);
const char *generate_relative_path(void);
const char *generate_filename(void);
const char *generate_username(void);
const char *generate_password(void);

const char *generate_start() {
    char *result;
    const char *temp0 = generate_username(), *temp1 = generate_password(), *temp2 = generate_cmds();
    result = malloc(strlen(temp0) + strlen(temp1) + strlen(temp2) + sizeof("USER \nPASS \n"));
    concat(result, "USER ", temp0, "\nPASS ", temp1, "\n", temp2, 0);
    free(temp0);
    free(temp1);
    free(temp2);
    return result;
}

const char *generate_cmds() {
    char *result;
    switch (__SYMBOLIC()) {
    case 0: {
        result = strdup("QUIT\n");
        break;
    }
//    case 1: {
//    char *temp0 = generate_start();
//    result = malloc(strlen(temp0) + sizeof("REIN\n"));
//        concat(result, "REIN\n", temp0, 0);
//        free(temp0);
//        break;
//    }
    default: {
        char *temp0 = generate_cmd(), *temp1 = generate_cmds();
        result = malloc(strlen(temp0) + strlen(temp1) + sizeof("\n"));
        concat(result, temp0, "\n", temp1, 0);
        free(temp0);
        free(temp1);
        break;
    }
    }
    return result;
}

const char *generate_no_arg_cmd() {
    int which_production;
    __SYMBOLIC(&which_production);
    return strdup(which_production == 0 ? "NOOP" :
                  which_production == 1 ? "CDUP" :
                  which_production == 2 ? "PWD " : // Add a space so that it's the same length
                  which_production == 3 ? "HELP" :
                  which_production == 4 ? "SYST" :
                  which_production == 5 ? "STAT" :
                  which_production == 6 ? "SITE" :
                  which_production == 7 ? "ABOR" :
                  which_production == 8 ? "LIST" :
                  which_production == 9 ? "NLST" :
                  "PASV");
}

const char *generate_upload_cmd() {
    int which_production;
    __SYMBOLIC(&which_production);
    return strdup(which_production == 0 ? "APPE" :
                  which_production == 1 ? "STOR" :
                  "STOU");
}

const char *generate_appe_or_stor() {
    return strdup(__SYMBOLIC() ? "APPE" : "STOR");
}

const char *generate_dir_cmd() {
    int which_production;
    __SYMBOLIC(&which_production);
    // Pad commands so that all are length 4
    return strdup(which_production == 0 ? "CWD " :
                  which_production == 1 ? "MKD " :
                  which_production == 2 ? "RMD " :
                  which_production == 3 ? "LIST" :
                  which_production == 4 ? "NLST" :
                  which_production == 5 ? "STAT" :
                  "SMNT");
}

const char *generate_file_cmd() {
    int which_production;
    __SYMBOLIC(&which_production);
    return strdup(which_production == 0 ? "DELE" :
                  which_production == 1 ? "RETR" :
                  which_production == 2 ? "LIST" :
                  which_production == 3 ? "NLST" :
                  "STAT");
}

const char *generate_s_b_c() {
    int which_production;
    __SYMBOLIC(&which_production);
    return strdup(which_production == 0 ? "S" : which_production == 1 ? "B" : "C");
}

const char *generate_f_r_p() {
    int which_production;
    __SYMBOLIC(&which_production);
    return which_production == 0 ? "F" : which_production == 1 ? "R" : "P";
}

const char *generate_type_param() {
    char *result;
    int which_production;
    __SYMBOLIC(&which_production);
    switch (which_production) {
    case 0: {
        char *temp = generate_int();
        result = malloc(strlen(temp) + sizeof("L "));
        concat(result, "L ", temp, 0);
        free(temp);
        break;
    }
    default: {
        result = strdup(which_production == 1 ? "A N" :
                        which_production == 2 ? "A T" :
                        which_production == 3 ? "A C" :
                        which_production == 4 ? "E N" :
                        which_production == 5 ? "E T" :
                        which_production == 6 ? "E C" :
                        "I  "); // Padded to be 3 characters
        break;
    }
    }
    return result;
}

const char *generate_dirname() {
    char *result;
    int choice;
    __SYMBOLIC(&choice);
    switch (choice) {
    case 0: {
        result = generate_relative_path();
        break;
    }
    default: {
        char *temp0 = generate_relative_path();
        result = malloc(strlen(temp0) + sizeof("/"));
        concat(result, "/", temp0, 0);
        free(temp0);
        break;
    }
    }
    return result;
}

const char *generate_relative_path() {
    char *result;
    int choice;
    __SYMBOLIC(&choice);
    switch (choice) {
    case 0: {
        result = generate_filename();
        break;
    }
    default: {
        char *temp0 = generate_filename(), *temp1 = generate_relative_path();
        result = malloc(strlen(temp0) + strlen(temp1) + sizeof("/"));
        concat(result, temp0, "/", temp1, 0);
        free(temp0); free(temp1);
        break;
    }
    }
    return result;
}

const char *generate_cmd() {
    char *result;
    int choice;
    __SYMBOLIC(&choice);
    switch (choice) {
    case 0: {
        result = generate_no_arg_cmd();
        break;
    }
    case 1: {
        char *temp0 = generate_upload_cmd(), *temp1 = generate_filename();
        result = malloc(strlen(temp0) + strlen(temp1) + sizeof(" "));
        concat(result, temp0, " ", temp1, 0);
        free(temp0); free(temp1);
        break;
    }
    case 2: {
        char *temp0 = generate_int(), *temp1 = generate_appe_or_stor(), *temp2 = generate_filename();
        result = malloc(strlen(temp0) + strlen(temp1) + strlen(temp2) + sizeof("REST \n "));
        concat(result, "REST ", temp0, "\n", temp1, " ", temp2, 0);
        free(temp0); free(temp1); free(temp2);
        break;
    }
    case 3: {
        char *temp0 = generate_int(), *temp1 = generate_filename();
        result = malloc(strlen(temp0) + strlen(temp1) + sizeof("REST \nRETR "));
        concat(result, "REST ", temp0, "\nRETR ", temp1, 0);
        free(temp0); free(temp1);
        break;
    }
    case 4: {
        char *temp0 = generate_filename(), *temp1 = generate_filename();
        result = malloc(strlen(temp0) + strlen(temp1) + sizeof("RNFR \nRNTO "));
        concat(result, "RNFR ", temp0, "\nRNTO ", temp1, 0);
        free(temp0); free(temp1);
        break;
    }
    case 5: {
        char *temp0 = generate_dir_cmd(), *temp1 = generate_dirname();
        result = malloc(strlen(temp0) + strlen(temp1) + sizeof(" "));
        concat(result, temp0, " ", temp1, 0);
        free(temp0); free(temp1);
        break;
    }
    case 6: {
        char *temp0 = generate_file_cmd(), *temp1 = generate_filename();
        result = malloc(strlen(temp0) + strlen(temp1) + sizeof(" "));
        concat(result, temp0, " ", temp1, 0);
        free(temp0); free(temp1);
        break;
    }
    case 7: {
        char *temp0 = generate_s_b_c();
        result = malloc(strlen(temp0) + sizeof("MODE "));
        concat(result, "MODE ", temp0, 0);
        free(temp0);
        break;
    }
    case 8: {
        char *temp0 = generate_f_r_p();
        result = malloc(strlen(temp0) + sizeof("STRU "));
        concat(result, "STRU ", temp0, 0);
        free(temp0);
        break;
    }
    case 9: {
        char *temp0 = generate_type_param();
        result = malloc(strlen(temp0) + sizeof("TYPE "));
        concat(result, "TYPE ", temp0, 0);
        free(temp0);
        break;
    }
    default: {
        char *temp0 = generate_byte(), *temp1 = generate_byte(), *temp2 = generate_byte(), *temp3 = generate_byte(), *temp4 = generate_byte(), *temp5 = generate_byte();
        result = malloc(strlen(temp0) + strlen(temp1) + strlen(temp2) + strlen(temp3) + strlen(temp4) + strlen(temp5) + sizeof("PORT ,,,,,"));
        concat(result, "PORT ", temp0, ",", temp1, ",", temp2, ",", temp3, ",", temp4, ",", temp5, 0);
        free(temp0); free(temp1); free(temp2); free(temp3); free(temp4); free(temp5);
        break;
    }
    }
    return result;
}

const char *generate_username() {
    return generate_string(3);
}

const char *generate_filename() {
    return generate_string(2);
}

const char *generate_password() {
    return generate_string(2);
}

#include <netinet/in.h>

int client_main()
{
    int fd = socket(AF_INET, SOCK_STREAM, 0);
    __ASSERT(fd >= 0);
    struct sockaddr_in* addr = calloc(sizeof(struct sockaddr_in), 1);
    addr->sin_family = AF_INET;
    addr->sin_port = htons(21);
	addr->sin_addr.s_addr = htonl(0x7F000001);
    int r = connect(fd, addr, sizeof(struct sockaddr_in));
    __ASSERT(r != -1);
    char *out_buf = generate_start();
    __EVALSTR(out_buf, 1000);
    int length = strlen(out_buf); // This causes forking based on the length. Is there a way to avoid that?
    char in_buf[20];
    for(int i = 0; i < 5; i++)
    {
        write(fd, out_buf, length);
        read(fd, in_buf, 20);
    }
    
    return(0);
}
