#include <stdlib.h>
#include <string.h>

#define MAX_VAR  8
int var_count = 0;
char *var_names[MAX_VAR];
char *var_values[MAX_VAR];

char *getenv(const char *name) {
        int i = 0;
        for (i = 0; i < var_count; i++) {
                if (strcmp(name, var_names[i]) == 0)
                        return var_values[i];
        }
        return NULL;
}

int putenv(char* str) {
        char *name_end = strchr(str, '=');
        int found = -1;

        int i = 0;
        for (i = 0; i < var_count; i++) {
                if (!strncmp(var_names[i], str, name_end - str)) {
                        found = i;
                }
        }

        if (found >= 0)
        {
                free(var_values[found]);
                var_values[found] = strdup(name_end + 1);
        }
        else
        {
                var_names[var_count] = malloc(sizeof(char) * (name_end - str + 1));
                strncpy(var_names[var_count], str, name_end - str);
                var_names[var_count][name_end - str] = '\0';

                var_values[var_count] = strdup(name_end + 1);

                var_count++;
        }

        return 0;
}

