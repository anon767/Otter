#include <sys/utsname.h>
#include <string.h>

int uname(struct utsname *name) {
    int has_uname; 
    __SYMBOLIC(&has_uname);

    if (has_uname) {
        strcpy(name->sysname,"sysname");
        strcpy(name->nodename,"nodename");
        strcpy(name->release,"release");
        strcpy(name->version,"version");
        strcpy(name->machine,"machine");
        strcpy(name->domainname,"domainname");
        return 0;
    }
    else 
        return -1;
}
