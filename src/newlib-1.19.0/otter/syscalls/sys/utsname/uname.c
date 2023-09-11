#include <sys/utsname.h>
#include <string.h>

int uname(struct utsname *name) {
    static int sys_has_uname; 

    // Make sure sys_has_uname is initialized only once
    static int sys_has_uname_set = 0;
    if (!sys_has_uname_set) {
        sys_has_uname_set = 1;
        __SYMBOLIC(&sys_has_uname);
    }

    if (sys_has_uname) {
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
