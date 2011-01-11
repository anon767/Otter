int 
strcasecmp(const char *s1, const char *s2)
{
    int r = 0;

    while ( ((*s1 == *s2) || 
             !(r = ((int)( tolower(*((char *)s1)))) - tolower(*((char *)s2))))
            && (++s2, *s1++));
    
    return r;
}
