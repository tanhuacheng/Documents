#include <stdio.h>
#include <string.h>
#include <regex.h>

int main (int argc, char* argv[])
{
    (void)argc;
    (void)argv;

    int cflags = REG_EXTENDED;
    const char* pattern = "(\\w+):\\/\\/([^:\\/]+):?([0-9]*)([^# ]*)";
    char* buf = "http://www.runoob.com:80/html/html-tutorial.html "
                "https://www.sunoob.com:81/itml/html-tutorial.html";

    regex_t reg;
    int status = regcomp(&reg, pattern, cflags);
    if (status) {
        char errstr[128];
        int el = regerror(status, &reg, errstr, 128);
        printf("error: %d: %.*s\n", status, el, errstr);
    }

    int nmatch = reg.re_nsub;
    printf("re_nsub: %ld\n\n", reg.re_nsub);

    size_t offset = 0;
    while (offset < strlen(buf)) {
        regmatch_t match[nmatch + 1];
        int status = regexec(&reg, buf + offset, nmatch + 1, match, 0);
        if (status == REG_NOMATCH) {
            printf("No Match\n");
            break;
        } else if (status != 0) {
            char errstr[128];
            int el = regerror(status, &reg, errstr, 128);
            printf("error: %d: %.*s\n", status, el, errstr);
        }

        printf("Match:%d,%d\n", match[0].rm_so, match[0].rm_eo);
        for (int i = match[0].rm_so; i < match[0].rm_eo; i++) {
            printf("%c", buf[i + offset]);
        }
        printf("\n");

        for (int n = 1; n <= nmatch; n++) {
            for (int i = match[n].rm_so; i < match[n].rm_eo; i++) {
                printf("%c", buf[i + offset]);
            }
            printf("\n");
        }

        offset += match[0].rm_eo;
        printf("\n");
    }
    regfree(&reg);

    return 0;
}

