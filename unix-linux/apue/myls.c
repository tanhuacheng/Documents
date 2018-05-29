#include <stdio.h>
#include <stdlib.h>
#include <dirent.h>

int main(int argc, char *argv[])
{
    if (argc != 2) {
        printf("Usage: ls directory_name\n");
        exit(1);
    }

    DIR *dp;
    struct dirent *dirp;

    if ((dp = opendir(argv[1])) == NULL) {
        perror("opendir");
        exit(1);
    }

    while ((dirp = readdir(dp)) != NULL) {
        printf("%s\n", dirp->d_name);
    }

    closedir(dp);

    return 0;
}
