#include <sys/types.h>
#include <dirent.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#define BUF_SIZE 1024
void getPidByName(char* task_name)
{
    DIR *dir;
    struct dirent *ptr;
    FILE *fp;
    char filepath[50];//大小随意，能装下cmdline文件的路径即可
    char cur_task_name[50];//大小随意，能装下要识别的命令行文本即可
    char buf[BUF_SIZE];
    dir = opendir("/proc"); //打开路径
    if (NULL != dir)
    {
        while ((ptr = readdir(dir)) != NULL) //循环读取路径下的每一个文件/文件夹
        {
            //如果读取到的是"."或者".."则跳过，读取到的不是文件夹名字也跳过
            if ((strcmp(ptr->d_name, ".") == 0) || (strcmp(ptr->d_name, "..") == 0))
continue;
            if (DT_DIR != ptr->d_type)
         continue;
 
            sprintf(filepath, "/proc/%s/status", ptr->d_name);//生成要读取的文件的路径
            fp = fopen(filepath, "r");//打开文件
            if (NULL != fp)
            {
                if( fgets(buf, BUF_SIZE-1, fp)== NULL ){
            fclose(fp);
            continue;
        }
        sscanf(buf, "%*s %s", cur_task_name);
 
                //如果文件内容满足要求则打印路径的名字（即进程的PID）
                if (!strcmp(task_name, cur_task_name))
            printf("PID:  %s\n", ptr->d_name);
                fclose(fp);
            }
 
        }
        closedir(dir);//关闭路径
    }
}
void getNameByPid(pid_t pid, char *task_name) {
    char proc_pid_path[BUF_SIZE];
    char buf[BUF_SIZE];
    sprintf(proc_pid_path, "/proc/%d/status", pid);
    FILE* fp = fopen(proc_pid_path, "r");
    if(NULL != fp){
        if( fgets(buf, BUF_SIZE-1, fp)== NULL ){
            fclose(fp);
        }
        fclose(fp);
        sscanf(buf, "%*s %s", task_name);
    }
}
void main(int argc, char** argv)
{
    char task_name[50];
    pid_t pid = getpid();
    printf("pid of this process:%d\n", pid);
    getNameByPid(pid, task_name);
 
    /*
    strcpy(task_name, argv[0]+2);
    printf("task name is %s\n", task_name);
    getPidByName(task_name);
    */
    printf("task name is %s\n", task_name);
    getPidByName(task_name);
    sleep(15);
}

