/*
   THIS FILE WILL BE AUTOGENERATED
*/
#include "task.h"

void dummy_function(void *arg) {
    int task_number = (int)(intptr_t)arg;
    printf("Thread %ld is executing task %d\n", pthread_self(), task_number);
    sleep(1); // Simulate work
}

int autogen_taskcount() { return 3; }

task_t* autogen_loadtask(int i) {
    task_t* t = malloc(sizeof(task_t));
    t->id = i;
    switch (i) {
        case 1:
        t->function = dummy_function;
        t->data = (void*)(intptr_t)i; /* just an int for now */
        break;
        case 2:
        t->function = dummy_function;
        t->data = (void*)(intptr_t)i; /* just an int for now */
        break;
        case 3:
        t->function = dummy_function;
        t->data = (void*)(intptr_t)i; /* just an int for now */
        break;
    }
    return t;
}