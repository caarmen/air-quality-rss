#include <stdio.h>
#include <pthread.h>
#include <stdlib.h>
#include <unistd.h>

// Calls the given callback function after a random delay.
void* call_me_back_thread(
    void * arg
) {
    void (*callback)() = arg;
    usleep(rand() % 10000);
    callback();
    return NULL;
}

// Calls the given no-args void callback functions, from another thread, after a random delay.
int call_me_back(
    void (*callback1)(),
    void (*callback2)()
) {
    pthread_t thread_id1;
    pthread_t thread_id2;

    pthread_create(&thread_id1, NULL, call_me_back_thread, callback1);
    pthread_create(&thread_id2, NULL, call_me_back_thread, callback2);

    pthread_join(thread_id1, NULL);
    pthread_join(thread_id2, NULL);

    return 0;
}
