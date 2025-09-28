#include <stdio.h>
#include <pthread.h>
#include <stdlib.h>
#include <unistd.h>

// Calls the given callback function after a random delay.
void* call_me_back_thread(
    void * arg
) {
    // extract the callback function pointer
    void (*callback)() = arg;
    //printf("Hello from the thread!\n");
    usleep(rand() % 10000);
    callback();
    return NULL;
}

// Calls the given no-args void callback function, from another thread, after a random delay.
int call_me_back(
    void (*callback1)(),
    void (*callback2)()
) {
    int result;
    pthread_t thread_id1;
    pthread_attr_t attr1;
    pthread_t thread_id2;
    pthread_attr_t attr2;

    pthread_attr_init(&attr1);
    pthread_attr_init(&attr2);
    //pthread_attr_setdetachstate(&attr1, PTHREAD_CREATE_DETACHED);
    //pthread_attr_setdetachstate(&attr2, PTHREAD_CREATE_DETACHED);

    result = pthread_create(&thread_id1, &attr1, call_me_back_thread, callback1);
    result = pthread_create(&thread_id2, &attr2, call_me_back_thread, callback2);
    result = pthread_join(thread_id2, NULL);
    result = pthread_join(thread_id1, NULL);

    pthread_attr_destroy(&attr1);
    pthread_attr_destroy(&attr2);
    return 0;
}
