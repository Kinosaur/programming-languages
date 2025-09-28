#include <stdio.h>
#include <time.h>

#define N 10000

int main() {
    static int arr[N][N];
    clock_t start, end;
    long long sum = 0;

    // Row-major access
    start = clock();
    for (int i = 0; i < N; i++) {
        for (int j = 0; j < N; j++) {
            sum += arr[i][j];
        }
    }
    end = clock();
    printf("Row-major access time: %f sec\n", (double)(end - start) / CLOCKS_PER_SEC);

    // Column-major access
    start = clock();
    for (int j = 0; j < N; j++) {
        for (int i = 0; i < N; i++) {
            sum += arr[i][j];
        }
    }
    end = clock();
    printf("Column-major access time: %f sec\n", (double)(end - start) / CLOCKS_PER_SEC);

    return 0;
}