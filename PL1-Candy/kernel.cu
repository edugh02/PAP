
#include "cuda_runtime.h"
#include "device_launch_parameters.h"
#include <curand.h>
#include <stdlib.h>
#include <stdio.h>
#include <curand_kernel.h>
#include <time.h>


/*const int fila;
const int columna;
*/
int vidas=5;



unsigned int generate_seed() {
    time_t t;
    time(&t);
    return (unsigned int)t%100000;
}


__global__ void random_matrix(int* mat, int n, int m, int lim_inf, int lim_sup,unsigned int ale, curandState* state) {
    int idx = threadIdx.x + blockDim.x * blockIdx.x;
    int idy = threadIdx.y + blockDim.y * blockIdx.y;

    if (idx < n && idy < m) {
        curand_init(ale, idx * m + idy, 0, &state[idx * m + idy]); // inicializar el generador de números aleatorios
        int val = curand(&state[idx * m + idy]) % lim_sup + lim_inf; // generar un número aleatorio entero entre 1 y 6
        mat[idx * m + idy] = val;
    }
}

void create_random_matrix(int* mat, int n, int m, int lim_inf, int lim_sup) {
    int* d_mat;
    cudaMalloc(&d_mat, n * m * sizeof(int));

    curandState* d_state;
    cudaMalloc(&d_state, n * m * sizeof(curandState));

    unsigned int ale = generate_seed();

    dim3 block_size(32, 32);
    dim3 num_blocks((n + block_size.x - 1) / block_size.x, (m + block_size.y - 1) / block_size.y);
    random_matrix << <num_blocks, block_size >> > (d_mat, n, m, lim_inf, lim_sup, ale, d_state);

    cudaMemcpy(mat, d_mat, n * m * sizeof(int), cudaMemcpyDeviceToHost);

    cudaFree(d_mat);
    cudaFree(d_state);
}



void imprimir(int* matriz, int n, int m) {
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < m; j++) {
            printf("%d ", matriz[i * m + j]);
        }
        printf("\n");
    }
}

int main()
{   

    /*
    int N = 10;
    int *d_a,*d_b,*d_c,*d_d;
    int matriz_host[N][N];

    printf("Bienvenido a Cundio Crack\n");
    printf("Introduce el modo de juego con el que quieres jugar: \n 1. Automático \n 2. Manual \n");
    int m = scanf("%d", &m);
    printf("Introduce la dificultad con la que quieres jugar: \n 1. Facil \n 2. Normal \n");
    int d = scanf("%d", &d);
    printf("Introduce el numero de filas que quieres que tenga el tablero: \n");
    int fi = scanf("%d", &fi);
    printf("Introduce el numero de columnas que quieres que tenga el tablero: \n");
    int co = scanf("%d", &co);
*/




    // método para crear una matriz aleatoria de n * m 
    int n = 10; // número de filas
    int m = 5; // número de columnas
    int lim_inf = 1; // valor mínimo
    int lim_sup = 6; // valor máximo
    int* mat = (int*)malloc(n * m * sizeof(int)); // matriz aleatoria

    create_random_matrix(mat, n, m, lim_inf, lim_sup);

    imprimir(mat,n,m);

    // hacer algo con la matriz aleatoria

    free(mat);



    return 0;
}