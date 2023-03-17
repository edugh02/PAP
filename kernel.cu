
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
int vidas = 5;



// Esta función genera una semilla aleatoria basada en la hora actual.
unsigned int generate_seed() {
    time_t t;
    time(&t);
    return (unsigned int)t % 100000;
}

__global__ void mover_candy(int* mat, int n, int m, int dir,int cordx,int cordy) {
    int col = threadIdx.x + blockDim.x * blockIdx.x;
    int fila = threadIdx.y + blockDim.y * blockIdx.y;
    int posicion = fila * n + col;
    int caramelo = cordy * n + cordx;
    if (posicion == caramelo) {
        switch (dir) {
        case 1://Arriba
            if (cordy != 0) {
                int temp=mat[caramelo];
                mat[caramelo] = mat[caramelo - n];
                mat[caramelo - n] = temp;
            }
            break;
        case 2://Abajo
            if (cordy != m) {
                int temp = mat[caramelo];
                mat[caramelo] = mat[caramelo + n];
                mat[caramelo + n] = temp;
            }
            break;
        case 3://Derecha
            if (cordy != n) {
                int temp = mat[caramelo];
                mat[caramelo] = mat[caramelo + 1];
                mat[caramelo + 1] = temp;
            }
            break;
        case 4://Izquierda
            if (cordy != 0) {
                int temp = mat[caramelo];
                mat[caramelo] = mat[caramelo - 1];
                mat[caramelo - 1] = temp;
            }
            break;
        default:
            break;
        }
    }
    
}

// Esta función genera una matriz aleatoria de números enteros entre "lim_inf" y "lim_sup".
__global__ void random_matrix(int* mat, int n, int m, int lim_inf, int lim_sup, unsigned int ale, curandState* state) {
    // Calcular las coordenadas x e y del hilo
    int idx = threadIdx.x + blockDim.x * blockIdx.x;
    int idy = threadIdx.y + blockDim.y * blockIdx.y;

    // Verificar si el hilo se encuentra dentro de los límites de la matriz
    if (idx < n && idy < m) {
        // Inicializar el generador de números aleatorios
        curand_init(ale, idx * m + idy, 0, &state[idx * m + idy]);
        // Generar un número aleatorio entero entre "lim_inf" y "lim_sup"
        int val = curand(&state[idx * m + idy]) % lim_sup + lim_inf;
        // Asignar el valor aleatorio a la matriz
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


int generarNumAleatorio(int hasta) {
    
    
    srand(generate_seed());
    int ale = rand() % (hasta + 1);
    return ale;
}

int main()
{

    int modo; //automático o manual
    int dificultad; //dificultad del juego
    int n; // número de filas
    int m; // número de columnas
    printf("Bienvenido a Cundio Crack\n");
    printf("Introduce el modo de juego con el que quieres jugar: \n 1. Automatico \n 2. Manual \n");
    scanf("%d", &modo);
    printf("Introduce la dificultad con la que quieres jugar: \n 1. Facil \n 2. Normal \n");
    scanf("%d", &dificultad);
    printf("Introduce el numero de filas que quieres que tenga el tablero: \n");
    scanf("%d", &n);
    printf("Introduce el numero de columnas que quieres que tenga el tablero: \n");
    scanf("%d", &m);
    int lim_inf = 1; // valor mínimo
    int lim_sup = 6; // valor máximo
    if (dificultad == 1) {
         lim_sup = 4; // valor máximo
    }
    
    int* mat = (int*)malloc(n * m * sizeof(int)); // matriz aleatoria

    create_random_matrix(mat, n, m, lim_inf, lim_sup);


    //AQUI                                                                                  //<--ESTO HAY QUE VER COMO HACERLO EFICIENTE
    dim3 dimBlock(n,m);
    dim3 dimGrid(1);

    int cordx=-1;
    int cordy=-1;
    int dir=-1;

    while (vidas > 0) {
        imprimir(mat, n, m);
        do {
            if (modo == 2) {
                printf("Introduce la cordenada X del caramelo que quieres mover\n");
                scanf("%d", &cordx);
                printf("Introduce la cordenada Y del caramelo que quieres mover\n");
                scanf("%d", &cordy);
                printf("Introduce la dirección hacia la que quieres mover el caramelo deseado: \n 1 Arriba\n 2 Abajo\n 3 Derecha \n 4 Izquierda\n");
                scanf("%d", &dir);

            }
            else {                                                              //ESTA MIERDA ESTA MAL, REVISAR A VER COMO SERÍA
                srand(time(NULL));
                cordx = generarNumAleatorio(n);
                srand(time(NULL));
                cordy = generarNumAleatorio(m);
                srand(time(NULL));
                dir = generarNumAleatorio(4) + 1;
                printf("%d\n", cordx);
                printf("%d\n", cordy);
                printf("%d\n\n", dir);
            }
        } while (cordx > n && cordy > m && cordx < 0 && cordy < 0 && 0 > dir && dir > 5);

        int* d_mat;
        cudaMalloc(&d_mat, n * m * sizeof(int));
        cudaMemcpy(d_mat, mat, n * m * sizeof(int), cudaMemcpyHostToDevice);

        mover_candy << <dimGrid, dimBlock >> > (d_mat, n, m, dir, cordx, cordy);

        cudaMemcpy(mat, d_mat, n * m * sizeof(int), cudaMemcpyDeviceToHost);
        cudaFree(d_mat);

        vidas -= 1;
    }

   

    free(mat);

    return 0;
}